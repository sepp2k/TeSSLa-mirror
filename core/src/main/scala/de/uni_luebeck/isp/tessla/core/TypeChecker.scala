/*
 * Copyright 2022 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp
import de.uni_luebeck.isp.tessla
import de.uni_luebeck.isp.tessla.core
import de.uni_luebeck.isp.tessla.core.Errors.*
import de.uni_luebeck.isp.tessla.core.FlatTessla.AnnotationEntry
import de.uni_luebeck.isp.tessla.core.util.*

import scala.annotation.tailrec
import scala.collection.mutable

object TypeChecker extends TranslationPhase[FlatTessla.Specification, TypedTessla.TypedSpecification] {
  override def translate(spec: FlatTessla.Specification): TranslationPhase.Result[TypedTessla.TypedSpecification] = {
    new TypeChecker(spec).translate()
  }
}

/**
 * The type checker:
 *  - Annotates every expression with its type
 *  - Checks for type errors
 *  - Converts constant value expressions to `default(nil, value)`-streams where needed
 *  - Converts primitive n-ary operator applications on streams `a + b` with an n-ary signal lift `slift(a, b, +)`
 */

class TypeChecker(spec: FlatTessla.Specification)
    extends TypedTessla.IdentifierFactory
    with TranslationPhase.Translator[TypedTessla.TypedSpecification] {

  private val typeMap = mutable.Map[TypedTessla.Identifier, TypedTessla.Type]()
  private val resolvedMemberAccesses = mutable.HashMap[(FlatTessla.Identifier, String), FlatTessla.Identifier]()

  type Env = Map[FlatTessla.Identifier, TypedTessla.Identifier]

  override def translateSpec(): TypedTessla.TypedSpecification = {
    val (defs, env) = translateDefsWithParents(spec.globalDefs)
    var outputStreams = spec.outStreams.flatMap(translateOutStream(_, defs, env))
    if (spec.hasOutAll) {
      val annotations = spec.outAll.get.annotations.map(translateAnnotation(_, defs, spec.globalDefs, env))
      val streams = defs.variables.values.filter(entry => entry.typeInfo.isStreamType)
      outputStreams ++= streams.flatMap { entry =>
        entry.id.nameOpt.map(name => TypedTessla.OutStream(entry.id, name, annotations, entry.loc))
      }
    }
    val globalAnnotations = spec.annotations.map(translateAnnotation(_, defs, spec.globalDefs, env))
    TypedTessla.TypedSpecification(globalAnnotations, defs, outputStreams, spec.outAll.map(_.loc))
  }

  def translateOutStream(
    stream: FlatTessla.OutStream,
    defs: TypedTessla.Definitions,
    env: Env
  ): List[TypedTessla.OutStream] = {
    val annotations = stream.annotations.map(translateAnnotation(_, defs, spec.globalDefs, env))
    val id = env(stream.id)

    def translateOutStreamInner(id: TypedTessla.Identifier, typ: TypedTessla.Type, name: String) = typ match {
      case b: TypedTessla.BuiltInType if b.isStreamType =>
        TypedTessla.OutStream(id, name, annotations, stream.loc)
      case t if t.isValueType =>
        TypedTessla.OutStream(
          liftConstant(id, defs, env, stream.loc),
          name,
          annotations,
          stream.loc
        )
      case other =>
        error(TypeMismatch(expected = "stream or value type", other, stream.loc))
        TypedTessla.OutStream(id, "<error>", annotations, stream.loc)
    }

    typeMap(id) match {
      case t @ TypedTessla.ObjectType(memberTypes) if !t.isValueType =>
        val memberAccesses = mutable.HashMap[List[String], TypedTessla.Identifier]()

        type Path = List[(String, TypedTessla.Type)]

        // Recursively collect members to generate output streams for
        def collectMembers(path: Path, members: Map[String, TypedTessla.Type]): Set[Path] = {
          val hasStreamMember = members.exists {
            case (name, t: TypedTessla.ObjectType) => !t.isValueType
            case (_, typ)                          => typ.isStreamType
          }

          if (hasStreamMember) {
            members.flatMap {
              case (name, typ: TypedTessla.ObjectType) if !typ.isValueType =>
                collectMembers(path :+ (name, typ), typ.memberTypes)
              case (name, typ) => List(path :+ (name, typ))
            }.toSet
          } else Set()
        }

        val members = collectMembers(Nil, memberTypes)
        members.map { path =>
          val finalId = path
            .foldLeft((id, List[String]())) {
              case ((previousId, accpath), (member, memberType)) =>
                val memberAccess = memberAccesses.getOrElseUpdate(
                  accpath :+ member, {
                    val maId = makeIdentifier()
                    val loc = stream.loc
                    val ma = TypedTessla.MemberAccess(TypedTessla.IdLoc(previousId, loc), member, loc, loc)
                    defs.addVariable(TypedTessla.VariableEntry(maId, ma, memberType, Seq(), false, loc))
                    typeMap.put(maId, memberType)
                    maId
                  }
                )
                (memberAccess, accpath :+ member)
            }
            ._1
          val typ = path.last._2
          val name = s"${stream.name}.${path.map(_._1).mkString(".")}"
          translateOutStreamInner(finalId, typ, name)
        }.toList
      case other => translateOutStreamInner(id, other, stream.name) :: Nil
    }
  }

  def processTypeAnnotation(entry: FlatTessla.VariableEntry, env: Env): Unit = entry.typeInfo.foreach { typ =>
    typeMap(env(entry.id)) = translateType(typ, env)
  }

  /**
   * Translate the provided type by resolving type parameters using the environment.
   *
   * @param typ the type to translate
   * @param env the environment
   * @return the resulting type
   */
  def translateType(typ: FlatTessla.Type, env: Env): TypedTessla.Type = typ match {
    case b: FlatTessla.BuiltInType =>
      TypedTessla.BuiltInType(b.name, b.typeArgs.map(translateType(_, env)))
    case f: FlatTessla.FunctionType =>
      val typeParams = f.typeParameters.map(tvar => makeIdentifier(tvar.nameOpt))
      val innerEnv = env ++ f.typeParameters.zip(typeParams).toMap
      val paramTypes = f.parameterTypes.map(p => p._1 -> translateType(p._2, innerEnv))
      TypedTessla.FunctionType(
        typeParams,
        paramTypes,
        translateType(f.returnType, innerEnv),
        f.isLiftable
      )
    case o: FlatTessla.ObjectType =>
      TypedTessla.ObjectType(mapValues(o.memberTypes)(translateType(_, env)))
    case tvar: FlatTessla.TypeParameter =>
      val newTvar = env.getOrElse(
        tvar.id,
        throw InternalError(s"Failed to look up type variable $tvar", tvar.loc)
      )
      TypedTessla.TypeParameter(newTvar, tvar.loc)
  }

  /**
   * Insert an inferred type into the type map.
   *
   * If the map already contains an entry with this id and the inferred type is not a subtype of the existing entry,
   * a type mismatch error is produced.
   *
   * @param id the identifier the type was inferred for
   * @param inferredType the inferred type
   * @param loc the location
   */
  def insertInferredType(
    id: TypedTessla.Identifier,
    inferredType: TypedTessla.Type,
    loc: Location
  ): Unit = {
    typeMap.get(id) match {
      case None =>
        typeMap(id) = inferredType
      case Some(declaredType) =>
        if (!isSubtypeOrEqual(parent = declaredType, child = inferredType)) {
          error(TypeMismatch(declaredType, inferredType, loc))
        }
    }
  }

  def translateDefsWithParents(defs: FlatTessla.Definitions): (TypedTessla.Definitions, Env) = {
    defs.parent.map(translateDefsWithParents) match {
      case Some((parentDefs, parentEnv)) =>
        translateDefs(defs, Some(parentDefs), parentEnv)
      case None =>
        translateDefs(defs, None, Map())
    }
  }

  def parameterTypes(
    mac: FlatTessla.Macro
  ): Seq[(Option[TesslaAST.RuntimeEvaluation], FlatTessla.Type)] = {
    mac.parameters.map(p => p._1 -> p._2.parameterType)
  }

  def isLiftableMacro(exp: FlatTessla.Expression): Boolean = exp match {
    case m: FlatTessla.Macro => m.isLiftable
    case _                   => false
  }

  def isBuiltIn(exp: FlatTessla.Expression): Boolean = exp.isInstanceOf[FlatTessla.Extern]

  /**
   * Return all the entries that need to be type inferred before the current entry, i.e.
   * all the entries that are used by this entry and do not have an explicit type annotation.
   *
   * @param defs the scope of definitions
   * @param entry the entry to infer the type of
   */
  def requiredEntries(
    defs: FlatTessla.Definitions,
    entry: FlatTessla.VariableEntry
  ): Seq[FlatTessla.VariableEntry] = {
    def resolve(id: FlatTessla.Identifier): List[FlatTessla.VariableEntry] = {
      defs.resolveVariable(id).toList.filter { arg =>
        arg.typeInfo.isEmpty || isBuiltIn(arg.expression)
      }
    }
    entry.annotations.flatMap(_.arguments.map(_.id)).flatMap(resolve) ++
      (entry.expression match {
        case v: FlatTessla.Variable =>
          resolve(v.id)
        case _: FlatTessla.Literal | _: FlatTessla.InputStream | _: FlatTessla.Parameter =>
          Seq()

        case builtIn: FlatTessla.Extern =>
          builtIn.referenceImplementation.toSeq.flatMap(resolve)

        case call: FlatTessla.MacroCall =>
          // Since we invoke requiredEntries* with an outer defs in the macro case (see below), we might encounter
          // identifiers that aren't defined in the defs we see, so we use flatMap to discard the Nones.
          val args = call.args.flatMap(arg => resolve(arg.id))
          resolve(call.macroID) ++ args

        case mac: FlatTessla.Macro =>
          // Since identifiers used in the macro may either be defined inside or outside the
          // macro (and we only want the outside ones), we need to filter them out afterwards.
          // We still need to use the inner scope to find the required entries, so that the
          // dependencies of inner definitions are still considered.
          // Note that identifiers are unique at this stage, so we won't run into a situation
          // where the macro contains a local identifier that shadows an outer one.
          resolve(mac.result.id) ++ mac.body.variables.values
            .flatMap(requiredEntries(mac.body, _))
            .filter { definition =>
              defs.resolveVariable(definition.id).isDefined
            }

        case obj: FlatTessla.ObjectLiteral =>
          obj.members.values.flatMap(member => resolve(member.id)).toSeq

        case acc: FlatTessla.MemberAccess =>
          @tailrec
          def resolveMA(id: FlatTessla.Identifier, accs: List[String]): List[FlatTessla.VariableEntry] = {
            defs.resolveVariable(id) match {
              case None => Nil
              case Some(arg) =>
                val required = arg.typeInfo.isEmpty || isLiftableMacro(arg.expression) || isBuiltIn(arg.expression)

                arg.expression match {
                  case obj: FlatTessla.ObjectLiteral if accs.size == 1 && obj.members.contains(accs.head) =>
                    resolvedMemberAccesses.update((acc.receiver.id, acc.member), obj.members(accs.head).id)
                  case _ if accs.isEmpty =>
                    resolvedMemberAccesses.update((acc.receiver.id, acc.member), arg.id)
                  case _ => resolvedMemberAccesses.remove((acc.receiver.id, acc.member))
                }

                if (required) {
                  arg.expression match {
                    case obj: FlatTessla.ObjectLiteral if accs.nonEmpty && obj.members.contains(accs.head) =>
                      resolveMA(obj.members(accs.head).id, accs.tail)
                    case acc: FlatTessla.MemberAccess =>
                      resolveMA(acc.receiver.id, acc.member +: accs)
                    case _ if accs.isEmpty => List(arg)
                    case _                 => resolve(acc.receiver.id)
                  }
                } else {
                  Nil
                }
            }
          }

          resolveMA(acc.receiver.id, acc.member :: Nil)
      })
  }

  /**
   * Translate an instance of [[FlatTessla.Definitions]].
   *
   * This generates a local environment, and processes all definition entries.
   *
   * @param defs the definitions to translate
   * @param parent the parent definitions, will be the parent of the resulting translated definitions
   * @param parentEnv the current environment
   * @return the translated definitions and the new environment
   */
  def translateDefs(
    defs: FlatTessla.Definitions,
    parent: Option[TypedTessla.Definitions],
    parentEnv: Env
  ): (TypedTessla.Definitions, Env) = {
    val env = parentEnv ++
      mapValues(defs.variables)(entry => makeIdentifier(entry.id.nameOpt)) ++
      mapValues(defs.annotations)(entry => makeIdentifier(entry.id.nameOpt))
    val resultingDefs = new TypedTessla.Definitions(parent)
    defs.variables.values.foreach(processTypeAnnotation(_, env))

    ReverseTopologicalSort.sort(defs.variables.values)(requiredEntries(defs, _)) match {
      case ReverseTopologicalSort.Cycles(nodesInCycles) =>
        nodesInCycles.foreach { entry =>
          entry.id.nameOpt.foreach(name => error(MissingTypeAnnotationRec(name, entry.loc)))
        }
        abort()
      case ReverseTopologicalSort.Sorted(sorted) =>
        sorted.filter(defs.variables.values.toSeq.contains(_)).foreach { entry =>
          resultingDefs.addVariable(translateEntry(entry, resultingDefs, defs, env))
        }
    }

    defs.annotations.foreach {
      case (id, FlatTessla.AnnotationEntry(_, parameters, global, loc)) =>
        resultingDefs.addAnnotation(TypedTessla.AnnotationEntry(env(id), parameters, global, loc))
    }
    (resultingDefs, env)
  }

  def translateEntry(
    entry: FlatTessla.VariableEntry,
    defs: TypedTessla.Definitions,
    flatDefs: FlatTessla.Definitions,
    env: Env
  ): TypedTessla.VariableEntry = {
    val id = env(entry.id)
    val (exp, typ) = translateExpression(entry.expression, typeMap.get(id), defs, flatDefs, env)
    insertInferredType(id, typ, exp.loc)
    val annotations = entry.annotations.map(translateAnnotation(_, defs, flatDefs, env))
    TypedTessla.VariableEntry(id, exp, typ, annotations, entry.parens, entry.loc)
  }

  def translateAnnotation(
    annotation: FlatTessla.Annotation,
    defs: TypedTessla.Definitions,
    flatDefs: FlatTessla.Definitions,
    env: Env
  ): TypedTessla.Annotation = {
    val id = env(annotation.id)
    val annotationDef = flatDefs.resolveAnnotation(annotation.id).get

    val args = resolveNamedArgs(annotation.arguments, annotationDef.parameters, annotation.loc)
    val parameterTypes = annotationDef.parameters.map(_.parameterType)

    if (args.size != parameterTypes.size) {
      error(ArityMismatch(annotationDef.id.nameOpt.get, parameterTypes.size, args.size, annotation.loc))
    }
    TypedTessla.Annotation(
      id,
      args.zip(parameterTypes).map {
        case ((arg, loc), expected) =>
          val id = env(arg.id)
          val exp = translateType(expected, env)
          val act = typeMap(id)
          if (!isSubtypeOrEqual(act, exp)) {
            error(TypeMismatch(exp, act, loc))
          }
          TypedTessla.PositionalArgument(id, loc)
      },
      annotation.loc
    )
  }

  private def condense(substitutions: mutable.Map[TypedTessla.Identifier, TypedTessla.Type]): Unit = {
    def substitute(t: TypedTessla.Type): TypedTessla.Type = t match {
      case ext: TypedTessla.BuiltInType => ext.copy(typeArgs = ext.typeArgs.map(substitute))
      case o: TypedTessla.ObjectType    => o.copy(memberTypes = mapValues(o.memberTypes)(substitute))
      case f: TypedTessla.FunctionType =>
        val parameterTypes = f.parameterTypes.map { case (eval, p) => eval -> substitute(p) }
        val returnType = substitute(f.returnType)
        TypedTessla.FunctionType(Seq(), parameterTypes, returnType, f.isLiftable)
      case TypedTessla.TypeParameter(id, _) => substitutions.getOrElse(id, t)
    }
    substitutions.keySet.foreach(k => substitutions.update(k, substitute(substitutions(k))))
  }

  /**
   * Perform type substitution, using an expected and an actual type, as well known substitutions.
   *
   * Known type parameters are substituted, and the substitutions are potentially expanded by new entries which
   * could be deduced from the given actual and expected types.
   *
   * @param expected the expected type
   * @param actual the actual type
   * @param substitutions the currently known substitutions
   * @return the substituted actual type
   */
  def typeSubst(
    expected: TypedTessla.Type,
    actual: TypedTessla.Type,
    substitutions: mutable.Map[TypedTessla.Identifier, TypedTessla.Type]
  ): TypedTessla.Type = {
    condense(substitutions)
    (expected, actual) match {
      case (expt: TypedTessla.TypeParameter, act: TypedTessla.TypeParameter) =>
        substitutions.get(expt.id) match {
          case Some(t) if expt != t =>
            if (act != t) substitutions.update(act.id, t)
            typeSubst(t, t, substitutions)
          case None if expt != act =>
            substitutions.update(expt.id, act)
            typeSubst(act, act, substitutions)
          case Some(t) => t
          case _ =>
            substitutions.update(expt.id, act)
            act
        }

      case (tparam: TypedTessla.TypeParameter, _) =>
        if (!actual.isValueType) {
          // Ugly hack: By returning an unexpanded type variable named "value type", the error message will say
          // "Expected value type"
          TypedTessla.TypeParameter(makeIdentifier("value type"), tparam.loc)
        } else {
          val t = substitutions.getOrElseUpdate(tparam.id, actual)
          typeSubst(t, t, substitutions)
        }
      case (expectedFunctionType: TypedTessla.FunctionType, actualFunctionType: TypedTessla.FunctionType) =>
        // Since we don't support higher-order types, i.e. function types that appear as a subtype can't be generic
        // themselves, we know that none of the function types, except the initial one that was used to generated the
        // type environment, will have type parameters, so we don't need to update the type environment with new type
        // variables.

        // Pad actual type parameters to have at least the size of the expected type parameters
        // to prevent the type substitution from dropping parameters
        val expT = expectedFunctionType.parameterTypes
        val actualT = actualFunctionType.parameterTypes
        val actualTPadded = actualT ++ expT.drop(actualT.length)
        val parameterTypes =
          expT.zip(actualTPadded).map {
            case ((_, expectedParamType), (eval, actualParamType)) =>
              eval -> typeSubst(expectedParamType, actualParamType, substitutions)
          }

        val returnType = typeSubst(
          expectedFunctionType.returnType,
          actualFunctionType.returnType,
          substitutions
        )

        TypedTessla.FunctionType(Seq(), parameterTypes, returnType, expectedFunctionType.isLiftable)
      case (expectedType: TypedTessla.BuiltInType, actualType: TypedTessla.BuiltInType)
          if expectedType.name == actualType.name =>
        val typeArgs = expectedType.typeArgs.zip(actualType.typeArgs).map {
          case (expectedElementType, actualElementType) =>
            typeSubst(expectedElementType, actualElementType, substitutions)
        }
        TypedTessla.BuiltInType(expectedType.name, typeArgs)
      // Allow for auto-lifting of values
      case (b: TypedTessla.BuiltInType, actualElementType) if b.isStreamType =>
        val expectedElementType = b.typeArgs.head
        TypedTessla.BuiltInType(
          b.name,
          typeSubst(
            expectedElementType,
            actualElementType,
            substitutions
          ) +: b.typeArgs.tail
        )
      case (expected: TypedTessla.ObjectType, actual: TypedTessla.ObjectType) =>
        val members = expected.memberTypes.map {
          case (name, expectedMemberType) =>
            name -> actual.memberTypes
              .get(name)
              .map(typeSubst(expectedMemberType, _, substitutions))
              .getOrElse(expectedMemberType)
        }
        TypedTessla.ObjectType(members)
      case (expectedType: TypedTessla.BuiltInType, actualType: TypedTessla.BuiltInType) =>
        assert(expectedType.name != actualType.name)
        expected
      case (expectedType, actualType) =>
        assert(expectedType.getClass != actualType.getClass)
        expected
    }
  }

  private def findPredef(name: String, env: Env): TypedTessla.Identifier = {
    env(
      spec.globalNames.getOrElse(name, throw InternalError(s"Standard library must define $name"))
    )
  }

  private def findPredef(name: String, env: Env, loc: Location): TypedTessla.Identifier = {
    env(
      spec.globalNames
        .getOrElse(name, throw UndefinedVariable(Tessla.Identifier(s"__root__.$name", loc)))
    )
  }

  /**
   * Lift a constant to a stream by wrapping it in a `default(nil, x)` and adds those newly generated definitions
   * to the known definitions.
   *
   * @param constant the constant value to lift
   * @param defs the current definitions
   * @param env the current environment
   * @param loc the location
   * @return the identifier referencing the lifted result
   */
  def liftConstant(
    constant: TypedTessla.Identifier,
    defs: TypedTessla.Definitions,
    env: Env,
    loc: Location
  ): TypedTessla.Identifier = {
    val typeOfConstant = typeMap(constant)
    val liftedType = streamType(typeOfConstant)
    val liftedId = makeIdentifier()
    val nilCall =
      TypedTessla.MacroCall(findPredef("nil", env), loc, Seq(typeOfConstant), Seq(), loc)
    val nilId = makeIdentifier()
    val nilEntry = TypedTessla.VariableEntry(nilId, nilCall, liftedType, Seq(), false, loc)
    defs.addVariable(nilEntry)
    val defaultArgs = Seq(
      TypedTessla.PositionalArgument(nilId, loc),
      TypedTessla.PositionalArgument(constant, loc)
    )
    val defaultCall =
      TypedTessla.MacroCall(findPredef("default", env), loc, Seq(typeOfConstant), defaultArgs, loc)
    val entry = TypedTessla.VariableEntry(liftedId, defaultCall, liftedType, Seq(), true, loc)
    defs.addVariable(entry)
    liftedId
  }

  def checkLiftability(functionType: TypedTessla.FunctionType): Boolean = {
    functionType.parameterTypes.forall(_._2.isValueType) && functionType.returnType.isValueType
  }

  def liftFunctionType(functionType: TypedTessla.FunctionType): TypedTessla.FunctionType = {
    TypedTessla.FunctionType(
      functionType.typeParameters,
      functionType.parameterTypes.map(t => t._1 -> streamType(t._2)),
      streamType(functionType.returnType),
      isLiftable = true
    )
  }

  /**
   * Checks if two types are subtypes of one another or equal.
   *
   * For function types this check not only checks for compatible types but also if they are liftable or not.
   *
   * @param parent the parent type
   * @param child the child type
   * @return the result of the check
   */
  def isSubtypeOrEqual(parent: TypedTessla.Type, child: TypedTessla.Type): Boolean =
    (parent, child) match {
      case (parent: TypedTessla.FunctionType, genericChild: TypedTessla.FunctionType) =>
        // TODO: This ignores type parameters of the expected type because functions with type parameters can't
        //       currently be passed around anyway. Once that is possible, this code needs to be adjusted.
        val typeSubstitutions = mutable.Map[TypedTessla.Identifier, TypedTessla.Type]()
        val compatibleParameterLength = parent.parameterTypes.length == genericChild.parameterTypes.length
        val child =
          typeSubst(genericChild, parent, typeSubstitutions)
            .asInstanceOf[TypedTessla.FunctionType]
        val compatibleLiftedness = !parent.isLiftable || child.isLiftable
        val compatibleReturnTypes = isSubtypeOrEqual(parent.returnType, child.returnType)
        val compatibleParameterTypes = compatibleParameterLength &&
          parent.parameterTypes.zip(child.parameterTypes).forall {
            // function parameters are contravariant, so the order of arguments to isSubtypeOrEqual is switched
            case ((_, parentParamType), (_, childParamType)) =>
              isSubtypeOrEqual(childParamType, parentParamType)
          }
        compatibleLiftedness && compatibleReturnTypes && compatibleParameterTypes
      case (parent: TypedTessla.ObjectType, child: TypedTessla.ObjectType) =>
        parent.memberTypes.forall {
          case (name, typ) =>
            child.memberTypes
              .get(name)
              .exists(childTyp => isSubtypeOrEqual(parent = typ, child = childTyp))
        } && (parent.memberTypes.keySet == child.memberTypes.keySet)
      case _ =>
        parent == child
    }

  val intType = TypedTessla.BuiltInType("Int", Seq())
  val floatType = TypedTessla.BuiltInType("Float", Seq())
  val stringType = TypedTessla.BuiltInType("String", Seq())
  val boolType = TypedTessla.BuiltInType("Bool", Seq())

  def streamType(t: TypedTessla.Type) = TypedTessla.BuiltInType("Events", Seq(t))

  /**
   * Translate an expression to a typed expression and infer its type.
   *
   * On macro calls, this also lifts expressions if necessary, and adds the new definitions accordingly.
   * Named arguments are translated into positional arguments.
   * Type parameters are inferred from their usage in call parameters, if possible.
   *
   * Member accesses on streams of records are lifted as well.
   *
   * @param expression the expression to translate
   * @param declaredType the declared type, if existing
   * @param defs the known definitions
   * @param flatDefs the already translated definitions
   * @param env the environment
   * @return the translated expression and the resulting type
   */
  def translateExpression(
    expression: FlatTessla.Expression,
    declaredType: Option[TypedTessla.Type],
    defs: TypedTessla.Definitions,
    flatDefs: FlatTessla.Definitions,
    env: Env
  ): (TypedTessla.Expression, TypedTessla.Type) = {
    expression match {
      case v: FlatTessla.Variable =>
        val id = env(v.id)
        TypedTessla.Variable(id, v.loc) -> typeMap(id)
      case lit: FlatTessla.Literal =>
        val t = lit.value match {
          case _: Tessla.IntLiteral   => intType
          case _: Tessla.FloatLiteral => floatType
          case _: Tessla.TimeLiteral  =>
            // TODO: Implement units of measure, this should contain the appropriate unit
            intType
          case _: Tessla.StringLiteral => stringType
        }
        TypedTessla.Literal(lit.value, lit.loc) -> t
      case inStream: FlatTessla.InputStream =>
        val typ = translateType(inStream.streamType, env)
        if (!typ.isStreamType) {
          error(InputStreamMustHaveStreamType(inStream.typeLoc))
        }
        TypedTessla.InputStream(inStream.name, typ, inStream.typeLoc, inStream.loc) -> typ
      case param: FlatTessla.Parameter =>
        val id = env(param.id)
        val t = typeMap(id)
        TypedTessla.Parameter(param.param, t, id) -> t
      case call: FlatTessla.MacroCall =>
        var liftedToFunctionType = false
        val t = typeMap(env(call.macroID)) match {
          case typ: TypedTessla.FunctionType                => typ
          case typ if lookup(flatDefs, call.macroID).parens =>
            // If the definition uses an empty parameter list, like `def x() = ...`, implicitly convert to a function type
            liftedToFunctionType = true
            TypedTessla.FunctionType(Seq(), Seq(), typ, isLiftable = false)
          case other =>
            throw TypeMismatch("function", other, call.macroLoc)
        }
        val name = call.macroID.nameOpt.getOrElse("<macro>")
        if (call.args.length != t.parameterTypes.length) {
          throw ArityMismatch(name, t.parameterTypes.length, call.args.length, call.loc)
        }
        val typeArgs = call.typeArgs.map(translateType(_, env))
        if (typeArgs.nonEmpty && typeArgs.length != t.typeParameters.length) {
          throw TypeArityMismatch(name, t.typeParameters.length, call.typeArgs.length, call.loc)
        }
        var macroID = env(call.macroID)
        var possiblyLiftedType = t
        var lastArgs: Seq[TypedTessla.Argument] = Seq()
        if (t.isLiftable && call.args.exists(arg => typeMap(env(arg.id)).isStreamType)) {
          possiblyLiftedType = liftFunctionType(t)
          lastArgs = Seq(TypedTessla.PositionalArgument(macroID, call.macroLoc))
          macroID = findPredef(s"slift${call.args.length}", env, call.macroLoc)
        }
        val parameters = lookupParameterNames(flatDefs, call.macroID) match {
          case Some((p, _)) =>
            val parameters = p.map(_._2)
            resolveNamedArgs(call.args, parameters, call.loc).map {
              case (arg, loc) => (arg.id, loc)
            }
          case None =>
            if (call.args.exists(_.isInstanceOf[FlatTessla.NamedArgument])) {
              throw Errors.InternalError("Unsupported use of named argument", call.loc)
            }
            call.args.map(arg => (arg.id, arg.loc))
        }
        val typeSubstitutions = mutable.Map(t.typeParameters.zip(typeArgs): _*)
        val typeParams = t.typeParameters.toSet
        val args = parameters.zip(possiblyLiftedType.parameterTypes).map {
          case ((argId, argLoc), (_, genericExpected)) =>
            val id = env(argId)
            val actual = typeMap(id)
            val expected = typeSubst(genericExpected, actual, typeSubstitutions)
            val possiblyLifted =
              if (isSubtypeOrEqual(parent = expected, child = actual)) {
                id
              } else if (streamType(actual) == expected) {
                liftConstant(id, defs, env, argLoc)
              } else {
                (actual, expected) match {
                  case (a: TypedTessla.FunctionType, e: TypedTessla.FunctionType)
                      if a.isLiftable && isSubtypeOrEqual(
                        parent = e,
                        child = liftFunctionType(a)
                      ) =>
                    val sliftId =
                      findPredef(s"slift${a.parameterTypes.length}_curried", env, argLoc)
                    val typeArgs = a.parameterTypes.map(_._2) :+ a.returnType
                    val sliftArgs = Seq(TypedTessla.PositionalArgument(id, argLoc))
                    val sliftCall =
                      TypedTessla.MacroCall(sliftId, argLoc, typeArgs, sliftArgs, argLoc)
                    val liftedId = makeIdentifier()
                    defs.addVariable(
                      TypedTessla.VariableEntry(liftedId, sliftCall, e, Seq(), true, argLoc)
                    )
                    liftedId
                  case _ =>
                    error(TypeMismatch(expected, actual, argLoc))
                    id
                }
              }

            TypedTessla.PositionalArgument(possiblyLifted, argLoc)
        } ++ lastArgs
        val leftOverTypeParameters = typeParams.diff(typeSubstitutions.keySet)
        if (leftOverTypeParameters.nonEmpty) {
          throw TypeArgumentsNotInferred(name, call.macroLoc)
        }
        val calculatedTypeArgs = t.typeParameters.map(typeSubstitutions)
        val returnType = typeSubst(
          possiblyLiftedType.returnType,
          possiblyLiftedType.returnType,
          typeSubstitutions
        )
        val newTypeArgs =
          if (t.isLiftable && call.args.exists(arg => typeMap(env(arg.id)).isStreamType)) {
            t.parameterTypes.map(p => typeSubst(p._2, p._2, typeSubstitutions)) :+
              typeSubst(t.returnType, t.returnType, typeSubstitutions)
          } else {
            calculatedTypeArgs
          }
        if (liftedToFunctionType)
          TypedTessla.Variable(macroID, call.macroLoc) -> returnType
        else
          TypedTessla.MacroCall(macroID, call.macroLoc, newTypeArgs, args, call.loc) -> returnType
      case o: FlatTessla.ObjectLiteral =>
        val members = mapValues(o.members)(member => TypedTessla.IdLoc(env(member.id), member.loc))
        val memberTypes = mapValues(members)(member => typeMap(member.id))
        TypedTessla
          .ObjectLiteral(members, o.loc) -> TypedTessla.ObjectType(memberTypes)

      case acc: FlatTessla.MemberAccess if resolvedMemberAccesses.contains((acc.receiver.id, acc.member)) =>
        val receiver = env(acc.receiver.id)
        val resolved = env(resolvedMemberAccesses((acc.receiver.id, acc.member)))
        val t = typeMap(resolved)
        val ma = TypedTessla.MemberAccess(
          TypedTessla.IdLoc(receiver, acc.receiver.loc),
          acc.member,
          acc.memberLoc,
          acc.loc
        )
        ma -> t

      case acc: FlatTessla.MemberAccess =>
        val receiver = env(acc.receiver.id)
        typeMap(receiver) match {
          case ot: TypedTessla.ObjectType =>
            val t = ot.memberTypes
              .getOrElse(acc.member, throw MemberNotDefined(ot, acc.member, acc.memberLoc))
            val ma = TypedTessla.MemberAccess(
              TypedTessla.IdLoc(receiver, acc.receiver.loc),
              acc.member,
              acc.memberLoc,
              acc.loc
            )
            ma -> t
          case b @ TypedTessla.BuiltInType(_, Seq(ot: TypedTessla.ObjectType)) if b.isStreamType =>
            val memberType = ot.memberTypes
              .getOrElse(acc.member, throw MemberNotDefined(ot, acc.member, acc.memberLoc))
            val slift1Id = findPredef(s"slift1", env, acc.loc)
            val macroId = makeIdentifier()
            val param = TypedTessla.Parameter(
              Tessla.Parameter(Tessla.Identifier("obj", acc.receiver.loc), None),
              ot,
              makeIdentifier()
            )
            val macroDefs = new TypedTessla.Definitions(Some(defs))
            macroDefs.addVariable(
              TypedTessla.VariableEntry(param.id, param, ot, Seq(), false, acc.receiver.loc)
            )
            val resultId = makeIdentifier()
            val resultExp =
              TypedTessla.MemberAccess(param.idLoc, acc.member, acc.memberLoc, acc.loc)
            macroDefs.addVariable(
              TypedTessla.VariableEntry(resultId, resultExp, memberType, Seq(), false, acc.loc)
            )
            val mac = TypedTessla.Macro(
              Seq(),
              Seq(None -> param),
              macroDefs,
              memberType,
              acc.loc,
              TypedTessla.IdLoc(resultId, acc.loc),
              acc.loc,
              isLiftable = false
            )
            defs.addVariable(
              TypedTessla.VariableEntry(
                macroId,
                mac,
                TypedTessla.FunctionType(Seq(), Seq(None -> ot), memberType, true),
                Seq(),
                true,
                acc.loc
              )
            )
            val arg = TypedTessla.PositionalArgument(receiver, acc.receiver.loc)
            val macroArg = TypedTessla.PositionalArgument(macroId, acc.loc)
            val liftedMacroCall = TypedTessla.MacroCall(
              slift1Id,
              acc.loc,
              Seq(ot, memberType),
              Seq(arg, macroArg),
              acc.loc
            )
            liftedMacroCall -> streamType(memberType)
          case other =>
            throw TypeMismatch("object", other, acc.receiver.loc)
        }

      case b: FlatTessla.Extern =>
        val t = declaredType.getOrElse(throw MissingTypeAnnotationExtern(b.name, b.loc))
        val typeParameters = t match {
          case ft: TypedTessla.FunctionType => ft.typeParameters
          case _                            => Seq()
        }
        val innerEnv =
          env ++ b.typeParameters.zip(typeParameters) ++ b.parameters.map(_._2.id).map { id =>
            id -> makeIdentifier(id.nameOpt)
          }
        val parameters = b.parameters.map {
          case (eval, p) =>
            val t = translateType(p.parameterType, innerEnv)
            eval -> TypedTessla.Parameter(p.param, t, innerEnv(p.id))
        }
        val refImpl = b.referenceImplementation.map(env)
        TypedTessla.Extern(b.name, typeParameters, parameters, refImpl, b.loc) -> t

      case mac: FlatTessla.Macro =>
        val tvarIDs = declaredType match {
          case Some(f: TypedTessla.FunctionType) =>
            f.typeParameters
          case _ =>
            mac.typeParameters.map(tvar => makeIdentifier(tvar.nameOpt))
        }
        val tvarEnv = mac.typeParameters.zip(tvarIDs).toMap
        val (innerDefs, innerEnv) = translateDefs(mac.body, Some(defs), env ++ tvarEnv)
        val result = TypedTessla.IdLoc(innerEnv(mac.result.id), mac.result.loc)
        val returnType = typeMap(result.id)
        val paramTypes = parameterTypes(mac).map(t => t._1 -> translateType(t._2, env ++ tvarEnv))
        val macroType =
          TypedTessla.FunctionType(tvarIDs, paramTypes, returnType, isLiftable = mac.isLiftable)
        val parameters = mac.parameters.map {
          case (eval, p) =>
            val t = translateType(p.parameterType, innerEnv)
            eval -> TypedTessla.Parameter(p.param, t, innerEnv(p.id))
        }
        if (mac.isLiftable) {
          if (!checkLiftability(macroType)) {
            error(UnliftableMacroType(mac.headerLoc))
          }
        }
        TypedTessla.Macro(
          tvarIDs,
          parameters,
          innerDefs,
          returnType,
          mac.headerLoc,
          result,
          mac.loc,
          mac.isLiftable
        ) -> macroType
    }
  }

  def resolveNamedArgs(
    args: Seq[FlatTessla.Argument],
    parameters: Seq[FlatTessla.Parameter],
    loc: Location
  ): Seq[(FlatTessla.Argument, Location)] = {
    val positions = parameters.map(p => p.name).zipWithIndex.toMap
    var allowPosArg = true
    args.zipWithIndex
      .map {
        case (arg: FlatTessla.NamedArgument, idx) =>
          val pos = positions.getOrElse(arg.name, throw Errors.UndefinedNamedArg(arg.name, arg.idLoc.loc))
          allowPosArg &= pos == idx
          (pos, (arg, loc))
        case (arg: FlatTessla.PositionalArgument, idx) =>
          if (!allowPosArg) throw Errors.PosArgAfterNamedArg(arg.loc)
          (idx, (arg, arg.loc))
      }
      .sortBy(_._1)
      .map(_._2)
  }

  def lookup(env: FlatTessla.Definitions, id: FlatTessla.Identifier): FlatTessla.VariableEntry =
    env.variables.getOrElse(id, lookup(env.parent.get, id))

  /**
   * Look up the parameter names of the macro with the given identifier. This is used to be able to resolve
   * named arguments.
   *
   * @param env the environment
   * @param macroID the macro id
   * @return the parameter names and locations
   */
  def lookupParameterNames(
    env: FlatTessla.Definitions,
    macroID: FlatTessla.Identifier
  ): Option[(Seq[(Option[TesslaAST.RuntimeEvaluation], FlatTessla.Parameter)], Location)] =
    lookup(env, macroID).expression match {
      case FlatTessla.Macro(_, parameter, _, _, _, _, loc, _) => Some((parameter, loc))
      case FlatTessla.Extern(_, _, parameter, _, loc)         => Some((parameter, loc))
      case FlatTessla.MemberAccess(receiver, member, _, _) =>
        lookup(env, receiver.id).expression match {
          case FlatTessla.ObjectLiteral(members, _) =>
            members.get(member).map(_.id).flatMap(lookupParameterNames(env, _))
          case _ => None
        }
      case _ => None
    }
}
