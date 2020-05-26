package de.uni_luebeck.isp.tessla

import TesslaAST.Typed
import Typed.Identifier
import cats.data.Ior
import de.uni_luebeck.isp.tessla.Errors.UndefinedBaseTime
import de.uni_luebeck.isp.tessla.Tessla.{
  ConstantExpression,
  FloatLiteral,
  IntLiteral,
  LiteralValue,
  StringLiteral,
  TimeLiteral
}
import de.uni_luebeck.isp.tessla.TypedTessla.{
  Annotation,
  BuiltInOperator,
  InputStream,
  Literal,
  Macro,
  MacroCall,
  MemberAccess,
  ObjectLiteral,
  Parameter,
  StaticIfThenElse,
  Variable,
  VariableEntry
}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

class TypedTessla2TesslaASTTypedWorker(
  spec: TypedTessla.TypedSpecification,
  baseTime: Option[TimeLiteral]
) extends TranslationPhase.Translator[Typed.Specification] {

  // used to instantiate unsued type variables where actual type cannnot be recovered from TypedTessla
  val unknownType = Typed.InstantiatedType("Unknown", Nil)

  val staticiteExtern = Typed.ExternExpression(List(Identifier("A")), List(
    (TesslaAST.StrictEvaluation, Typed.BoolType),
    (TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("A"))),
    (TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("A")))
  ), Typed.TypeParam(Identifier("A")), "staticite")


  val knownExterns = Map(
    "true" -> Typed.ApplicationExpression(
      Typed.TypeApplicationExpression(
        Typed.ExternExpression(Nil, Nil, Typed.InstantiatedType("Bool", Nil), "true"),
        Nil
      ),
      ArraySeq()
    ),
    "false" -> Typed.ApplicationExpression(
      Typed.TypeApplicationExpression(
        Typed.ExternExpression(Nil, Nil, Typed.InstantiatedType("Bool", Nil), "false"),
        Nil
      ),
      ArraySeq()
    ),
    "last" -> Typed.ExternExpression(
      List(Identifier("A"), Identifier("B")),
      List(
        (
          TesslaAST.LazyEvaluation,
          Typed.InstantiatedType("Events", List(Typed.TypeParam(Identifier("A"))))
        ),
        (
          TesslaAST.StrictEvaluation,
          Typed.InstantiatedType("Events", List(Typed.TypeParam(Identifier("B"))))
        )
      ),
      Typed.InstantiatedType("Events", List(Typed.TypeParam(Identifier("A")))),
      "last"
    ),
    "delay" -> Typed.ExternExpression(
      List(Identifier("A")),
      List(
        (TesslaAST.LazyEvaluation, Typed.InstantiatedType("Events", List(Typed.IntType))),
        (
          TesslaAST.StrictEvaluation,
          Typed.InstantiatedType("Events", List(Typed.TypeParam(Identifier("A"))))
        )
      ),
      Typed.InstantiatedType("Events", List(Typed.UnitType)),
      "delay"
    ),
    "ite" -> Typed.ExternExpression(
      List(Identifier("A")),
      List(
        (TesslaAST.StrictEvaluation, Typed.BoolType),
        (TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("A"))),
        (TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("A")))
      ),
      Typed.TypeParam(Identifier("A")),
      "ite"
    ),
    "List_fold" -> Typed.ExternExpression(
      List(Identifier("A"), Identifier("B")),
      List(
        (
          TesslaAST.StrictEvaluation,
          Typed.InstantiatedType("List", List(Typed.TypeParam(Identifier("A"))))
        ),
        (TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("B"))),
        (
          TesslaAST.StrictEvaluation,
          Typed.FunctionType(
            Nil,
            List(
              (TesslaAST.StrictEvaluation, Typed.TypeParam(Identifier("B"))),
              (TesslaAST.StrictEvaluation, Typed.TypeParam(Identifier("A")))
            ),
            Typed.TypeParam(Identifier("B"))
          )
        )
      ),
      Typed.TypeParam(Identifier("B")),
      "List_fold"
    ),
    "Set_fold" -> Typed.ExternExpression(
      List(Identifier("A"), Identifier("B")),
      List(
        (
          TesslaAST.StrictEvaluation,
          Typed.InstantiatedType("Set", List(Typed.TypeParam(Identifier("A"))))
        ),
        (TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("B"))),
        (
          TesslaAST.StrictEvaluation,
          Typed.FunctionType(
            Nil,
            List(
              (TesslaAST.StrictEvaluation, Typed.TypeParam(Identifier("B"))),
              (TesslaAST.StrictEvaluation, Typed.TypeParam(Identifier("A")))
            ),
            Typed.TypeParam(Identifier("B"))
          )
        )
      ),
      Typed.TypeParam(Identifier("B")),
      "Set_fold"
    ),
    "Map_fold" -> Typed.ExternExpression(
      List(Identifier("A"), Identifier("B"), Identifier("C")),
      List(
        (
          TesslaAST.StrictEvaluation,
          Typed.InstantiatedType(
            "Map",
            List(Typed.TypeParam(Identifier("A")), Typed.TypeParam(Identifier("B")))
          )
        ),
        (TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("C"))),
        (
          TesslaAST.StrictEvaluation,
          Typed.FunctionType(
            Nil,
            List(
              (TesslaAST.StrictEvaluation, Typed.TypeParam(Identifier("C"))),
              (TesslaAST.StrictEvaluation, Typed.TypeParam(Identifier("A"))),
              (TesslaAST.StrictEvaluation, Typed.TypeParam(Identifier("B")))
            ),
            Typed.TypeParam(Identifier("C"))
          )
        )
      ),
      Typed.TypeParam(Identifier("C")),
      "Map_fold"
    )
  )
  private val ins = mutable.Map[Typed.Identifier, (Typed.Type, Typed.Annotations)]()

  override protected def translateSpec() = {
    val outs = spec.outStreams.map { out =>
      val annotations = translateAnnotations(out.annotations)
      val name = Map("$name" -> ArraySeq(Typed.StringLiteralExpression(out.name)))
      (
        Typed.ExpressionRef(
          toIdentifier(out.id, lookup(spec.globalDefs, out.id).loc),
          lookupType(out.id, spec.globalDefs),
          out.loc
        ),
        annotations ++ name
      )
    }.toList
    val defs: Map[Typed.Identifier, Typed.ExpressionArg] = translateEnv(spec.globalDefs)

    Typed.Specification(ins.toMap, defs, outs, defs.flatMap(_._1.idOrName.right).max)
  }

  def lookupType(id: TypedTessla.Identifier, env: TypedTessla.Definitions): Typed.Type = {
    val value = lookup(env, id)
    value.expression match {
      case TypedTessla.MemberAccess(receiver, member, _, _) =>
        lookupType(receiver.id, env).asInstanceOf[Typed.RecordType].entries(member)._1
      case TypedTessla.BuiltInOperator(name, _, _, _, _) =>
        knownExterns.get(name).map(_.tpe).getOrElse(toType(value.typeInfo))
      case TypedTessla.ObjectLiteral(members, _) =>
        Typed.RecordType(members.map(x => (x._1 -> (lookupType(x._2.id, env), Location.unknown))))
      case _ => toType(value.typeInfo)
    }
  }

  def lookupType2(id: TypedTessla.Identifier, env: TypedTessla.Definitions): Typed.Type =
    env.variables.get(id) match {
      case Some(value) => toType(value.typeInfo)
      case None => lookupType2(id, env.parent.get)
    }

  def translateEnv(env: TypedTessla.Definitions): Map[Typed.Identifier, Typed.ExpressionArg] =
    env.variables.toMap.map {
      case (id, definition) =>
        (
          toIdentifier(id, definition.loc),
          definition.expression match {
            case InputStream(name, streamType, typeLoc, loc) =>
              ins += (Typed.Identifier(Ior.Left(name), loc) -> (toType(
                streamType
              ), translateAnnotations(definition.annotations)))
              Typed.ExpressionRef(
                Typed.Identifier(Ior.Left(name), loc),
                toType(streamType, typeLoc),
                loc
              )
            case Parameter(param, parameterType, id) =>
              Typed.ExpressionRef(
                Typed.Identifier(Ior.Left(param.id.name), param.id.loc),
                toType(parameterType),
                param.loc
              )
            case Macro(typeParameters, parameters, body, returnType, headerLoc, result, loc, _) =>
              Typed.FunctionExpression(
                typeParameters.map(toIdentifier(_, Location.unknown)).toList,
                parameters
                  .map(x =>
                    (
                      Typed.Identifier(Ior.Left(x.param.id.name), x.loc),
                      if (x.parameterType.isStreamType) TesslaAST.LazyEvaluation
                      else TesslaAST.StrictEvaluation,
                      toType(x.parameterType)
                    )
                  )
                  .toList,
                translateEnv(body),
                Typed.ExpressionRef(
                  toIdentifier(result.id, Location.unknown),
                  lookupType(result.id, body),
                  result.loc
                ),
                loc
              )
            case Variable(id, loc) =>
              Typed.ExpressionRef(toIdentifier(id, lookup(env, id).loc), lookupType(id, env), loc)
            case MacroCall(macroID, macroLoc, typeArgs, args, loc) =>
              val callable = Typed.TypeApplicationExpression(
                Typed.ExpressionRef(toIdentifier(macroID, macroLoc), lookupType(macroID, env)),
                typeArgs.map(x => toType(x)).toList
              )

              val parameterNames = lookupParameterNames(env, macroID)

              val argumentList = parameterNames match {
                case Some((parameter, loc)) =>
                  generateArgumentList(macroID.toString, loc, args, parameter)
                case None =>
                  if (args.exists(_.isInstanceOf[TypedTessla.NamedArgument])) {
                    throw Errors.InternalError("Unsupported use of named argument", loc)
                  }
                  args.map(a => (a.id, a.loc))
              }

              Typed.ApplicationExpression(
                callable,
                argumentList.zip(callable.tpe.asInstanceOf[Typed.FunctionType].paramTypes).map { case ((id, loc), (_, t)) =>
                  val tpe = lookupType(id, env)
                  val ref = Typed.ExpressionRef(toIdentifier(id, lookup(env, id).loc), tpe, loc)
                  if (t == tpe) ref else {
                    val typeArgs = determineTypeArguments(t, tpe).withDefaultValue(unknownType)
                    val typeArgList = tpe.asInstanceOf[Typed.FunctionType].typeParams.map(typeArgs)
                    Typed.TypeApplicationExpression(ref, typeArgList)
                  }
                }.to(ArraySeq),
                loc)

            case BuiltInOperator(name, typeParameters, parameters, referenceImplementation, loc) => // TODO: suport reference implementation
              knownExterns.getOrElse(
                name,
                Typed.ExternExpression(
                  typeParameters.map(toIdentifier(_, Location.unknown)).toList,
                  parameters.map(x => (TesslaAST.StrictEvaluation, toType(x.parameterType))).toList,
                  toType(definition.typeInfo.asInstanceOf[TypedTessla.FunctionType].returnType),
                  name,
                  loc
                )
              )
            case StaticIfThenElse(condition, thenCase, elseCase, loc) =>
              Typed.ApplicationExpression(
                Typed.TypeApplicationExpression(
                  staticiteExtern,
                  List(
                    lookupType(thenCase.id, env)
                  )
                ),
                ArraySeq(
                  Typed.ExpressionRef(
                    toIdentifier(condition.id, loc),
                    lookupType(condition.id, env),
                    loc
                  ),
                  Typed.ExpressionRef(
                    toIdentifier(thenCase.id, loc),
                    lookupType(thenCase.id, env),
                    loc
                  ),
                  Typed.ExpressionRef(
                    toIdentifier(elseCase.id, loc),
                    lookupType(elseCase.id, env),
                    loc
                  )
                ),
                loc
              )
            case MemberAccess(receiver, member, memberLoc, loc) =>
              Typed.RecordAccessorExpression(
                member,
                Typed.ExpressionRef(
                  toIdentifier(receiver.id, receiver.loc),
                  lookupType(receiver.id, env)
                ),
                memberLoc,
                loc
              )
            case ObjectLiteral(members, loc) =>
              Typed.RecordConstructorExpression(
                members.map(x =>
                  (
                    x._1,
                    (
                      Typed
                        .ExpressionRef(toIdentifier(x._2.id, x._2.loc), lookupType(x._2.id, env)),
                      Location.unknown
                    )
                  )
                ),
                loc
              )
            case Literal(value, loc) => translateLiteralValue(value, loc)
          }
        )
    }

  def translateAnnotations(annotations: Seq[Annotation]) =
    annotations
      .map { annotation =>
        val translatedArguments = annotation.arguments.map { arg =>
          arg._1 -> (translateConstantExpression(arg._2), Location.unknown)
        }
        annotation.name -> Typed.RecordConstructorExpression(translatedArguments)
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).to(ArraySeq))
      .toMap

  def translateLiteralValue(value: LiteralValue, loc: Location): Typed.ExpressionArg = value match {
    case IntLiteral(value) => Typed.IntLiteralExpression(value, loc)
    case FloatLiteral(value) => Typed.FloatLiteralExpression(value, loc)
    case StringLiteral(value) => Typed.StringLiteralExpression(value, loc)
    case timeLit@TimeLiteral(x, tu) =>
      baseTime match {
        case Some(base) =>
          val unitConversionFactor = timeLit.unit.convertTo(base.unit).getOrElse {
            error(Errors.TimeUnitConversionError(timeLit.unit, base.unit))
            BigInt(1)
          }
          val value = unitConversionFactor * timeLit.value / base.value
          if (timeLit.value > 0 && value == 0) {
            error(Errors.TimeConversionError(timeLit, base))
          }
          Typed.IntLiteralExpression(value, loc)
        case None =>
          error(UndefinedBaseTime(tu.loc))
          Typed.IntLiteralExpression(x, loc)
      }
  }

  def translateConstantExpression(exp: ConstantExpression): Typed.ExpressionArg = exp match {
    case ConstantExpression.Object(members, loc) =>
      val translatedMembers =
        members.map(x => (x._1.name, (translateConstantExpression(x._2), x._1.loc)))
      Typed.RecordConstructorExpression(translatedMembers, loc)
    case ConstantExpression.Literal(value, loc) => translateLiteralValue(value, loc)
  }

  def toType(tpe: TypedTessla.Type, location: Location = Location.unknown): Typed.Type = tpe match {
    case TypedTessla.BuiltInType(name, typeArgs) =>
      Typed.InstantiatedType(name, typeArgs.map(toType(_)).toList)
    case TypedTessla.FunctionType(typeParameters, parameterTypes, returnType, _) =>
      Typed.FunctionType(
        typeParameters.map(toIdentifier(_, Location.unknown)).toList,
        parameterTypes
          .map(x =>
            (
              if (x.isStreamType) TesslaAST.LazyEvaluation else TesslaAST.StrictEvaluation,
              toType(x)
            )
          )
          .toList,
        toType(returnType)
      )
    case TypedTessla.ObjectType(memberTypes, isOpen) =>
      if (isOpen) {
        throw new IllegalArgumentException("Open types not supported.")
      }
      Typed.RecordType(memberTypes.map(x => (x._1, (toType(x._2), Location.unknown))))
    case TypedTessla.TypeParameter(id, loc) => Typed.TypeParam(toIdentifier(id, loc), loc)
  }

  def lookup(env: TypedTessla.Definitions, id: TypedTessla.Identifier): VariableEntry =
    env.variables.getOrElse(id, lookup(env.parent.get, id))

  def toIdentifier(identifier: TypedTessla.Identifier, location: Location) =
    Typed.Identifier(Ior.fromOptions(identifier.nameOpt, Some(identifier.uid)).get, location)

  def determineTypeArguments(resolved: Typed.Type, resolvable: Typed.Type): Map[Typed.Identifier, Typed.Type] = (resolved, resolvable) match {
    case (tpe, Typed.TypeParam(name, _)) =>
      Map(name -> tpe)
    case (Typed.InstantiatedType(_, typeArgs1, _), Typed.InstantiatedType(_, typeArgs2, _)) =>
      typeArgs1.zip(typeArgs2).map { case (arg1, arg2) =>
        determineTypeArguments(arg1, arg2)
      }.fold(Map())(_ ++ _)
    case (Typed.FunctionType(typeParams, paramTypes1, resultType1, _), Typed.FunctionType(_, paramTypes2, resultType2, _)) =>
      (paramTypes1.zip(paramTypes2).map { case ((_, t1), (_, t2)) =>
        determineTypeArguments(t1, t2)
      }.fold(Map())(_ ++ _) ++ determineTypeArguments(resultType1, resultType2)) -- typeParams
    case (Typed.RecordType(entries1, _), Typed.RecordType(entries2, _)) =>
      entries1.map { case (name, tpe) =>
        determineTypeArguments(tpe._1, entries2(name)._1)
      }.fold(Map())(_ ++ _)
    case (t1, t2) => Map()
  }

  def generateArgumentList(
    macroName: String,
    loc: Location,
    args: Seq[TypedTessla.Argument],
    parameter: Seq[TypedTessla.Parameter]
  ): Seq[(TypedTessla.Identifier, Location)] = {
    val name2pos = parameter.map(p => p.name).zipWithIndex.toMap
    val pos2id = args.zipWithIndex
      .map {
        case (TypedTessla.PositionalArgument(id, loc), i) => (i, (id, loc))
        case (TypedTessla.NamedArgument(name, idLoc, _), _) =>
          (
            name2pos.getOrElse(name, throw Errors.UndefinedNamedArg(name, idLoc.loc)),
            (idLoc.id, loc)
          )
      }
      .sortBy(_._1)
    if (pos2id.map(_._1) != (0 until parameter.size)) {
      throw Errors.ArityMismatch(
        macroName,
        parameter.size,
        pos2id.size,
        loc
      ) // TODO: better error message for same number of arguments but missmatched arguments
    }
    pos2id.map(_._2)
  }

  // Note: the current type checker does not treat parameter names as part of signatures.
  // Therefor, named arguments can only be resolved, if the macro id can be resolved statically.
  def lookupParameterNames(env: TypedTessla.Definitions, macroID: TypedTessla.Identifier) =
    lookup(env, macroID).expression match {
      case TypedTessla.Macro(_, parameter, _, _, _, _, loc, _) => Some((parameter, loc))
      case TypedTessla.BuiltInOperator(_, _, parameter, _, loc) => Some((parameter, loc))
      case TypedTessla.MemberAccess(receiver, member, _, _) =>
        lookup(env, receiver.id).expression match {
          case TypedTessla.ObjectLiteral(members, _) =>
            members.getOrElse(member, None) match {
              case TypedTessla.Macro(_, parameter, _, _, _, _, loc, _) => Some((parameter, loc))
              case TypedTessla.BuiltInOperator(_, _, parameter, _, loc) => Some((parameter, loc))
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
}

class TypedTessla2TesslaASTCore(baseTime: Option[TimeLiteral])
  extends TranslationPhase[TypedTessla.TypedSpecification, Typed.Specification] {
  override def translate(spec: TypedTessla.TypedSpecification) = {
    new TypedTessla2TesslaASTTypedWorker(spec, baseTime).translate()
  }
}
