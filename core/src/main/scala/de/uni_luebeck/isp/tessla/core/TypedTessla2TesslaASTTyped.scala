package de.uni_luebeck.isp.tessla.core

import cats.data.Ior
import de.uni_luebeck.isp.tessla.core.Errors.UndefinedBaseTime
import de.uni_luebeck.isp.tessla.core.Tessla.{ConstantExpression, FloatLiteral, IntLiteral, StringLiteral, TimeLiteral}
import de.uni_luebeck.isp.tessla.core.TesslaAST.Typed
import de.uni_luebeck.isp.tessla.core.TranslationPhase._

import scala.collection.mutable
import de.uni_luebeck.isp.tessla.core.TypedTessla._
import de.uni_luebeck.isp.tessla.core.util._

import scala.collection.immutable.ArraySeq

class TypedTessla2TesslaASTCore(baseTime: Option[TimeLiteral])
    extends TranslationPhase[TypedTessla.TypedSpecification, Typed.Specification] {
  override def translate(spec: TypedTessla.TypedSpecification): Result[TesslaAST.Typed.Specification] = {
    new TypedTessla2TesslaASTTypedWorker(spec, baseTime).translate()
  }
}

/**
 * Translates from the old TeSSLa 1.0 AST to the newly introduced AST in version 1.2.
 *
  * This phase also adds a \$name annotation to each output stream defining its display name.
 *
  * @param baseTime the base time relative to which time literals get evaluated
 */

class TypedTessla2TesslaASTTypedWorker(
  spec: TypedTessla.TypedSpecification,
  baseTime: Option[TimeLiteral]
) extends TranslationPhase.Translator[Typed.Specification] {

  // used to instantiate unused type variables where actual type cannot be recovered from TypedTessla
  val unknownType = Typed.InstantiatedType("Unknown", Nil)

  private val ins = mutable.Map[Typed.Identifier, (Typed.Type, Typed.Annotations)]()

  override protected def translateSpec(): TesslaAST.Typed.Specification = {
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
    toType(value.typeInfo)
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
            case Parameter(param, parameterType, _) =>
              Typed.ExpressionRef(
                Typed.Identifier(Ior.Left(param.id.name), param.id.loc),
                toType(parameterType),
                param.loc
              )
            case Macro(typeParameters, parameters, body, _, _, result, loc, _) =>
              Typed.FunctionExpression(
                typeParameters.map(toIdentifier(_, Location.unknown)).toList,
                parameters.map {
                  case (eval, typ) =>
                    (
                      Typed.Identifier(Ior.Left(typ.param.id.name), typ.loc),
                      eval.getOrElse(
                        if (typ.parameterType.isStreamType) TesslaAST.LazyEvaluation
                        else TesslaAST.StrictEvaluation
                      ),
                      toType(typ.parameterType)
                    )
                }.toList,
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

              Typed.ApplicationExpression(
                callable,
                args
                  .zip(callable.tpe.asInstanceOf[Typed.FunctionType].paramTypes)
                  .map {
                    case (arg, (_, t)) =>
                      val tpe = lookupType(arg.id, env)
                      val ref = Typed.ExpressionRef(toIdentifier(arg.id, lookup(env, arg.id).loc), tpe, arg.loc)
                      if (t == tpe) ref
                      else {
                        val typeArgs = determineTypeArguments(t, tpe).withDefaultValue(unknownType)
                        val typeArgList =
                          tpe.asInstanceOf[Typed.FunctionType].typeParams.map(typeArgs)
                        Typed.TypeApplicationExpression(ref, typeArgList)
                      }
                  }
                  .to(ArraySeq),
                loc
              )

            case Extern(name, _, _, _, loc) => // TODO: support reference implementation
              Typed.ExternExpression(
                name,
                toType(definition.typeInfo),
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
                    (Typed.ExpressionRef(toIdentifier(x._2.id, x._2.loc), lookupType(x._2.id, env)), Location.unknown)
                  )
                ),
                loc
              )
            case Literal(value, loc) => translateLiteralValue(value, loc)
          }
        )
    }

  def translateAnnotations(annotations: Seq[Annotation]): Map[String, ArraySeq[Typed.RecordConstructorExpression]] =
    mapValues(
      annotations
        .map { annotation =>
          val translatedArguments = annotation.arguments.map { arg =>
            arg._1 -> (translateConstantExpression(arg._2), Location.unknown)
          }
          annotation.name -> Typed.RecordConstructorExpression(translatedArguments)
        }
        .groupBy(_._1)
    )(_.map(_._2).to(ArraySeq))

  def translateLiteralValue(value: LiteralValue, loc: Location): Typed.ExpressionArg = value match {
    case IntLiteral(value)    => Typed.IntLiteralExpression(value, loc)
    case FloatLiteral(value)  => Typed.FloatLiteralExpression(value, loc)
    case StringLiteral(value) => Typed.StringLiteralExpression(value, loc)
    case timeLit @ TimeLiteral(x, tu) =>
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
        parameterTypes.map {
          case (eval, typ) =>
            (
              eval.getOrElse(
                if (typ.isStreamType) TesslaAST.LazyEvaluation else TesslaAST.StrictEvaluation
              ),
              toType(typ)
            )
        }.toList,
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

  def determineTypeArguments(
    resolved: Typed.Type,
    resolvable: Typed.Type
  ): Map[Typed.Identifier, Typed.Type] = (resolved, resolvable) match {
    case (tpe, Typed.TypeParam(name, _)) =>
      Map(name -> tpe)
    case (Typed.InstantiatedType(_, typeArgs1, _), Typed.InstantiatedType(_, typeArgs2, _)) =>
      typeArgs1
        .zip(typeArgs2)
        .map {
          case (arg1, arg2) =>
            determineTypeArguments(arg1, arg2)
        }
        .fold(Map())(_ ++ _)
    case (
          Typed.FunctionType(typeParams, paramTypes1, resultType1, _),
          Typed.FunctionType(_, paramTypes2, resultType2, _)
        ) =>
      (paramTypes1
        .zip(paramTypes2)
        .map {
          case ((_, t1), (_, t2)) =>
            determineTypeArguments(t1, t2)
        }
        .fold(Map())(_ ++ _) ++ determineTypeArguments(resultType1, resultType2)) -- typeParams
    case (Typed.RecordType(entries1, _), Typed.RecordType(entries2, _)) =>
      entries1
        .map {
          case (name, tpe) =>
            determineTypeArguments(tpe._1, entries2(name)._1)
        }
        .fold(Map())(_ ++ _)
    case (_, _) => Map()
  }

}
