package de.uni_luebeck.isp.tessla

import TesslaAST.{Identifier, Typed}
import de.uni_luebeck.isp.tessla.Errors.UndefinedTimeUnit
import de.uni_luebeck.isp.tessla.Tessla.{FloatLiteral, IntLiteral, StringLiteral, TimeLiteral}
import de.uni_luebeck.isp.tessla.TypedTessla.{BuiltInOperator, InputStream, Literal, Macro, MacroCall, MemberAccess, ObjectLiteral, Parameter, StaticIfThenElse, Variable, VariableEntry}

import scala.collection.mutable

class TypedTessla2TesslaASTCoreWorker(spec: TypedTessla.TypedSpecification, baseTimeUnit: Option[TimeUnit]) extends TranslationPhase.Translator[Typed.Specification] {

  val staticiteExtern = Typed.ExternExpression(List(Identifier("A")), List(
    (Identifier("c"), TesslaAST.StrictEvaluation, Typed.BoolType),
    (Identifier("t"), TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("A"))),
    (Identifier("e"), TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("A")))
  ), Typed.TypeParam(Identifier("A")), "staticite")

  val ins = mutable.Map[Identifier, (Typed.Type, List[Nothing])]()

  override protected def translateSpec() = {
    val outs = spec.outStreams.map(x => (toIdenifier(x.id), x.nameOpt, Nil)).toList
    val defs: Map[Identifier, Typed.ExpressionArg] = translateEnv(spec.globalDefs)

    Typed.Specification(ins.toMap, defs, outs)
  }

  def lookupType(id: TypedTessla.Identifier, env: TypedTessla.Definitions): Typed.Type = env.variables.get(id) match {
    case Some(value) => toType(value.typeInfo)
    case None => lookupType(id, env.parent.get)
  }

  def translateEnv(env: TypedTessla.Definitions): Map[Identifier, Typed.ExpressionArg] = env.variables.toMap.map { case (id, definition) =>
    (toIdenifier(id), definition.expression match {
      case InputStream(name, streamType, typeLoc, loc) =>
        ins += (Identifier(name) -> (toType(streamType), Nil))
          Typed.ExpressionRef(Identifier(name), toType(streamType, typeLoc), loc)
      case Parameter(param, parameterType, id) => Typed.ExpressionRef(toIdenifier(id), toType(parameterType))
      case Macro(typeParameters, parameters, body, returnType, headerLoc, result, loc, _) =>
        Typed.FunctionExpression(
          typeParameters.map(toIdenifier).toList,
          parameters.map(x => (toIdenifier(x.id), TesslaAST.LazyEvaluation, toType(x.parameterType))).toList, // TODO: determine lazyness
          translateEnv(body),
          Typed.ExpressionRef(toIdenifier(result.id), lookupType(result.id, body)),
          loc
        )
      case Variable(id, loc) => Typed.ExpressionRef(toIdenifier(id), lookupType(id, env)) // TODO: determine type
      case MacroCall(macroID, macroLoc, typeArgs, args, loc) =>
        Typed.ApplicationExpression(
          Typed.TypeApplicationExpression(
            Typed.ExpressionRef(toIdenifier(macroID), lookupType(macroID, env)), // TODO: determine type
            typeArgs.map(x => toType(x)).toList
          ),
          args.map(x => Typed.ExpressionRef(toIdenifier(x.id), lookupType(x.id, env))).toList
        )
      case BuiltInOperator(name, typeParameters, parameters, referenceImplementation, loc) =>
        Typed.ExternExpression(typeParameters.map(toIdenifier).toList,
          parameters.map(x => (toIdenifier(x.id), TesslaAST.LazyEvaluation, toType(x.parameterType))).toList,
          null, name)
      case StaticIfThenElse(condition, thenCase, elseCase, loc) =>
        Typed.ApplicationExpression(staticiteExtern, List(
          Typed.ExpressionRef(toIdenifier(condition.id), lookupType(condition.id, env)),
          Typed.ExpressionRef(toIdenifier(thenCase.id), lookupType(thenCase.id, env)),
          Typed.ExpressionRef(toIdenifier(elseCase.id), lookupType(elseCase.id, env))
        )) // TODO: Instantiate type parameter correctly
      case ObjectLiteral(members, loc) => Typed.RecordConstructorExpression(
        members.map(x => (Identifier(x._1), Typed.ExpressionRef(toIdenifier(x._2.id), lookupType(x._2.id, env)))), loc)
      case MemberAccess(receiver, member, memberLoc, loc) =>
        Typed.RecordAccesorExpression(Identifier(member), Typed.ExpressionRef(toIdenifier(receiver.id), lookupType(receiver.id, env)))
      case Literal(value, loc) => value match {
        case IntLiteral(value) => Typed.IntLiteralExpression(value, loc)
        case FloatLiteral(value) => Typed.FloatLiteralExpression(value, loc)
        case StringLiteral(value) => Typed.StringLiteralExpression(value, loc)
        case TimeLiteral(x, tu) => baseTimeUnit match {
          case Some(base) =>
            val conversionFactor = tu.convertTo(base).getOrElse(throw Errors.TimeUnitConversionError(tu, base))
            Typed.IntLiteralExpression(conversionFactor * x, loc)
          case None =>
            error(UndefinedTimeUnit(tu.loc))
            Typed.IntLiteralExpression(x, loc)
        }
      }
    })
  }

  def toType(tpe: TypedTessla.Type, location: Location = Location.unknown):  Typed.Type = tpe match {
    case TypedTessla.BuiltInType(name, typeArgs) => Typed.InstatiatedType(name, typeArgs.map(toType(_)).toList)
    case TypedTessla.FunctionType(typeParameters, parameterTypes, returnType, _) =>
      Typed.FunctionType(typeParameters.map(toIdenifier).toList,
        parameterTypes.map(x => (TesslaAST.LazyEvaluation, toType(x))).toList,
        toType(returnType))
    case TypedTessla.ObjectType(memberTypes, isOpen) =>
      if (isOpen) {
        throw new IllegalArgumentException("Open types not supported.")
      }
      Typed.RecordType(memberTypes.map(x => (Identifier(x._1), toType(x._2))))
    case TypedTessla.TypeParameter(id, loc) => Typed.TypeParam(toIdenifier(id), loc)
  }

  def toIdenifier(identifier: TypedTessla.Identifier) = Identifier(identifier.nameOpt.getOrElse("") + "$" + identifier.uid)
}

class TypedTessla2TesslaASTCore(baseTimeUnit: Option[TimeUnit]) extends TranslationPhase[TypedTessla.TypedSpecification, Typed.Specification] {
  override def translate(spec: TypedTessla.TypedSpecification) = {
    new TypedTessla2TesslaASTCoreWorker(spec, baseTimeUnit).translate()
  }
}