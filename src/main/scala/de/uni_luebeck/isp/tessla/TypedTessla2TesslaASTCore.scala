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
    val outs = spec.outStreams.map(x => (toIdenifier(x.id, x.loc), x.nameOpt, Nil)).toList
    val defs: Map[Identifier, Typed.ExpressionArg] = translateEnv(spec.globalDefs)

    Typed.Specification(ins.toMap, defs, outs)
  }

  def lookupType(id: TypedTessla.Identifier, env: TypedTessla.Definitions): Typed.Type = env.variables.get(id) match {
    case Some(value) => toType(value.typeInfo)
    case None => lookupType(id, env.parent.get)
  }

  def translateEnv(env: TypedTessla.Definitions): Map[Identifier, Typed.ExpressionArg] = env.variables.toMap.map { case (id, definition) =>
    (toIdenifier(id, definition.loc), definition.expression match {
      case InputStream(name, streamType, typeLoc, loc) =>
        ins += (Identifier(name) -> (toType(streamType), Nil))
        Typed.ExpressionRef(Identifier(name), toType(streamType, typeLoc), loc)
      case Parameter(param, parameterType, id) =>
        Typed.ExpressionRef(TesslaAST.Identifier(param.id.name, param.loc), toType(parameterType), param.loc)
      case Macro(typeParameters, parameters, body, returnType, headerLoc, result, loc, _) =>
        Typed.FunctionExpression(
          typeParameters.map(toIdenifier(_, Location.unknown)).toList,
          parameters.map(x => (TesslaAST.Identifier(x.param.id.name, x.loc), if (x.parameterType.isStreamType) TesslaAST.LazyEvaluation else TesslaAST.StrictEvaluation,
            toType(x.parameterType))).toList, // TODO: determine lazyness
          translateEnv(body),
          Typed.ExpressionRef(toIdenifier(result.id, result.loc), lookupType(result.id, body)),
          loc
        )
      case Variable(id, loc) =>
        Typed.ExpressionRef(toIdenifier(id, loc), lookupType(id, env), loc) // TODO: determine type
      case MacroCall(macroID, macroLoc, typeArgs, args, loc) =>
        Typed.ApplicationExpression(
          Typed.TypeApplicationExpression(
            Typed.ExpressionRef(toIdenifier(macroID, macroLoc), lookupType(macroID, env)), // TODO: determine type
            typeArgs.map(x => toType(x)).toList
          ),
          args.map(x => Typed.ExpressionRef(toIdenifier(x.id, x.loc), lookupType(x.id, env))).toList,
          loc
        )
      case BuiltInOperator(name, typeParameters, parameters, referenceImplementation, loc) => // TODO: suport reference implementation
        name match {
          case "true" | "false" => Typed.ApplicationExpression(Typed.TypeApplicationExpression(
            Typed.ExternExpression(Nil, Nil, Typed.InstatiatedType("Bool", Nil), name, loc), Nil), Nil)
          case "last" => Typed.ExternExpression(
            List(Identifier("A"), Identifier("B")),
            List(
              (Identifier("a"), TesslaAST.LazyEvaluation, Typed.InstatiatedType("Events", List(Typed.TypeParam(Identifier("A"))))),
              (Identifier("b"), TesslaAST.StrictEvaluation, Typed.InstatiatedType("Events", List(Typed.TypeParam(Identifier("B")))))
            ),
            Typed.TypeParam(Identifier("A")),
            "last"
          )
          case "delay" => Typed.ExternExpression(
            List(Identifier("A")),
            List(
              (Identifier("a"), TesslaAST.LazyEvaluation, Typed.InstatiatedType("Events", List(Typed.IntType))),
              (Identifier("b"), TesslaAST.StrictEvaluation, Typed.InstatiatedType("Events", List(Typed.TypeParam(Identifier("A")))))
            ),
            Typed.UnitType,
            "delay"
          )
          case "ite" => Typed.ExternExpression(
            List(Identifier("A")),
            List(
              (Identifier("a"), TesslaAST.StrictEvaluation, Typed.BoolType),
              (Identifier("b"), TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("A"))),
              (Identifier("c"), TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("A")))
            ),
            Typed.UnitType,
            "ite"
          )
          case _ => Typed.ExternExpression(typeParameters.map(toIdenifier(_, Location.unknown)).toList,
            parameters.map(x => (toIdenifier(x.id, x.loc), TesslaAST.StrictEvaluation, toType(x.parameterType))).toList,
            toType(definition.typeInfo.asInstanceOf[TypedTessla.FunctionType].returnType), name, loc)
        }
      case StaticIfThenElse(condition, thenCase, elseCase, loc) =>
        Typed.ApplicationExpression(Typed.TypeApplicationExpression(staticiteExtern, List(
          lookupType(thenCase.id, env)
        )), List(
          Typed.ExpressionRef(toIdenifier(condition.id, loc), lookupType(condition.id, env), loc),
          Typed.ExpressionRef(toIdenifier(thenCase.id, loc), lookupType(thenCase.id, env), loc),
          Typed.ExpressionRef(toIdenifier(elseCase.id, loc), lookupType(elseCase.id, env), loc)
        ), loc)
      case ObjectLiteral(members, loc) => Typed.RecordConstructorExpression(
        members.map(x => (Identifier(x._1), Typed.ExpressionRef(toIdenifier(x._2.id, x._2.loc), lookupType(x._2.id, env)))), loc)
      case MemberAccess(receiver, member, memberLoc, loc) =>
        Typed.RecordAccesorExpression(Identifier(member), Typed.ExpressionRef(toIdenifier(receiver.id, receiver.loc), lookupType(receiver.id, env)), loc)
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

  def toType(tpe: TypedTessla.Type, location: Location = Location.unknown): Typed.Type = tpe match {
    case TypedTessla.BuiltInType(name, typeArgs) => Typed.InstatiatedType(name, typeArgs.map(toType(_)).toList)
    case TypedTessla.FunctionType(typeParameters, parameterTypes, returnType, _) =>
      Typed.FunctionType(typeParameters.map(toIdenifier(_, Location.unknown)).toList,
        parameterTypes.map(x => (if (x.isStreamType) TesslaAST.LazyEvaluation else TesslaAST.StrictEvaluation, toType(x))).toList,
        toType(returnType))
    case TypedTessla.ObjectType(memberTypes, isOpen) =>
      if (isOpen) {
        throw new IllegalArgumentException("Open types not supported.")
      }
      Typed.RecordType(memberTypes.map(x => (Identifier(x._1), toType(x._2))))
    case TypedTessla.TypeParameter(id, loc) => Typed.TypeParam(toIdenifier(id, loc), loc)
  }

  def toIdenifier(identifier: TypedTessla.Identifier, location: Location) = Identifier(identifier.nameOpt.getOrElse("") + "$" + identifier.uid, location)
}

class TypedTessla2TesslaASTCore(baseTimeUnit: Option[TimeUnit]) extends TranslationPhase[TypedTessla.TypedSpecification, Typed.Specification] {
  override def translate(spec: TypedTessla.TypedSpecification) = {
    new TypedTessla2TesslaASTCoreWorker(spec, baseTimeUnit).translate()
  }
}