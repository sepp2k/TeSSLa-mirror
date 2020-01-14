package de.uni_luebeck.isp.tessla

import TesslaAST.Typed
import Typed.Identifier
import cats.data.Ior
import de.uni_luebeck.isp.tessla
import de.uni_luebeck.isp.tessla.Errors.UndefinedTimeUnit
import de.uni_luebeck.isp.tessla.Tessla.{FloatLiteral, IntLiteral, StringLiteral, TimeLiteral}
import de.uni_luebeck.isp.tessla.TypedTessla.{BuiltInOperator, InputStream, Literal, Macro, MacroCall, MemberAccess, ObjectLiteral, Parameter, StaticIfThenElse, Variable, VariableEntry}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

class TypedTessla2TesslaASTTypedWorker(spec: TypedTessla.TypedSpecification, baseTimeUnit: Option[TimeUnit]) extends TranslationPhase.Translator[Typed.Specification] {

  val staticiteExtern = Typed.ExternExpression(List(Identifier("A")), List(
    (TesslaAST.StrictEvaluation, Typed.BoolType),
    (TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("A"))),
    (TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("A")))
  ), Typed.TypeParam(Identifier("A")), "staticite")

  // TODO: add all folds
  val knownExterns = Map(
    "true" -> Typed.ApplicationExpression(Typed.TypeApplicationExpression(
      Typed.ExternExpression(Nil, Nil, Typed.InstatiatedType("Bool", Nil), "true"), Nil), ArraySeq()),
    "false" -> Typed.ApplicationExpression(Typed.TypeApplicationExpression(
      Typed.ExternExpression(Nil, Nil, Typed.InstatiatedType("Bool", Nil), "false"), Nil), ArraySeq()),
    "last" -> Typed.ExternExpression(
      List(Identifier("A"), Identifier("B")),
      List(
        (TesslaAST.LazyEvaluation, Typed.InstatiatedType("Events", List(Typed.TypeParam(Identifier("A"))))),
        (TesslaAST.StrictEvaluation, Typed.InstatiatedType("Events", List(Typed.TypeParam(Identifier("B")))))
      ),
      Typed.InstatiatedType("Events", List(Typed.TypeParam(Identifier("A")))),
      "last"
    ),
    "delay" -> Typed.ExternExpression(
      List(Identifier("A")),
      List(
        (TesslaAST.LazyEvaluation, Typed.InstatiatedType("Events", List(Typed.IntType))),
        (TesslaAST.StrictEvaluation, Typed.InstatiatedType("Events", List(Typed.TypeParam(Identifier("A")))))
      ),
      Typed.InstatiatedType("Events", List(Typed.UnitType)),
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
        (TesslaAST.StrictEvaluation, Typed.InstatiatedType("List", List(Typed.TypeParam(Identifier("A"))))),
        (TesslaAST.LazyEvaluation, Typed.TypeParam(Identifier("B"))),
        (TesslaAST.StrictEvaluation, Typed.FunctionType(
          Nil, List(
            (TesslaAST.StrictEvaluation, Typed.TypeParam(Identifier("B"))),
            (TesslaAST.StrictEvaluation, Typed.TypeParam(Identifier("A")))),
          Typed.TypeParam(Identifier("B"))
        ))
      ),
      Typed.TypeParam(Identifier("B")),
      "List_fold")
  )
  val ins = mutable.Map[Typed.Identifier, (Typed.Type, List[Nothing])]()

  override protected def translateSpec() = {
    val outs = spec.outStreams.map(x => (toIdenifier(x.id, x.loc), x.nameOpt, Nil)).toList
    val defs: Map[Typed.Identifier, Typed.ExpressionArg] = translateEnv(spec.globalDefs)

    Typed.Specification(ins.toMap, defs, outs, defs.flatMap(_._1.idOrName.right).max)
  }

  def lookupType(id: TypedTessla.Identifier, env: TypedTessla.Definitions): Typed.Type = {
    val value = lookup(env, id)
    value.expression match {
      case TypedTessla.MemberAccess(receiver, member, _, _) =>
        lookupType(receiver.id, env).asInstanceOf[Typed.RecordType].entries(TesslaAST.Name(member))
      case TypedTessla.BuiltInOperator(name, _, _, _, _) =>
        knownExterns.get(name).map(_.tpe).getOrElse(toType(value.typeInfo))
      case TypedTessla.ObjectLiteral(members, _) =>
        Typed.RecordType(members.map(x => (TesslaAST.Name(x._1) -> lookupType(x._2.id, env))))
      case _ => toType(value.typeInfo)
    }
  }

  def lookupType2(id: TypedTessla.Identifier, env: TypedTessla.Definitions): Typed.Type = env.variables.get(id) match {
    case Some(value) => toType(value.typeInfo)
    case None => lookupType2(id, env.parent.get)
  }

  def translateEnv(env: TypedTessla.Definitions): Map[Typed.Identifier, Typed.ExpressionArg] = env.variables.toMap.map { case (id, definition) =>
    (toIdenifier(id, definition.loc), definition.expression match {
      case InputStream(name, streamType, typeLoc, loc) =>
        ins += (Typed.Identifier(Ior.Left(name), loc) -> (toType(streamType), Nil))
        Typed.ExpressionRef(Typed.Identifier(Ior.Left(name), loc), toType(streamType, typeLoc), loc)
      case Parameter(param, parameterType, id) =>
        Typed.ExpressionRef(Typed.Identifier(Ior.Left(param.id.name), param.id.loc), toType(parameterType), param.loc)
      case Macro(typeParameters, parameters, body, returnType, headerLoc, result, loc, _) =>
        Typed.FunctionExpression(
          typeParameters.map(toIdenifier(_, Location.unknown)).toList,
          parameters.map(x => (Typed.Identifier(Ior.Left(x.param.id.name), x.loc), if (x.parameterType.isStreamType) TesslaAST.LazyEvaluation else TesslaAST.StrictEvaluation,
            toType(x.parameterType))).toList,
          translateEnv(body),
          Typed.ExpressionRef(toIdenifier(result.id, Location.unknown), lookupType(result.id, body), result.loc),
          loc
        )
      case Variable(id, loc) =>
        Typed.ExpressionRef(toIdenifier(id, lookup(env, id).loc), lookupType(id, env), loc)
      case MacroCall(macroID, macroLoc, typeArgs, args, loc) =>
        Typed.ApplicationExpression(
          Typed.TypeApplicationExpression(
            Typed.ExpressionRef(toIdenifier(macroID, macroLoc), lookupType(macroID, env)),
            typeArgs.map(x => toType(x)).toList
          ),
          args.map(x => Typed.ExpressionRef(toIdenifier(x.id, lookup(env, x.id).loc), lookupType(x.id, env), x.loc)).to(ArraySeq),
          loc
        )
      case BuiltInOperator(name, typeParameters, parameters, referenceImplementation, loc) => // TODO: suport reference implementation
        knownExterns.getOrElse(name,
          Typed.ExternExpression(typeParameters.map(toIdenifier(_, Location.unknown)).toList,
            parameters.map(x => (TesslaAST.StrictEvaluation, toType(x.parameterType))).toList,
            toType(definition.typeInfo.asInstanceOf[TypedTessla.FunctionType].returnType), name, loc))
      case StaticIfThenElse(condition, thenCase, elseCase, loc) =>
        Typed.ApplicationExpression(Typed.TypeApplicationExpression(staticiteExtern, List(
          lookupType(thenCase.id, env)
        )), ArraySeq(
          Typed.ExpressionRef(toIdenifier(condition.id, loc), lookupType(condition.id, env), loc),
          Typed.ExpressionRef(toIdenifier(thenCase.id, loc), lookupType(thenCase.id, env), loc),
          Typed.ExpressionRef(toIdenifier(elseCase.id, loc), lookupType(elseCase.id, env), loc)
        ), loc)
      case ObjectLiteral(members, loc) => Typed.RecordConstructorExpression(
        members.map(x => (TesslaAST.Name(x._1), Typed.ExpressionRef(toIdenifier(x._2.id, x._2.loc), lookupType(x._2.id, env)))), loc)
      case MemberAccess(receiver, member, memberLoc, loc) =>
        Typed.RecordAccesorExpression(TesslaAST.Name(member), Typed.ExpressionRef(toIdenifier(receiver.id, receiver.loc), lookupType(receiver.id, env)), loc)
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
      Typed.RecordType(memberTypes.map(x => (TesslaAST.Name(x._1), toType(x._2))))
    case TypedTessla.TypeParameter(id, loc) => Typed.TypeParam(toIdenifier(id, loc), loc)
  }

  def lookup(env: TypedTessla.Definitions, id: TypedTessla.Identifier): VariableEntry =
    env.variables.getOrElse(id, lookup(env.parent.get, id))

  def toIdenifier(identifier: TypedTessla.Identifier, location: Location) = Typed.Identifier(Ior.fromOptions(identifier.nameOpt, Some(identifier.uid)).get, location)

}

class TypedTessla2TesslaASTCore(baseTimeUnit: Option[TimeUnit]) extends TranslationPhase[TypedTessla.TypedSpecification, Typed.Specification] {
  override def translate(spec: TypedTessla.TypedSpecification) = {
    new TypedTessla2TesslaASTTypedWorker(spec, baseTimeUnit).translate()
  }
}