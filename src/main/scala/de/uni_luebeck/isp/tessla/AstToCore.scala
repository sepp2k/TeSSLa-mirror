package de.uni_luebeck.isp.tessla

import scala.util.Try

class AstToCore {
    var counter = 0

    def mkId(name: String) = {
        counter += 1
        name + "$" + counter
    }

    sealed abstract class Value
    case class Literal(value: TesslaCore.LiteralValue) extends Value
    case class Macro(f: (String, Seq[Ast.MacroArg], Env, SourceLoc) => (List[(String, TesslaCore.Expression)], Env)) extends Value

    sealed abstract class Type
    case class StreamType(elementType: PrimitiveType) extends Type
    sealed abstract class PrimitiveType
    case object IntType extends PrimitiveType
    case object BoolType extends PrimitiveType
    case object UnitType extends PrimitiveType

    def typeOfLiteral(lit: TesslaCore.LiteralValue): PrimitiveType = lit match {
        case TesslaCore.IntLiteral(_,_) => IntType
        case TesslaCore.BoolLiteral(_,_) => BoolType
        case TesslaCore.Unit(_) => UnitType
    }

    case class Env(
        values: Map[(String, Int), Value],
        types: Map[String, Type]
    ) {
        def addConstant(name: String, value: TesslaCore.LiteralValue) = {
            Env(values + ((name, 0) -> Literal(value)), types)
        }
    }

    val builtIns = Map(
        ("nil", 0) -> Macro((name, _ , env, loc) => (List(name -> TesslaCore.Nil(loc)), env))
    )

    def translateSpec(spec: Ast.Spec) = {
        val macros = spec.statements.collect {
            case Ast.Def(name, params, typeOpt, body, loc) if !params.isEmpty =>
                (name.name, params.size) -> Macro((targetName, args, env, _) => translateExpression(targetName, body, env))
        }.toMap
        val globalEnv = Env(builtIns ++ macros, Map())

        val streams: Seq[(String, TesslaCore.Expression)] = spec.statements.scanLeft((List[(String, TesslaCore.Expression)](), globalEnv)) {
            case ((_, env), Ast.Def(name, Nil, typeOpt, body, loc)) =>
                translateExpression(name.name, body, env)
            case ((_, env), _) => (List(), env)
        }.map(_._1).flatten
        val inStreams = spec.statements.collect {
            case Ast.In(name, _, loc) => (name.name, loc)
        }
        val outStreams = spec.statements.collect {
            case Ast.Out(name, loc) => (name.name, loc)
        }
        TesslaCore.Specification(streams.toMap, inStreams, outStreams)
    }

    def translateExpression(name: String, exp: Ast.Expr, env: Env): (List[(String, TesslaCore.Expression)], Env) = exp match {
        case Ast.ExprIntLit(Ast.IntLit(value, loc)) => (List(), env.addConstant(name, TesslaCore.IntLiteral(value, loc)))
        case Ast.ExprBoolLit(Ast.BoolLit(value, loc)) => (List(), env.addConstant(name, TesslaCore.BoolLiteral(value, loc)))
        case Ast.ExprName(Ast.Identifier(id, loc)) =>
            env.values.get((id, 0)) match {
                case Some(Macro(f)) => f(name, Seq(), env, loc)
                case Some(Literal(lit)) => (List(), env.addConstant(name, lit))
                case None => (List(name -> TesslaCore.Var(id, loc)), env)
            }
    }
}

object AstToCore extends CompilerPass[Ast.Spec, TesslaCore.Specification] {
    override def apply(compiler: Compiler, spec: Ast.Spec) = Try {
        new AstToCore().translateSpec(spec)
    }
}