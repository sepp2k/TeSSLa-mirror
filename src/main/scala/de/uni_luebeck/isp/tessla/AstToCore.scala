package de.uni_luebeck.isp.tessla

import scala.util.Try

class AstToCore {
    var counter = 0

    def mkId(name: String) = {
        counter += 1
        name + "$" + counter
    }

    type MacroEnv = Map[(String, Int), (Seq[Ast.MacroArg], Ast.Expr)]

    def translateSpec(spec: Ast.Spec) = {
        val macros: MacroEnv = spec.statements.collect {
            case Ast.Def(name, params, typeOpt, body, loc) if !params.isEmpty =>
                (name.name, params.size) -> (params -> body)
        }.toMap

        val streams: Seq[(String, TesslaCore.Expression)] = spec.statements.flatMap {
            case Ast.Def(name, Nil, typeOpt, body, loc) =>
                translateExpression(name.name, body, macros)
            case _ => List()
        }
        val inStreams = spec.statements.collect {
            case Ast.In(name, _, loc) => (name.name, loc)
        }
        val outStreams = spec.statements.collect {
            case Ast.Out(name, loc) => (name.name, loc)
        }
        TesslaCore.Specification(streams.toMap, inStreams, outStreams)
    }

    def translateExpression(name: String, exp: Ast.Expr, macros: MacroEnv): List[(String, TesslaCore.Expression)] = ???
}

object AstToCore extends CompilerPass[Ast.Spec, TesslaCore.Specification] {
    override def apply(compiler: Compiler, spec: Ast.Spec) = Try {
        new AstToCore().translateSpec(spec)
    }
}