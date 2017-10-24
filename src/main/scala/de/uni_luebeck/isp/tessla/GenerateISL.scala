package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}

object GenerateISL {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: generate-isl spec.tessla")
      sys.exit(1)
    }
    val tesslaFile = args(0)
    TesslaParser.translate(TesslaSource.fromFile(tesslaFile)) match {
      case Success(ast, _) =>
        generateForSpec(ast)
      case Failure(_, _) =>
        System.err.println("Parsing failed")
    }
  }

  def generateForSpec(spec: Ast.Spec): Unit = {
    spec.statements.foreach(generateForStatement)
  }

  def generateForStatement(statement: Ast.Statement): Unit = {
    statement match {
      case Ast.Def(_, _, _, body, _) =>
        generateForExpr(body)
      case Ast.Out(expr, _, _) =>
        generateForExpr(expr)
      case _ =>
        // Do nothing
    }
  }

  def generateForExpr(expr: Ast.Expr): Unit = {
    expr match {
      case Ast.MacroArg(name, typeAscr) =>
        generateForMacro(name, typeAscr)
      case _ =>
        // loop back
    }
  }

  def generateForMacro(name: Ast.Identifier, typeAscr: Option[Ast.Type]): Unit = {
    name match {
      case Ast.Identifier("")
    }
  }
}
