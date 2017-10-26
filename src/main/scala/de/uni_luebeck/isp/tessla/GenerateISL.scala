package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Tessla.Expression
import de.uni_luebeck.isp.tessla.TranslationPhase.{Failure, Success}

object GenerateISL {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: generate-isl spec.tessla")
      sys.exit(1)
    }
    val tesslaFile = args(0)
    TesslaParser.translateSpec(TesslaSource.fromFile(tesslaFile)) match {
      case Success(ast, _) =>
        generateForSpec(ast)
      case Failure(_, _) =>
        System.err.println("Parsing failed")
    }
  }

  def generateForSpec(spec: Tessla.Spec): Unit = {
    spec.statements.foreach(generateForStatement)
  }

  def generateForStatement(statement: Tessla.Statement): Unit = {
    statement match {
      case Tessla.Definition(_, _, _, body, _) =>
        generateForExpr(body)
      case Tessla.Out(expr, _, _) =>
        generateForExpr(expr)
      case _ =>
        // Do nothing
    }
  }

  def generateForExpr(expr: Tessla.Expression): Unit = {
    expr match {
      case Tessla.MacroCall(id, args, _) =>
        generateForMacro(id, args)
      case _ =>
        // loop back
    }
  }

  def generateForMacro(id: Tessla.Identifier, args: Seq[Tessla.Argument] ): Seq[String] = {
    id match {
      case Tessla.Identifier("code_line_exec",_) => List(s"line_reached:${args[0].toString}")
    }
  }
}
