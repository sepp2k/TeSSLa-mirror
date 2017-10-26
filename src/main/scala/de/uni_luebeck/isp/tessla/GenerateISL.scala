package de.uni_luebeck.isp.tessla

object GenerateISL {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println("Usage: generate-isl spec.tessla")
      sys.exit(1)
    }
    val tesslaFile = args(0)
    val ast = new TesslaParser().translateSpec(TesslaSource.fromFile(tesslaFile))
    println(generateForSpec(ast))
  }

  def generateForSpec(spec: Tessla.Spec): String = {
    spec.statements.flatMap(generateForStatement).mkString("\n")
  }

  def generateForStatement(statement: Tessla.Statement): Seq[String] = {
    statement match {
      case Tessla.Definition(_, _, _, body, _) =>
        generateForExpr(body)
      case Tessla.Out(expr, _, _) =>
        generateForExpr(expr)
      case _ =>
        Seq()
    }
  }

  def generateForExpr(expr: Tessla.Expression): Seq[String] = {
    expr match {
      case Tessla.MacroCall(id, args, _) =>
        generateForMacro(id, args) ++ args.flatMap {
          case Tessla.NamedArgument(_, argExpr) =>
            generateForExpr(argExpr)
          case Tessla.PositionalArgument(argExpr) =>
            generateForExpr(argExpr)
        }
      case Tessla.TypeAssertion(exprArg, _) =>
        generateForExpr(exprArg)
      case Tessla.Block(defs, exprArg,_) =>
        generateForExpr(exprArg) ++ defs.flatMap{
          case Tessla.Definition(_, _, _, body, _) =>
            generateForExpr(body)
        }
      case _ =>
        Seq()
    }
  }


  def generateForMacro(id: Tessla.Identifier, args: Seq[Tessla.Argument] ): Seq[String] = {
    id match {
      case Tessla.Identifier("code_line_exec",_) =>
        args match {
          case Seq(Tessla.PositionalArgument(Tessla.IntLiteral(value, _))) =>
            Seq(s"if [line_reached:${value}] then line_reached fi")
          case _ =>
            Seq("line_reached")
        }
      case Tessla.Identifier("function_call",_) =>
        args match {
          // This wont work because we are already in that function when functions evaluates
          //case Seq(Tessla.PositionalArgument(Tessla.StringLiteral(value, _))) =>
          //  Seq(s"if [functions:${value}] then function_call fi")
          case _ =>
            Seq("function_call")
        }
      case Tessla.Identifier("function_return",_) =>
        args match {
          case Seq(Tessla.PositionalArgument(Tessla.IntLiteral(value, _))) =>
            Seq(s"if &([functions:${value}], [opcodes:ret]) then functions opcodes fi")
          case _ =>
            Seq("line_reached")
        }
      case _ =>
        Seq()
    }
  }
}
