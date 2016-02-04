package de.uni_luebeck.isp.tessla

import scala.io.Source

object CompilerApp extends App {
  val source = if (args.length > 0) {
    Source.fromFile(args(0))
  } else {
    Source.stdin
  }

  val compiler = new Compiler

  def flushDiags() {
    for (diag <- compiler.diagnostics) {
      println(diag)
      diag.printStackTrace()
    }
    compiler.diagnostics = Seq()
  }

  // TODO move this logic into the compiler
  println("parsing")
  val ast = Parser(compiler, new TesslaSource(source)).get
  flushDiags()
  println(ast)

  println("extracting definitions")
  val defs = DefExtractor(compiler, ast).get
  flushDiags()
  println(defs)
}