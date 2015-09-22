package de.uni_luebeck.isp.tessla

import scala.io.Source


object CompilerApp extends App {
  val source = if (args.length > 0) {
    Source.fromFile(args(0)).mkString
  } else {
    Source.stdin.mkString
  }
  val compiler = new Compiler(debug = true)
  val result = compiler.compile(Compiler.Source(source))
  result.get
}
