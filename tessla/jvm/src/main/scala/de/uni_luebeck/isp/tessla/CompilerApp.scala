package de.uni_luebeck.isp.tessla

import scala.io.Source
import de.uni_luebeck.isp.tessla.Compiler._
import de.uni_luebeck.isp.tessla.modules2.Module
import org.json4s.native.JsonMethods._

object CompilerApp extends App {
  val source = if (args.length > 0) {
    Source.fromFile(args(0)).mkString
  } else {
    Source.stdin.mkString
  }
  val compiler = new Compiler(debug = true)
  val result = compiler.compile(Compiler.Source(source))

  println("##############")
  result.get match {
    case ModuleList(list) => println(pretty(render(Module.json(list))))
    case _ => {}
  }
//  println("#############")
}
