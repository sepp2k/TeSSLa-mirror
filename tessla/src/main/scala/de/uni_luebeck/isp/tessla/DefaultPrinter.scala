package de.uni_luebeck.isp.tessla

import util.Try

/**
  * A compiler pass that simply prints out the input
  */
object DefualtPrinter extends CompilerPass[Any, Any] {

  //TODO: implement some logic to choose alternative output methods
  //var outputStream = ...
  //def toStdOut = {this.outputStream = sys.stdout....}
  //def toStdErr = ...
  //def toFile(name: String) = ...

  override def applyPass(in: Any): Try[Any] = {
    Try(println(in))
  }
}
