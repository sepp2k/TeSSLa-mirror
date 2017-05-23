package de.uni_luebeck.isp.tessla.interpreter

import scala.io.Source

object Main {
  def main(args: Array[String]) = {
    val (tesslaFile, traceSource) = args match {
      case Array(tesslaFile, traceFile) => (tesslaFile, Source.fromFile(traceFile))
      case Array(tesslaFile) => (tesslaFile, Source.stdin)
      case _ =>
        System.err.println("Usage: tessla-interpreter tessla-file [trace-file]")
        sys.exit(1)
    }
    val tesslaSpec = Interpreter.fromFile(tesslaFile)
    tesslaSpec.outStreams.foreach { case (name, stream) => tesslaSpec.printStream(stream, name) }

    def provide(streamName: String, value: tesslaSpec.PrimValue) = {
      tesslaSpec.inStreams.get(streamName) match {
        case Some(inStream) => inStream.provide(value)
        case None => sys.error(s"Undeclared input stream: $streamName")
      }
    }

    def parseValue(string: String) = string match {
      case "()" => tesslaSpec.UnitValue
      case "true" => tesslaSpec.BoolValue(true)
      case "false" => tesslaSpec.BoolValue(false)
      case _ =>
        tesslaSpec.IntValue(BigInt(string))
    }

    var previousTS: BigInt = 0
    traceSource.getLines.map(_.split("\\s+")).foreach {
      case Array(timestamp, inStream, value) =>
        val ts = BigInt(timestamp)
        if(ts < previousTS) sys.error("Decreasing time stamps")
        if(ts > previousTS) {
          tesslaSpec.step(ts - previousTS)
          previousTS = ts
        }
        provide(inStream, parseValue(value));
    }

    tesslaSpec.step()
  }
}
