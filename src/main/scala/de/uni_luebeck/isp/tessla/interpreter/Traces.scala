package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.UnknownLoc
import de.uni_luebeck.isp.tessla.interpreter.Interpreter.InterpreterError

import scala.io.Source

object Traces {
  def feedInput(tesslaSpec: Interpreter, traceSource: Source): Unit = {
    def provide(streamName: String, value: Interpreter.Value) = {
      tesslaSpec.inStreams.get(streamName) match {
        case Some(inStream) => inStream.provide(value)
        case None => throw InterpreterError(s"Undeclared input stream: $streamName", UnknownLoc)
      }
    }

    val StringPattern = """^"([^"]*)"$""".r

    def parseValue(string: String) = string match {
      case "()" => Interpreter.UnitValue
      case "true" => Interpreter.BoolValue(true)
      case "false" => Interpreter.BoolValue(false)
      case StringPattern(s) => Interpreter.StringValue(s)
      case _ =>
        Interpreter.IntValue(BigInt(string))
    }

    var previousTS: BigInt = 0
    def handleInput(timestamp: String, inStream: String, value: Interpreter.Value = Interpreter.UnitValue) {
      val ts = BigInt(timestamp)
      if(ts < previousTS) sys.error("Decreasing time stamps")
      if(ts > previousTS) {
        tesslaSpec.step(ts - previousTS)
        previousTS = ts
      }
      provide(inStream, value)
    }

    val InputPattern = """(\d+)\s*:\s*([a-zA-Z][0-9a-zA-Z]*)(?:\s*=\s*(.+))?""".r
    val EmptyLinePattern = """\s*""".r

    traceSource.getLines.zipWithIndex.foreach {
      case (EmptyLinePattern(), _) =>
        // do nothing
      case (InputPattern(timestamp, inStream, null), _) =>
        handleInput(timestamp, inStream)
      case (InputPattern(timestamp, inStream, value), _) =>
        handleInput(timestamp, inStream, parseValue(value))
      case (line, index) =>
        sys.error(s"Syntax error on input line $index: $line")
    }
    tesslaSpec.step()
  }
}