package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.interpreter.TimeUnit.Nanos
import de.uni_luebeck.isp.tessla.{CompilationError, TesslaCore, Types, UnknownLoc}

import scala.io.Source

object Traces {
  case class InvalidInputError(message: String) extends CompilationError {
    def loc = UnknownLoc
  }
  def feedInput(tesslaSpec: Interpreter, traceSource: Source, threshold: BigInt): Unit = {
    val queue = new TracesQueue(threshold)
    var timeUnit: TimeUnit.Unit = Nanos

    def provide(streamName: String, value: TesslaCore.Value) = {
      tesslaSpec.inStreams.get(streamName) match {
        case Some((inStream, typ)) =>
          if (value.typ == typ) {
            inStream.provide(value)
          } else {
            throw InvalidInputError(s"Tried to provide value of type ${value.typ} ($value) to input stream '$streamName' of type $typ")
          }
        case None => throw InvalidInputError(s"Undeclared input stream: $streamName")
      }
    }

    val StringPattern = """^"([^"]*)"$""".r

    def parseValue(string: String) = string match {
      case "()" => TesslaCore.Unit(UnknownLoc)
      case "true" => TesslaCore.BoolLiteral(true, UnknownLoc)
      case "false" => TesslaCore.BoolLiteral(false, UnknownLoc)
      case StringPattern(s) => TesslaCore.StringLiteral(s, UnknownLoc)
      case _ =>
        TesslaCore.IntLiteral(BigInt(string), UnknownLoc)
    }

    var previousTS: BigInt = 0

    def handleInput(timestamp: BigInt, inStream: String, value: TesslaCore.Value = TesslaCore.Unit(UnknownLoc)) {
      val ts = timestamp
      if(ts < previousTS) sys.error("Decreasing time stamps: first = " + previousTS + " , second = " + ts)
      if(ts > previousTS) {
        tesslaSpec.step(ts - previousTS)
        previousTS = ts
      }
      provide(inStream, value)
    }

    val InputPattern = """(\d+)\s*:\s*([a-zA-Z][0-9a-zA-Z]*)(?:\s*=\s*(.+))?""".r
    val EmptyLinePattern = """\s*""".r
    val TimeUnitPattern = """\$timeunit\s*=\s*([a-zA-Z]{1,2})""".r

    def dequeue(timeStamp: String): Unit = {
      while(queue.hasNext(BigInt(timeStamp))) {
        queue.dequeue(BigInt(timeStamp)) match {
          case Some((ts, (n, v))) => handleInput(ts, n, v)
          case None =>
        }
      }
    }


    traceSource.getLines.zipWithIndex.foreach {
      case (EmptyLinePattern(), _) =>
        // do nothing
      case (InputPattern(timestamp, inStream, null), _) => {
        queue.enqueue(BigInt(timestamp), inStream)
        dequeue(timestamp)
      }
      case (InputPattern(timestamp, inStream, value), _) => {
        queue.enqueue(BigInt(timestamp), inStream, parseValue(value))
        dequeue(timestamp)
      }
      case (TimeUnitPattern(unit), _) =>
        timeUnit = TimeUnit.fromString(unit)
      case (line, index) =>
        sys.error(s"Syntax error on input line $index: $line")
    }

    //in the end handle every remaining event from the queue
    queue.toList().foreach{
      case (ts, (n, v)) =>
        handleInput(ts, n, v)
    }

    tesslaSpec.step()
  }
}