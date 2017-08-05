package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{Types, UnknownLoc}
import de.uni_luebeck.isp.tessla.interpreter.Interpreter.InterpreterError

import scala.io.Source

object Traces {
  def feedInput(tesslaSpec: Interpreter, traceSource: Source, threshold: Option[BigInt]): Unit = {

    val queue = threshold match {
      case Some(n) => new TracesQueue(n)
      case None => new TracesQueue()
    }

    def provide(streamName: String, value: Interpreter.Value) = {
      tesslaSpec.inStreams.get(streamName) match {
        case Some((inStream, typ)) =>
          if (value.typ == typ) {
            inStream.provide(value)
          } else {
            throw InterpreterError(s"Tried to provide value of type ${value.typ} ($value) to input stream '$streamName' of type $typ", UnknownLoc)
          }
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
    def handleInput(timestamp: BigInt, inStream: String, value: Interpreter.Value = Interpreter.UnitValue) {
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