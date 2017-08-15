package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.TimeUnit
import de.uni_luebeck.isp.tessla.{CompilationError, TesslaCore, UnknownLoc}

import scala.io.Source
import scala.util.matching.Regex


object Traces {
  val TimeUnitPattern = """\$timeunit\s*=\s*("[a-zA-Z]{1,2}")""".r
  val InputPattern = """(\d+)\s*:\s*([a-zA-Z][0-9a-zA-Z]*)(?:\s*=\s*(.+))?""".r
  val EmptyLinePattern = """\s*""".r

  val StringPattern = """^"([^"]*)"$""".r

  def parseValue(string: String): TesslaCore.LiteralValue = string match {
    case "()" => TesslaCore.Unit(UnknownLoc)
    case "true" => TesslaCore.BoolLiteral(true, UnknownLoc)
    case "false" => TesslaCore.BoolLiteral(false, UnknownLoc)
    case StringPattern(s) => TesslaCore.StringLiteral(s, UnknownLoc)
    case _ =>
      TesslaCore.IntLiteral(BigInt(string), UnknownLoc)
  }

  def read(traceSource: Source, callback: (Option[BigInt], String, TesslaCore.Value) => Unit): Traces = {
    def getValues(src: Seq[String]) = src.zipWithIndex.filterNot {
      case (line, _) => EmptyLinePattern.pattern.matcher(line).matches()
    }.map {
      case (InputPattern(timestamp, inStream, null), _) =>
        (BigInt(timestamp), inStream, TesslaCore.Unit(UnknownLoc))
      case (InputPattern(timestamp, inStream, value), _) =>
        (BigInt(timestamp), inStream, parseValue(value))
      case (line, index) =>
        sys.error(s"Syntax error on input line $index: $line")
    }

    traceSource.getLines().take(1).toList.headOption match {
      case Some(TimeUnitPattern(u)) =>
        val timeUnit = TimeUnit.fromString(u)
        callback(None, "$timeunit",
          TesslaCore.StringLiteral(timeUnit.toString, UnknownLoc))
        new Traces(timeUnit, getValues(traceSource.getLines().toSeq), callback)
      case Some(v) =>
        new Traces(TimeUnit.Nanos, getValues(Seq(v) ++ traceSource.getLines()), callback)
      case None =>
        new Traces(TimeUnit.Nanos, Seq(), callback)
    }
  }
}

class Traces(val timeStampUnit: TimeUnit.Unit, values: => Seq[(BigInt, String, TesslaCore.LiteralValue)], val callback: (Option[BigInt], String, TesslaCore.Value) => Unit) {

  case class InvalidInputError(message: String) extends CompilationError {
    def loc = UnknownLoc
  }

  def feedInput(tesslaSpec: Interpreter, threshold: BigInt): Unit = {
    val queue = new TracesQueue(threshold)

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

    var previousTS: BigInt = 0

    def handleInput(timestamp: BigInt, inStream: String, value: TesslaCore.Value = TesslaCore.Unit(UnknownLoc)) {
      val ts = timestamp
      if (ts < previousTS) sys.error("Decreasing time stamps: first = " + previousTS + " , second = " + ts)
      if (ts > previousTS) {
        tesslaSpec.step(ts - previousTS)
        previousTS = ts
      }
      provide(inStream, value)
    }

    def dequeue(timeStamp: BigInt): Unit = {
      while (queue.hasNext(timeStamp)) {
        queue.dequeue(timeStamp) match {
          case Some((ts, (n, v))) => handleInput(ts, n, v)
          case None =>
        }
      }
    }

    tesslaSpec.outStreams.foreach {
      case (name, stream) => stream.addListener {
        case Some(value) => callback(Some(tesslaSpec.getTime), name, value)
        case None =>
      }
    }

    values.foreach {
      case (ts, inStream, value) =>
        queue.enqueue(ts, inStream, value)
        dequeue(ts)
    }

    //in the end handle every remaining event from the queue
    queue.toList().foreach {
      case (ts, (n, v)) =>
        handleInput(ts, n, v)
    }

    tesslaSpec.step()
  }
}
