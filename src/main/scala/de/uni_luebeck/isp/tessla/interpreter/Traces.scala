package de.uni_luebeck.isp.tessla.interpreter

import de.uni_luebeck.isp.tessla.{Types, UnknownLoc}
import de.uni_luebeck.isp.tessla.interpreter.Interpreter.InterpreterError

import scala.io.Source

object Traces {
  var queue: List[(BigInt, String, Option[Interpreter.Value])] = Nil
  var counter = 0
  def feedInput(tesslaSpec: Interpreter, traceSource: Source): Unit = {
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
      //provide(inStream, value)
      val ts = timestamp
      if(ts < previousTS) sys.error("Decreasing time stamps")
      if(ts > previousTS) {
        tesslaSpec.step(ts - previousTS)
        previousTS = ts
      }
      println("provide" + inStream + " " + value)

      provide(inStream, value)
    }

    val InputPattern = """(\d+)\s*:\s*([a-zA-Z][0-9a-zA-Z]*)(?:\s*=\s*(.+))?""".r
    val EmptyLinePattern = """\s*""".r

    val threshold = 0 // Diff between timestamps, should be a positive value (nanoseconds)

    //handles every event from the queue where the difference between the newest timestamp and the
    // timestamp of the first element of the queue is greater or equals the threshold
    def removeFromQueue(timeStamp: String): Unit = {
      val time = BigInt(timeStamp)

      if (queue != Nil && time >= queue.head._1 + threshold){
        //println("Head und Time: " +queue.head._1 + " " + time)
        queue.head match {
          case (ts, n, Some(v)) =>
            handleInput(ts, n, v)
          case (ts, n, None) => handleInput(ts, n)
        }
        queue = queue.tail
        removeFromQueue(timeStamp)
      }
    }

    //handles every event from the queue
    def flush(): Unit = {
      queue.foreach{
        case (ts, n, Some(v)) =>
          //println("ts" + ts)
          handleInput(ts, n, v)
        case (ts, n, None) =>
          //println("ts" + ts)
          handleInput(ts, n)
      }
      queue = Nil
    }


    //puts a new event in the queue, using the timestamp as the priority
    def enqueue(timeStamp: BigInt, inStream: String, value: Option[String] = None): Unit = {
      val subqueues = queue.span{case (t,s,v) => t <= timeStamp}
      queue = (subqueues._1:+(timeStamp, inStream, value.map(parseValue)))++subqueues._2
      //println(queue.mkString("\n"))
    }


    traceSource.getLines.zipWithIndex.foreach {
      case (EmptyLinePattern(), _) =>
        // do nothing
      case (InputPattern(timestamp, inStream, null), _) => {
        enqueue(BigInt(timestamp), inStream)
        removeFromQueue(timestamp)
      }
      case (InputPattern(timestamp, inStream, value), _) => {
        enqueue(BigInt(timestamp), inStream, Some(value))
        removeFromQueue(timestamp)
      }
      case (line, index) =>
        sys.error(s"Syntax error on input line $index: $line")
    }

    //in the end handle every event from the queue

    flush()
    tesslaSpec.step()
  }
}