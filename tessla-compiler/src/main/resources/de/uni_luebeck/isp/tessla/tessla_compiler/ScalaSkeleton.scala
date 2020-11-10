/*

 */

//This Scala code was automatically created by tessla-compiler from a TeSSLa Specification

import java.io.BufferedReader
import java.io.InputStreamReader

object Main {

  case class InputError(m: String, s: String) extends java.lang.Exception {
    override def getMessage: String = s"$m: $s"
  }

  case class UnknownEventError(base: Throwable) extends java.lang.Exception {
    override def getMessage: String =
      "Uncertainty concerning the existence of an event" +
        (if (base != null)
           s" due to the follwing base error:\n ${base.getMessage}"
         else
           ".")

  }

  object EOSome {
    def apply[A](value: => A): ErrorOption[A] = {
      try {
        ErrorOption[A](Some(value), None)
      } catch {
        case t: Throwable => ErrorOption[A](None, Some(t))
      }
    }
  }

  object EONone {
    def apply[A](): ErrorOption[A] = ErrorOption[A](None, None)
  }

  case class ErrorOption[A](value: Option[A], error: Option[Throwable]) {
    def get: A = if (value.isDefined) value.get else throw error.get

    def isDefined: Boolean = value.isDefined || error.isDefined

    def isEmpty: Boolean = value.isEmpty && error.isEmpty
  }

  def findEnd(s: String, delim: String, start: Int): Int = {
    if (start + delim.length <= s.length && s.substring(start, start + delim.length) == delim) {
      start
    } else if (start >= s.length - 1) {
      s.length
    } else {
      val c = s.charAt(start)
      val newStart: Int = c match {
        case '(' if delim != "\""  => findEnd(s, ")", start + 1) + 1
        case '{' if delim != "\""  => findEnd(s, "}", start + 1) + 1
        case '\"'                  => findEnd(s, "\"", start + 1) + 1
        case '\\' if delim == "\"" => start + 2
        case _                     => start + 1
      }
      findEnd(s, delim, newStart)
    }
  }

  def splitString(s: String, by: String): Seq[String] = {
    var pos = 0;
    var parts: Seq[String] = Seq()
    while (pos < s.length) {
      val newPos = findEnd(s, by, pos)
      parts = parts :+ s.substring(pos, newPos)
      pos = newPos + by.length
    }

    parts
  }

  def processStringInput(s: String): String = {
    if (s.charAt(0) == '\"' && findEnd(s, "\"", 1) == s.length - 1) {
      s.substring(1, s.length - 1)
        .replaceAll("\\\\n", "\n")
        .replaceAll("\\\\r", "\r")
        .replaceAll("\\\\\"", "\"")
        .replaceAll("\\\\\\\\", "\\")
        .replaceAll("\\\\t", "\t");
    } else {
      throw InputError("No valid string input", s)
    }
  }

  def outputVar(output: String, trueName: String, error: Throwable, ts: Long, raw: Boolean): Unit = {
    if (error != null) {
      System.err.println(s"$ts: FATAL: $trueName evaluation encountered an Error:\n ${error.getMessage}")
      System.exit(1)
    } else {
      val prefix = if (raw) "" else s"$ts: $trueName = "
      println(s"$prefix$output")
    }
  }

  def main(args: Array[String]): Unit = {

//STATIC

//VARDEF

    try {
      val br: BufferedReader = new BufferedReader(new InputStreamReader(System.in))

      var line: String = "";
      var inputEndReached: Boolean = false;

      while (true) {

        if ({ line = br.readLine(); line != null }) {
          val col = line.indexOf(':')
          newInputTs = java.lang.Long.parseLong(line.substring(0, col).strip)
          val rightPart: String = line.substring(col + 1, line.length).strip
          val eq = rightPart.indexOf('=')
          if (eq != -1) {
            inputStream = rightPart.substring(0, eq).strip
            value = rightPart.substring(eq + 1, rightPart.length).strip
          } else {
            inputStream = rightPart
            value = ""
          }
        } else {
          inputEndReached = true
        }

        if (newInputTs > currTs || inputEndReached) {
          if (inputEndReached) {
            newInputTs += 1
          }

          var doProcessing = true
          while (doProcessing) {

//TRIGGER

            if (currTs == newInputTs) {
              doProcessing = false
            } else {

//STEP

//TAIL

              lastProcessedTs = currTs
              currTs = newInputTs
            }
          }
          if (inputEndReached) {
            System.exit(0)
          }
        } else if (newInputTs < currTs) {
          System.err.println(s"$currTs: FATAL: decreasing timestamp received")
          System.exit(128)
        }
//INPUTPROCESSING
      }
    } catch {
      case e: Exception => {
        System.err.println("ERROR parsing the input Stream\n " + e)
        System.exit(129)
      }
    }
  }
}
