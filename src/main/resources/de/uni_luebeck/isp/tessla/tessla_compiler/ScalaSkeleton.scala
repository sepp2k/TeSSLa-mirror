//This Scala code was automatically created by tessla-compiler from a TeSSLa Specification

import java.io.BufferedReader
import java.io.InputStreamReader

object Main {

    case class InputError(m: String, s: String) extends java.lang.Exception {
        override def toString: String = s"$m: $s"
    }
    case class ErrorContainer(errCode: Long) extends  java.lang.Exception

    def findEnd(s: String, delim: String, start: Int): Int = {
        if (start + delim.length <= s.length && s.substring(start, start + delim.length) == delim ) {
            start
        } else if (start >= s.length - 1) {
            s.length
        } else {
            val c = s.charAt(start)
            val newStart : Int = c match {
                case '(' if delim != "\"" => findEnd(s, ")", start + 1) + 1
                case '{' if delim != "\""=> findEnd(s, "}", start + 1) + 1
                case '\"' => findEnd(s, "\"", start + 1) + 1
                case '\\' if delim == "\"" => start + 2
                case _ => start + 1
            }
            findEnd(s, delim, newStart)
        }
    }

    def splitString(s: String, by: String) : Seq[String] = {
        var pos = 0;
        var parts: Seq[String] = Seq()
        while(pos < s.length) {
            val newPos = findEnd(s, by, pos)
            parts = parts :+ s.substring(pos, newPos)
            pos = newPos + by.length
        }

        parts
    }

    def processStringInput(s: String) : String = {
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

    def getErrorCode(err: Throwable) : Long = {
        err match {
            case ErrorContainer(errCode) => errCode
            case err: java.lang.ArithmeticException if err.getMessage == "/ by zero" => 1
            case _: java.util.NoSuchElementException => 2
            case _ => 2048
        }
    }

    def outputVar(output: String, trueName: String, errorCode: Long, ts: Long) : Unit = {
        var outputWithError: String = if (errorCode != 0)  "FATAL: " + trueName + " evaluation encountered an Error: " else ""

        errorCode match {
            case 0 =>  outputWithError = trueName + " = " + output
            case 1 => outputWithError += "Division by zero"
            case 2 => outputWithError += "Map access to non existing key"
            case _  => outputWithError += "Unknown error code " + errorCode
        }
        println(s"$ts: $outputWithError")
        if (errorCode != 0) System.exit(errorCode.toInt)
    }

    def main(args: Array[String]) : Unit = {

//STATIC

//VARDEF

        try {
            val br: BufferedReader = new BufferedReader(new InputStreamReader(System.in))

            var line: String = "";
            var inputEndReached: Boolean = false;

            while (true) {

                if ({line = br.readLine(); line != null}) {
                    val col = line.indexOf(':')
                    newInputTs = java.lang.Long.parseLong(line.substring(0, col).strip)
                    val rightPart : String = line.substring(col + 1, line.length).strip
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
                System.err.println("ERROR parsing the input Stream\n" + e)
                System.exit(129)
            }
        }
    }
}
