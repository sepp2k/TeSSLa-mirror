//This Scala code was automatically created by tessla-compiler from a TeSSLa Specification

import util.control.Breaks._
import java.io.BufferedReader
import java.io.InputStreamReader

object Main {

    def getErrorCode(err: Throwable) : Long = {
        err match {
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

        var lastProcessedTs: Long = 0
//VARDEF

        try {
            var newInputTs: Long = 0;

            val br: BufferedReader = new BufferedReader(new InputStreamReader(System.in))

            var line: String = "";
            var inputEndReached: Boolean = false;

            while (true) breakable {

                if ({line = br.readLine(); line != null}) {
                    if (line.startsWith("$timeunit")) {
                        println(line)
                        break
                    }
                    newInputTs = java.lang.Long.parseLong(line.split(":")(0).trim())
                    val rightPart : String = line.split(":")(1)
                    inputStream = rightPart.split("=")(0).trim()
                    if (rightPart.contains("=")) {
                        value = rightPart.split("=")(1).trim()
                    } else {
                        value = ""
                    }
                } else {
                    inputEndReached = true
                }

                if (newInputTs > currTs || inputEndReached) {
                    if (inputEndReached) {
                        newInputTs += 1
                    }

                    breakable {
                        while (true) {

//TRIGGER

                            if (currTs == newInputTs) break

//STEP

                            lastProcessedTs = currTs
                            currTs = newInputTs
                        }
                    }
                    if (inputEndReached) {
                        System.exit(0)
                    }
                } else if (newInputTs < currTs) {
                    System.err.println(s"$currTs: FATAL: decreasing timestamp received")
                    System.exit(1)
                }
//INPUTPROCESSING
            }
        } catch {
            case e: Exception => {
                System.err.println("ERROR parsing the input Stream\n\n" + e)
                System.exit(2)
            }
        }
    }
}
