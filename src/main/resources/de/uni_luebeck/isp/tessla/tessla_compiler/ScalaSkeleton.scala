//This Scala code was automatically created by tessla-compiler from a TeSSLa Specification

import util.control.Breaks._
import java.io.BufferedReader
import java.io.InputStreamReader

object Main {

    def processStringInput(s: String) : String = {
        if (s.charAt(0) == '\"' && s.charAt(s.length - 1) == '\"') {
            s.substring(1, s.length - 1)
        } else {
            System.err.println("FATAL: String input has incorrect format: \n" + s)
            System.exit(129)
            ""
        }
    }

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

//VARDEF

        try {
            val br: BufferedReader = new BufferedReader(new InputStreamReader(System.in))

            var line: String = "";
            var inputEndReached: Boolean = false;

            while (true) {

                if ({line = br.readLine(); line != null}) {
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
                System.err.println("ERROR parsing the input Stream\n\n" + e)
                System.exit(129)
            }
        }
    }
}
