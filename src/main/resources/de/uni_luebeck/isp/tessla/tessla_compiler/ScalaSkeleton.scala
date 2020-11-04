//This Scala code was automatically created by tessla-compiler from a TeSSLa Specification

import java.io.BufferedReader
import java.io.InputStreamReader

object Main {

  def getErrorCode(err: Throwable): Long = {
    err match {
      case err: java.lang.ArithmeticException
          if err.getMessage == "/ by zero" =>
        1
      case _: java.util.NoSuchElementException => 2
      case _                                   => 2048
    }
  }

  def outputVar(output: String,
                trueName: String,
                errorCode: Long,
                ts: Long): Unit = {
    var outputWithError: String =
      if (errorCode != 0)
        "FATAL: " + trueName + " evaluation encountered an Error: "
      else ""

    errorCode match {
      case 0 => outputWithError = trueName + " = " + output
      case 1 => outputWithError += "Division by zero"
      case 2 => outputWithError += "Map access to non existing key"
      case _ => outputWithError += "Unknown error code " + errorCode
    }
    println(s"$ts: $outputWithError")
    if (errorCode != 0) System.exit(errorCode.toInt)
  }

//STATIC

//VARDEF

//CALLBACKS

  def run(): Unit = {
    if (newInputTs > currTs) {

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
    } else if (newInputTs < currTs) {
      System.err.println(s"$currTs: FATAL: decreasing timestamp received")
      System.exit(128)
    }
  }

  def main(args: Array[String]) = {

    val inputSize = args(0).toLong

    //JVM warmup
    runTest(1000, 0)

    val (res, time) = runTest(inputSize, 3100)

    println(s"$res")
    println(s"Runtime: $time ns")
  }

  def runTest(size: Long, offset: Long): (Long, Long) = {

    var cumRes: Long = 0

    val start: Long = System.nanoTime()

    out_z = (value: Long, ts: Long, _: String, _: Long) => {
      cumRes = cumRes + (value + ts)
    }

    var i = 0L
    while(i < size) {
      val ts = offset + i

      //println(s"$ts: $input")
      set_var_x.apply(i, ts)
      i+=1
    }

    val stop: Long = System.nanoTime()

    (cumRes, stop - start)
  }
}
