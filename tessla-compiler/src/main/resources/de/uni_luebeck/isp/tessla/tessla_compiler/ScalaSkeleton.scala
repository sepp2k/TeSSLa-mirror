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

//USERCODE
}
