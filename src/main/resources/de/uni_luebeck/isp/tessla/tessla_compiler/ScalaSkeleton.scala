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


  //Methods included solely for benchmark creation
  //Comment out for other examples

  def main(args: Array[String]) = {

    val inputSize = args(0).toLong

    //JVM warmup
    runTest(1000, 0)

    val (res, time) = runTest(inputSize, 3100)

    println(s"Result: $res")
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
