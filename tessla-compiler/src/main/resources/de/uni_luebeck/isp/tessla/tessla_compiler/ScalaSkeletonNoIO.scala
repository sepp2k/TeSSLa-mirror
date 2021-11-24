//This Scala code was automatically created by tessla-compiler from a TeSSLa Specification

//USERINCLUDES

object TesslaMonitor {

//STATIC

//VARDEF

  case class UnknownEventError(base: Throwable) extends java.lang.Exception {
    override def getMessage: String =
      "Uncertainty concerning the existence of an event" +
        (if (base != null)
           s" due to the follwing base error:\n ${base}"
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

  def flush(): Unit = step(currTs, true)

  private def step(newInputTs: Long, flush: Boolean = false): Unit = {
    var flushReq = flush

    if (newInputTs > currTs || flushReq) {

      var doProcessing = true
      while (doProcessing) {

//TRIGGER

        if (currTs == newInputTs && !flushReq) {
          doProcessing = false
        } else {

//STEP

//TAIL

          flushReq = flush && (currTs != newInputTs)
          lastProcessedTs = currTs
          currTs = newInputTs
        }
      }
    } else if (newInputTs < currTs) {
      System.err.println(s"$currTs: FATAL: decreasing timestamp received")
      System.exit(128)
    }
  }
}
