package de.uni_luebeck.isp.tessla.util

class LazyWithStack[A, StackFrame](a: List[StackFrame] => A) {
  override def toString = s"Lazy($a)"

  private var computing = false
  private var computationStack: Option[List[StackFrame]] = None

  private var result: Option[A] = None

  def get(stack: List[StackFrame]): A = {
    result match {
      case Some(r) => r
      case None =>
        computing = true
        computationStack = Some(stack)
        try {
          val r = a(stack)
          result = Some(r)
          r
        } finally {
          computing = false
        }
    }
  }

  def getComputationStack = computationStack
  def isComputing = computing
}
object LazyWithStack {
  def apply[A, StackFrame](a: List[StackFrame] => A): LazyWithStack[A, StackFrame] =
    new LazyWithStack(a)
}

