package de.uni_luebeck.isp.tessla.util

class LazyWithStack[A, Stack](a: (Stack, Boolean) => A) {
  override def toString = s"Lazy($a)"

  private var computing = false
  private var computationStack: Option[Stack] = None

  private var result: Option[A] = None

  def get(stack: Stack): A = {
    result match {
      case Some(r) => r
      case None =>
        val rec = computing
        computing = true
        computationStack = Some(stack)
        try {
          val r = a(stack, rec)
          result = Some(r)
          r
        } finally {
          computing = false
        }
    }
  }

  def flatMap[B](f: A => LazyWithStack[B, Stack]): LazyWithStack[B, Stack] = LazyWithStack((stack, _) => f(get(stack)).get(stack))

  def map[B](f: A => B): LazyWithStack[B, Stack] = LazyWithStack((stack, _) => f(get(stack)))

  def getComputationStack = computationStack

  def isComputing = computing
}

object LazyWithStack {
  def apply[A, Stack](a: (Stack, Boolean) => A): LazyWithStack[A, Stack] =
    new LazyWithStack(a)
}

