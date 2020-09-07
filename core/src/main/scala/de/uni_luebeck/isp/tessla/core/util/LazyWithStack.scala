package de.uni_luebeck.isp.tessla.core.util

import cats._

/**
 * Defers a computation until `get` is invoked.
 * Supports the notion of a call stack.
 * Used to keep track of the stack when walking an AST recursively using lazy computation.
 */
class LazyWithStack[Frame]() {

  type Stack = List[Frame]

  object Lazy {
    def apply[A](a: => A): StackLazy[A] = new StackLazyImpl(_ => a)
  }

  object StackLazy {
    def apply[A](a: Stack => A): StackLazy[A] = new StackLazyImpl(a)

    implicit def monadInstance: CommutativeMonad[StackLazy] = new CommutativeMonad[StackLazy] {
      override def pure[A](x: A) = StackLazy(_ => x)

      override def map[A, B](fa: StackLazy[A])(f: A => B) = StackLazy { stack =>
        f(fa.get(stack))
      }

      override def flatMap[A, B](fa: StackLazy[A])(f: A => StackLazy[B]) =
        StackLazy[B](stack => f(fa.get(stack)).get(stack))

      override def tailRecM[A, B](a: A)(f: A => StackLazy[Either[A, B]]): StackLazy[B] =
        flatMap(f(a)) {
          case Right(b)    => pure(b)
          case Left(nextA) => tailRecM(nextA)(f)
        }
    }

  }

  sealed trait StackLazy[+A] {
    self =>
    def get(stack: Stack): A

    def push(f: Frame): StackLazy[A] = new StackLazy[A] {
      override def get(stack: Stack): A = self.get(f :: stack)
    }
  }

  private class StackLazyImpl[+A](a: Stack => A) extends StackLazy[A] {
    self =>
    override def toString = s"Lazy($a)"

    private var computing = false

    private[this] var result: Option[A] = None

    override def get(stack: Stack): A = {
      result match {
        case Some(r) => r
        case None =>
          val rec = computing
          computing = true
          try {
            val r = call(stack, rec)(a)
            result = Some(r)
            r
          } finally {
            computing = false
          }
      }
    }
  }

  protected def call[A](stack: Stack, rec: Boolean)(a: Stack => A) = a(stack)

}
