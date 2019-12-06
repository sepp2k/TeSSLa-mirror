package de.uni_luebeck.isp.tessla.util

import cats._
import cats.implicits._

class LazyWithStack[Stack] {

  object StackLazy {
    def apply[A](a: Stack => A) = new StackLazy(a)

    implicit def monadInstance: Monad[StackLazy] = new Monad[StackLazy] {
      override def pure[A](x: A) = StackLazy(_ => x)

      override def map[A, B](fa: StackLazy[A])(f: A => B) = StackLazy {stack =>
        f(fa.get(stack))
      }

      override def flatMap[A, B](fa: StackLazy[A])(f: A => StackLazy[B]) =
        StackLazy[B](stack =>
          f(fa.get(stack)).get(stack)
        )

      override def tailRecM[A, B](a: A)(f: A => StackLazy[Either[A, B]]) =
        flatMap(f(a)) {
          case Right(b) => pure(b)
          case Left(nextA) => tailRecM(nextA)(f)
        }
    }

  }

  class StackLazy[+A](a: Stack => A) {
    override def toString = s"Lazy($a)"

    private var computing = false
    private var computationStack: Option[Stack] = None

    private val result: Helper[_ <: A] = new Helper(a)

    def get(stack: Stack): A = result.get(stack)

    private class Helper[B](b: Stack => B) {
      private var result: Option[B] = None

      def get(stack: Stack): B = {
        result match {
          case Some(r) => r
          case None =>
            val rec = computing
            computing = true
            computationStack = Some(stack)
            try {
              val r = call(stack, rec)(b)
              result = Some(r)
              r
            } finally {
              computing = false
            }
        }
      }
    }

    def getComputationStack = computationStack

    def isComputing = computing

    def toLazy(stack: Stack) = Lazy(get(stack))
  }

  def call[A](stack: Stack, rec: Boolean)(a: Stack => A) = a(stack)

}

object LazyWithStack {
  def apply[Stack]: LazyWithStack[Stack] = new LazyWithStack
}

