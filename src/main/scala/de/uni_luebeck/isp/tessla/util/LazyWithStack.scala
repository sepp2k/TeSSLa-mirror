package de.uni_luebeck.isp.tessla.util

import cats._
import cats.implicits._

class LazyWithStack[Stack] {

  object StackLazy {
    def apply[A](a: Stack => A) = new StackLazyImpl(a)

    implicit def monadInstance: Monad[StackLazyImpl] = new Monad[StackLazyImpl] {
      override def pure[A](x: A) = StackLazy(_ => x)

      override def map[A, B](fa: StackLazyImpl[A])(f: A => B) = StackLazy {stack =>
        f(fa.get(stack))
      }

      override def flatMap[A, B](fa: StackLazyImpl[A])(f: A => StackLazyImpl[B]) =
        StackLazy[B](stack =>
          f(fa.get(stack)).get(stack)
        )

      override def tailRecM[A, B](a: A)(f: A => StackLazyImpl[Either[A, B]]) =
        flatMap(f(a)) {
          case Right(b) => pure(b)
          case Left(nextA) => tailRecM(nextA)(f)
        }
    }

  }

  type StackLazy[+A] = StackLazyImpl[_ <: A]

  class StackLazyImpl[A](a: Stack => A) {
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
            val r = call(stack, rec)(a)
            result = Some(r)
            r
          } finally {
            computing = false
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

