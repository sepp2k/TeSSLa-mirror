package de.uni_luebeck.isp.tessla.util

import cats._
import cats.implicits._

import scala.annotation.tailrec

class Lazy[+A](a: => A) {
  override def toString = s"Lazy($a)"

  lazy val get: A = a
}

object Lazy {
  implicit def monadInstance: Monad[Lazy] = new Monad[Lazy] {
    override def pure[A](x: A) = Lazy(x)

    override def flatMap[A, B](fa: Lazy[A])(f: A => Lazy[B]) = f(fa.get)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Lazy[Either[A, B]]): Lazy[B] =
      f(a).get match {
        case Right(b) => Lazy(b)
        case Left(a) => tailRecM(a)(f)
      }
  }

  def unapply[T](value: Lazy[T]) = Some(value.get)

  def apply[A](a: => A): Lazy[A] =
    new Lazy(a)
}

