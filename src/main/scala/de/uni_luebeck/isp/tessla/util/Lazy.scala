package de.uni_luebeck.isp.tessla.util

import scala.language.implicitConversions

class Lazy[+A](a: => A) {
  lazy val get: A = a

  def map[B](f: A => B): Lazy[B] = Lazy(f(a))
}

object Lazy {
  def unapply[T](value: Lazy[T]) = Some(value.get)

  def apply[A](a: => A): Lazy[A] =
    new Lazy(a)
}

