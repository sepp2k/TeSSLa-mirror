package de.uni_luebeck.isp.tessla.util

import scala.language.implicitConversions

class Lazy[+A](a: => A) {
  override def toString = s"Lazy($a)"

  lazy val get: A = a

  def map[B](f: A => B): Lazy[B] = Lazy(f(get))

  def flatMap[B](f: A => Lazy[B]): Lazy[B] = f(get)
}

object Lazy {
  def unapply[T](value: Lazy[T]) = Some(value.get)

  def apply[A](a: => A): Lazy[A] =
    new Lazy(a)
}

