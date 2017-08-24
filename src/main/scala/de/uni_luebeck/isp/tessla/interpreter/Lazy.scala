package de.uni_luebeck.isp.tessla.interpreter

import scala.language.implicitConversions

class Lazy[+A](a: => A) {
  lazy val get: A = a
}

object Lazy {
  def apply[A](a: => A): Lazy[A] =
    new Lazy(a)

  implicit def forceLazy[A](l: Lazy[A]): A =
    l.get
}

