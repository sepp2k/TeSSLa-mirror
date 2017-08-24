package de.uni_luebeck.isp.tessla.interpreter

import shapeless._

import scala.language.higherKinds

import scala.language.implicitConversions

object ImplicitConstraints {

  trait ConstraintOr {
    type FS[C] <: HList
  }

  type CFalse =
    ConstraintFalse.type

  object ConstraintFalse extends ConstraintOr {
    override type FS[C] =
      HNil
  }

  trait ||:[A, B <: ConstraintOr] extends ConstraintOr {
    override type FS[C] =
      (A => C) :: B#FS[C]

    def switch[C](fs: FS[C]): C
  }

  implicit val implicitConstraintNil: CFalse =
    ConstraintFalse

  implicit def implicitConstraintOrA[A, B <: ConstraintOr](implicit a: A): ||:[A, B] =
    new ||:[A, B] {
      override def switch[C](fs: FS[C]): C =
        fs.head(a)
    }

  implicit def implicitConstraintOrB[A, B, C <: ConstraintOr](implicit b: B ||: C): ||:[A, B ||: C] =
    new ||:[A, B ||: C] {
      override def switch[D](fs: FS[D]): D =
        b.switch(fs.tail)
    }

  sealed abstract class =:=[From, To] extends (From => To) with Serializable {
    def inverse(to: To): From
  }

  private[this] final val singleton_=:= =
    new =:=[Any, Any] {
      def apply(x: Any): Any =
        x

      def inverse(x: Any): Any =
        x
    }

  object =:= {
    implicit def tpEquals[A]: A =:= A =
      singleton_=:=.asInstanceOf[A =:= A]
  }

}
