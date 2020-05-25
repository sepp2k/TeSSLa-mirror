package de.uni_luebeck.isp.tessla.util

import cats._
import cats.implicits

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object ArraySeqMonad {

  implicit val instance: Monad[ArraySeq] with Traverse[ArraySeq] =
    new Monad[ArraySeq] with Traverse[ArraySeq] {
      override def flatMap[A, B](fa: ArraySeq[A])(f: A => ArraySeq[B]) = fa.flatMap(f)

      def tailRecM[A, B](a: A)(f: A => ArraySeq[Either[A, B]]): ArraySeq[B] = {
        val buf = ArraySeq.untagged.newBuilder[B]
        @tailrec def go(lists: List[Iterator[Either[A, B]]]): Unit = lists match {
          case it :: tail =>
            if (it.isEmpty) {
              go(tail)
            } else {
              it.next() match {
                case Right(b) =>
                  buf += b
                  go(it :: tail)
                case Left(a) => go(f(a).iterator :: it :: tail)
              }
            }
          case Nil => ()
        }
        go(f(a).iterator :: Nil)
        buf.result
      }

      override def pure[A](x: A) = ArraySeq.untagged(x)

      override def foldLeft[A, B](fa: ArraySeq[A], b: B)(f: (B, A) => B) = fa.foldLeft(b)(f)

      def traverse[G[_], A, B](
        fa: ArraySeq[A]
      )(f: A => G[B])(implicit G: Applicative[G]): G[ArraySeq[B]] =
        foldRight[A, G[ArraySeq[B]]](fa, Always(G.pure(ArraySeq.empty))) { (a, lglb) =>
          G.map2Eval(f(a), lglb)(_ +: _)
        }.value

      def foldRight[A, B](fa: ArraySeq[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(as: Iterator[A]): Eval[B] =
          if (as.isEmpty) lb else f(as.next(), Eval.defer(loop(as)))
        Eval.defer(loop(fa.iterator))
      }
    }
}
