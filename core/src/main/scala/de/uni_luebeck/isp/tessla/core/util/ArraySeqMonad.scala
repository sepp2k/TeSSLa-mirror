/*
 * Copyright (c) 2020 Institute of Software Engineering and Programming Languages,
 * University of Lübeck, Germany
 *
 * Modified MIT license:
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this binary (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software and the code which is
 * generated by the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.uni_luebeck.isp.tessla.core.util

import cats._

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
        buf.result()
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
