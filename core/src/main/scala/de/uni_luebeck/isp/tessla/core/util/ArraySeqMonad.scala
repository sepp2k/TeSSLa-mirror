/*
 * Copyright 2020 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.core.util

import cats._

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

/**
 * Provides an implicit value for a cats Monad for ArraySeqs.
 * */
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
