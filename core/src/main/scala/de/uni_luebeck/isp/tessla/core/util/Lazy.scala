/*
 * Copyright 2021 The TeSSLa Community
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

/**
 * Defer a computation until `get` is called.
 */
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
        case Left(a)  => tailRecM(a)(f)
      }
  }

  def unapply[T](value: Lazy[T]) = Some(value.get)

  def apply[A](a: => A): Lazy[A] =
    new Lazy(a)
}
