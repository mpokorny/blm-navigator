/* Copyright © 2016 Martin Pokorny <martin@truffulatree.org>
 *
 * This file is part of blm-navigator.
 *
 * blm-navigator is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * blm-navigator is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * blm-navigator. If not, see <http://www.gnu.org/licenses/>.
 */
package org.truffulatree.blmnav

import cats._

case class IndexedValue[A](index: Int, value: A)

object IndexedValue {
  implicit object IndexedValueComonad extends Comonad[IndexedValue] {
    override def coflatMap[A, B](fa: IndexedValue[A])(f: (IndexedValue[A]) ⇒ B):
        IndexedValue[B] =
      IndexedValue(fa.index, f(fa))

    override def extract[A](x: IndexedValue[A]): A =
      x.value

    override def map[A, B](fa: IndexedValue[A])(f: (A) ⇒ B): IndexedValue[B] =
      IndexedValue(fa.index, f(fa.value))
  }
}
