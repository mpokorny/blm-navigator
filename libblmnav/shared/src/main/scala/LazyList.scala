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

sealed trait LazyList[+A]

final case class LazyListCons[A](head: A, tail: Eval[LazyList[A]])
  extends LazyList[A]

case object LazyNil extends LazyList[Nothing]

object LazyList {
  private def fromIterator[A](iter: Iterator[A]): Eval[LazyList[A]] =
    if (iter.hasNext)
      Eval.now(LazyListCons(iter.next(), Eval.defer(fromIterator(iter))))
    else
      Eval.now(LazyNil)

  def apply[A](iterable: Iterable[A]): Eval[LazyList[A]] =
    fromIterator(iterable.iterator)
}
