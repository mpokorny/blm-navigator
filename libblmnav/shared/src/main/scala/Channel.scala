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

import scala.collection.mutable
import scala.concurrent._
import scala.concurrent.duration._

import cats._

class Channel[A] {

  private[this] val elementQueue: mutable.Queue[Option[A]] =
    mutable.Queue.empty

  private[this] val promiseQueue: mutable.Queue[Promise[Option[A]]] =
    mutable.Queue.empty

  def push(element: Option[A]): Unit =
    synchronized {
      if (!promiseQueue.isEmpty)
        promiseQueue.dequeue().success(element)
      else
        elementQueue.enqueue(element)
    }

  def pop(): Future[Option[A]] =
    synchronized {
      if (!elementQueue.isEmpty) {
        Future.successful(elementQueue.dequeue())
      } else {
        val p = Promise[Option[A]]

        promiseQueue.enqueue(p)

        p.future
      }
    }

  def next(atMost: Duration): Option[A] =
    Await.result(pop(), atMost)
}

object Channel {
  implicit object ChannelReducible extends Foldable[Channel] {
    override def foldLeft[A, B](fa: Channel[A], b: B)(f: (B, A) ⇒ B): B =
      fa.next(Duration.Inf) map { a =>
        foldLeft(fa, f(b, a))(f)
      } getOrElse b

    override def foldRight[A, B](
      fa: Channel[A], lb: Eval[B])(
      f: (A, Eval[B]) ⇒ Eval[B]):
        Eval[B] = {
      def next: Eval[B] =
        Eval.now(fa.next(Duration.Inf)) flatMap { oa =>
          oa map (a => f(a, Eval.defer(next))) getOrElse lb
        }

      next
    }
  }
}
