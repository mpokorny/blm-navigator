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

import java.io.{ BufferedReader, FileReader }

import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.concurrent.duration._

import cats._
import cats.implicits._
import monix.execution.Scheduler.Implicits.global

object TRS2LatLon extends App {

  val channel = new Channel[IndexedValue[Table2LatLon.LatLonResult]]

  val submit =
    Future {
      val lines =
        new JStreamIterable(new BufferedReader(new FileReader(args(0))).lines())

      val reader = new TableReader(new CSVReader)

      implicit val timeout = 2.seconds

      val converter = Table2LatLon(reader, channel, 1)

      converter(lines)
    }

  (channel.foldRight(Eval.now(())) {
    case (res@_, lb@_) =>
      println(s"$res")

      lb
  }).value
}

class JStreamIterable[A](stream: java.util.stream.Stream[A]) extends Iterable[A] {
  override def iterator: Iterator[A] =
    stream.iterator()
}
