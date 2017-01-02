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

class CSVReader(separator: Char = ',') extends SheetReader[Iterable[String]] {

  def cells(str: String): Seq[CellValue] =
    str.split(separator) map { s =>
      val s1 = s.trim
      if (s1.length == 0) CellBlank
      else CellString(s1)
    }

  final class CSVState(row: Int, stream: Eval[LazyList[String]])
      extends State {

    override def step: Option[(State, (Int, Seq[CellValue]))] =
      (stream map {
         case LazyListCons(hd@_, tl@_) =>
           val cs = cells(hd)

           if (cs.length > 0)
             Some((new CSVState(row + 1, tl), (row, cs)))
           else
             (new CSVState(row + 1, tl)).step

         case _ =>
           None
       }).value
  }

  override def initial(stream: Iterable[String]): State = 
    new CSVState(0, LazyList(stream))
}
