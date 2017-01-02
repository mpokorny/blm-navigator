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

import cats.data._
import cats.implicits._

abstract class SheetReader[A] {

  /** Sheet state
    *
    * Comprises reference to Sheet instance and a row index
    */
  trait State {

    /* Get next non-empty row. We skip empty rows, although the row index
     * increases! */
    def step: Option[(State, (Int, Seq[CellValue]))]
  }

  def nextRow: SheetReader.StateT[State] =
    StateT(_.step)

  def initial(a: A): State
}

object SheetReader {
  type StateT[A] = cats.data.StateT[Option, A, (Int, Seq[CellValue])]
}
