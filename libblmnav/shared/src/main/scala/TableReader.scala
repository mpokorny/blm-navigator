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
import cats.instances.list._
import cats.instances.option._

class TableReader[A](val sheetReader: SheetReader[A]) {

  type S = sheetReader.State

  val nextRow = sheetReader.nextRow

  import TableReader._

  sealed trait State {
    def step:
        Option[(State, (Int, ValidatedNel[Error, Map[String, CellValue]]))]
  }

  final class StateUninitialized(readerState: S) extends State {

    val cellString = CellString("")

    override def step:
        Option[(State, (Int, ValidatedNel[Error, Map[String, CellValue]]))] = {

      val next =
        nextRow.run(readerState) map {
          case (readerState1@_, (i@_, hdr@_)) =>
            val nonStringHdrIndices =
              hdr.zipWithIndex.
                withFilter(h => !h._1.hasTypeOf(cellString)).
                map(_._2)
            if (nonStringHdrIndices.length > 0) {
              val state: State = StateDone
              val err: ValidatedNel[Error, Map[String, CellValue]] =
                Validated.invalidNel(InvalidHeader(nonStringHdrIndices))

              Some((state, (i, err)))
            } else {
              val hdrNames = hdr collect { case CellString(s@_) => s }

              new StateReady(
                readerState1,
                hdrNames,
                List.fill(hdrNames.length)(None)).
                step
            }

        }

      next getOrElse None
    }
  }

  final class StateReady(
    readerState: S,
    colNames: Seq[String],
    cellTemplate: List[Option[CellValue]])
      extends State {

    override def step:
        Option[(State, (Int, ValidatedNel[Error, Map[String, CellValue]]))] = {

      val next =
        nextRow.run(readerState) map {
          case (readerState1@_, (i@_, cvs@_)) =>
            val (newTemplate, vcvs) =
              validateColTypes(cellTemplate, cvs.toList)
            val rval = vcvs.map(colNames.zip(_).toMap)
            val state: State =
              new StateReady(readerState1, colNames, newTemplate)

            Some((state, (i, rval)))
        }

      next getOrElse None
    }
  }

  final case object StateDone extends State  {

    override def step:
        Option[(State, (Int, ValidatedNel[Error, Map[String, CellValue]]))] =
      None
  }

  def initial(a: A): State =
    new StateUninitialized(sheetReader.initial(a))

  def nextRecord: StateT[State] =
    StateT(_.step)
}

object TableReader {

  type StateT[A] =
    cats.data.StateT[Option, A, (Int, ValidatedNel[Error, Map[String, CellValue]])]

  def validateColTypes(
    template: List[Option[CellValue]],
    row: List[CellValue]):
      (List[Option[CellValue]], ValidatedNel[Error, List[CellValue]]) = {

    def validate(cell: ((Option[CellValue], CellValue), Int)):
        (Option[CellValue], ValidatedNel[Error, CellValue]) = {
      val ((templateValue, cellValue), colIdx) = cell

      templateValue map { setValue =>
        val vCell: ValidatedNel[Error, CellValue] =
          /* allow blank values everywhere to support optional values */
          if (cellValue.hasTypeOf(setValue) || cellValue.hasTypeOf(CellBlank)) {
            Validated.valid(cellValue)
          } else {
            Validated.invalidNel(CellType(colIdx, setValue.typeDescription))
          }

        (Some(setValue), vCell)
      } getOrElse {
        // set template for this cell only if cell is not blank
        val setValue: Option[CellValue] =
          if (cellValue.hasTypeOf(CellBlank)) None
          else Some(cellValue)

        (setValue, Validated.valid(cellValue))
      }
    }

    val cells = template.zipAll(row, None, CellBlank).zipWithIndex

    val (newTemplate, vCells) =
      cells.foldLeft(
        (List.empty[Option[CellValue]],
         Validated.valid[Error, List[CellValue]](List.empty).toValidatedNel)) {
        case ((template@_, vCells@_), cell@_) =>
          val (cTemplate, vCell) = validate(cell)

          (cTemplate :: template, vCell.map(List(_)).combine(vCells))
      }

    (newTemplate.reverse, vCells.map(_.reverse))
  }

  sealed trait Error
  final case class InvalidHeader(columns: Seq[Int]) extends Error
  final case class CellType(column: Int, expectedType: String) extends Error
}
