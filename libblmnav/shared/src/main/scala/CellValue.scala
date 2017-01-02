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

import java.util.Date

/** XLS file cell value representation
  *
  */
sealed trait CellValue {
  type A
  val value: A
  def hasTypeOf(other: CellValue): Boolean
  def typeDescription: String
}

/** String cell value
  */
final case class CellString(value: String) extends CellValue {
  type A = String
  override def hasTypeOf(other: CellValue): Boolean = other match {
      case CellString(_) => true
      case _ => false
    }
  def typeDescription: String = "String"
}

/** Numeric cell value
  */
final case class CellNumeric(value: Double) extends CellValue {
  type A = Double
  override def hasTypeOf(other: CellValue): Boolean = other match {
      case CellNumeric(_) => true
      case _ => false
    }
  def typeDescription: String = "Numeric"
}

/** Date cell value
  */
final case class CellDate(value: Date) extends CellValue {
  type A = Date
  override def hasTypeOf(other: CellValue): Boolean = other match {
      case CellDate(_) => true
      case _ => false
    }
  def typeDescription: String = "Date"
}

/** Boolean cell value
  */
final case class CellBoolean(value: Boolean) extends CellValue {
  type A = Boolean
  override def hasTypeOf(other: CellValue): Boolean = other match {
      case CellBoolean(_) => true
      case _ => false
    }
  def typeDescription: String = "Boolean"
}

/** Blank cell value
  */
final case object CellBlank extends CellValue {
  type A = Unit
  val value = ()
  override def hasTypeOf(other: CellValue): Boolean = other match {
      case CellBlank => true
      case _ => false
    }
  def typeDescription: String = "Blank"
}

/** Formula cell value
  */
final case class CellFormula(value: String) extends CellValue {
  type A = String
  override def hasTypeOf(other: CellValue): Boolean = other match {
      case CellFormula(_) => true
      case _ => false
    }
  def typeDescription: String = "Formula"
}

/** Error cell value
  */
final case class CellError(value: Byte) extends CellValue {
  type A = Byte
  override def hasTypeOf(other: CellValue): Boolean = other match {
      case CellError(_) => true
      case _ => false
    }
  def typeDescription: String = "Error"
}
