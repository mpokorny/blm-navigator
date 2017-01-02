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

object Directions {

  private val north = "N"
  private val south = "S"
  private val east = "E"
  private val west = "W"
  private val northeast = "NE"
  private val southeast = "SE"
  private val southwest = "SW"
  private val northwest = "NW"

  trait D
  trait NS extends D
  trait EW extends D
  trait Corner extends D
  case object North extends NS { override def toString: String = north }
  case object South extends NS { override def toString: String = south }
  case object East extends EW { override def toString: String = east }
  case object West extends EW { override def toString: String = west }
  case object NorthWest extends Corner {
    override def toString: String = northwest
  }
  case object NorthEast extends Corner {
    override def toString: String = northeast
  }
  case object SouthEast extends Corner {
    override def toString: String = southeast
  }
  case object SouthWest extends Corner {
    override def toString: String = southwest
  }

  val ns: PartialFunction[String, NS] =
    _.toUpperCase match {
      case `north` => North
      case `south` => South
    }

  val ew: PartialFunction[String, EW] =
    _.toUpperCase match {
      case `east` => East
      case `west` => West
    }

  val corner: PartialFunction[String, Corner] =
    _.toUpperCase match {
      case `northwest` => NorthWest
      case `northeast` => NorthEast
      case `southeast` => SouthEast
      case `southwest` => SouthWest
    }

  def apply(str: String): D =
    (ns orElse ew orElse corner)(str)

  val division: IndexedSeq[Corner] =
    IndexedSeq(NorthWest, NorthEast, SouthWest, SouthEast)
}
