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
import cats.data._

final class TRS(
  val state: States.State,
  val principalMeridian: TRS.PrincipalMeridian,
  val townshipNumber: Int,
  val townshipFraction: TRS.Fraction,
  val townshipDirection: Directions.NS,
  val rangeNumber: Int,
  val rangeFraction: TRS.Fraction,
  val rangeDirection: Directions.EW,
  val sectionNumber: TRS.Section,
  val sectionDivision: List[Directions.Corner], // most to least significant
  val townshipDuplicate: Int) {

  override def hashCode(): Int =
    state.hashCode() + 41 * (
      principalMeridian.hashCode() + 41 * (
        townshipNumber.hashCode() + 41 * (
          townshipFraction.hashCode() + 41 * (
            townshipDirection.hashCode() + 41 * (
              rangeNumber.hashCode() + 41 * (
                rangeFraction.hashCode() + 41 * (
                  rangeDirection.hashCode() + 41 * (
                    sectionNumber.hashCode() + 41 * (
                      sectionDivision.hashCode() + 41 * (
                        townshipDuplicate.hashCode()))))))))))

  override def equals(that: Any): Boolean =
    if (that.isInstanceOf[TRS]) {
      val other = that.asInstanceOf[TRS]

      other.state == this.state &&
        other.principalMeridian == this.principalMeridian &&
        other.townshipNumber == this.townshipNumber &&
        other.townshipFraction == this.townshipFraction &&
        other.townshipDirection == this.townshipDirection &&
        other.rangeNumber == this.rangeNumber &&
        other.rangeFraction == this.rangeFraction &&
        other.rangeDirection == this.rangeDirection &&
        other.sectionNumber == this.sectionNumber &&
        other.sectionDivision == this.sectionDivision &&
        other.townshipDuplicate == this.townshipDuplicate
    } else {
      false
    }

  override def toString: String =
    s"TRS($state,$principalMeridian,$townshipNumber,$townshipFraction," ++
      s"$townshipDirection,$rangeNumber,$rangeFraction,$rangeDirection, " ++
      s"$sectionNumber,$sectionDivision,$townshipDuplicate)"

  def plssId =
    f"${state}%s${principalMeridian.p}%02d" ++
      f"${townshipNumber}%03d${townshipFraction.f}%1d" ++
      f"${townshipDirection.toString}%s${rangeNumber}%03d" ++
      f"${rangeFraction.f}%1d${rangeDirection.toString}%s" ++
      f"${townshipDuplicate}%1dSN${sectionNumber.s}%03d" ++
      s"A${sectionDivision.take(2).reverse.mkString("")}"
}

object TRS {

  final case class Fraction(f: Int) {
    require(0 <= Fraction.minVal && f <= Fraction.maxVal)
  }

  object Fraction {
    val minVal = 0
    val maxVal = 3
  }

  final case class Section(s: Int) {
    require(1 <= Section.minVal && Section.maxVal <= 36)
  }

  object Section {
    val minVal = 1
    val maxVal = 36
  }

  final case class PrincipalMeridian(p: Int) {
    require(1 <= PrincipalMeridian.minVal &&
              p <= PrincipalMeridian.maxVal)
  }

  object PrincipalMeridian {
    val minVal = 1

    val maxVal = 45

    def lookup(str: String): Option[PrincipalMeridian] =
      names.get(str.toLowerCase)

    lazy val names = {
      val lcNames = List(
          "first", "second", "third", "fourth", "fifth", "sixth",
          "black hills", "boise", "chickasaw", "choctaw", "cimarron",
          "copper river", "fairbanks", "gila and salt river", "humboldt",
          "hunstville", "indian", "louisiana", "michigan", "montana",
          "mount diablo", "navajo", "new mexico", "st helens", "st stephens",
          "salt lake", "san bernardino", "seward", "tallahassee",
          "uintah special", "ute", "washington", "willamette", "wind river",
          "ohio", "INVALID", "muskigum river", "ohio river", "first scioto river",
          "second scioto river", "third scioto river", "ellicotts line",
          "twelve mile square", "kateel river", "umiat")

      (lcNames.zipWithIndex.toMap - "INVALID").
        mapValues(i => PrincipalMeridian(i + 1))
    }

  }

  def parseState(cv: CellValue): ValidatedNel[Error, States.State] =
    cv match {
      case CellString(s) =>
        if (States.apply.isDefinedAt(s))
          Validated.valid(States(s))
        else
          Validated.invalidNel(Error(s"Unrecognized state value '$s'"))

      case _ =>
        Validated.invalidNel(Error("Invalid state value type"))
    }

  def parsePrincipalMeridian(cv: CellValue):
      ValidatedNel[Error, PrincipalMeridian] = {
    def fromInt(i: Int): ValidatedNel[Error, PrincipalMeridian] =
      if (PrincipalMeridian.minVal <= i && i <= PrincipalMeridian.maxVal)
        Validated.valid(PrincipalMeridian(i))
      else
        Validated.invalidNel(
          Error(s"Out of range principal meridian value '$i'"))

    cv match {
      case CellNumeric(d) =>
        if (d.isValidInt) {
          fromInt(d.toInt)
        } else {
          Validated.invalidNel(
            Error(s"Non-integer principal meridian value '$d'"))
        }

      case CellString(s) =>
        try {
          fromInt(s.toInt)
        } catch {
          case _: NumberFormatException =>
            PrincipalMeridian.lookup(s) match {
              case Some(pm) =>
                Validated.valid(pm)
              case None =>
                Validated.invalidNel(
                  Error(s"Unrecognized principal meridian value '$s'"))
            }
        }

      case _ =>
        Validated.invalidNel(Error("Invalid principal meridian value type"))
    }
  }

  def parseInt(name: String, cv: CellValue): ValidatedNel[Error, Int] =
    cv match {
      case CellNumeric(d) =>
        if (d.isValidInt)
          Validated.valid(d.toInt)
        else
          Validated.invalidNel(Error(s"Non-integer $name value '$d'"))

      case CellString(s) =>
        try {
          Validated.valid(s.toInt)
        } catch {
          case _: NumberFormatException =>
            Validated.invalidNel(Error(s"Non-integer $name value '$s'"))
        }

      case _ =>
        Validated.invalidNel(Error(s"Invalid $name value type"))
    }

  def parseFraction(name: String, cv: CellValue): ValidatedNel[Error, Fraction] = {
    def fromInt(i: Int): ValidatedNel[Error, Fraction] =
      if (Fraction.minVal <= i && i <= Fraction.maxVal)
        Validated.valid(Fraction(i))
      else
        Validated.invalidNel(
          Error(s"Out of range $name value '$i'"))

    cv match {
      case CellNumeric(d) =>
        if (d.isValidInt) {
          fromInt(d.toInt)
        } else {
          Validated.invalidNel(
            Error(s"Non-integer $name value '$d'"))
        }

      case CellString(s) =>
        try {
          fromInt(s.toInt)
        } catch {
          case _: NumberFormatException =>
            Validated.invalidNel(Error(s"Unrecognized $name value '$s"))
        }

      case _ =>
        Validated.invalidNel(Error(s"Invalid $name value type"))
    }
  }

  def parseTownshipDirection(cv: CellValue): ValidatedNel[Error, Directions.NS] =
    cv match {
      case CellString(s) =>
        if (Directions.ns.isDefinedAt(s))
          Validated.valid(Directions.ns(s))
        else
          Validated.invalidNel(
            Error(s"Unrecognized Township Direction value '$s'"))

      case _ =>
        Validated.invalidNel(Error(s"Invalid Township Direction value type"))
    }

  def parseRangeDirection(cv: CellValue): ValidatedNel[Error, Directions.EW] =
    cv match {
      case CellString(s) =>
        if (Directions.ew.isDefinedAt(s))
          Validated.valid(Directions.ew(s))
        else
          Validated.invalidNel(
            Error(s"Unrecognized Range Direction value '$s'"))

      case _ =>
        Validated.invalidNel(Error(s"Invalid Range Direction value type"))
    }

  def parseSection(cv: CellValue): ValidatedNel[Error, Section] = {
    def fromInt(i: Int): ValidatedNel[Error, Section] =
      if (Section.minVal <= i && i <= Section.maxVal)
        Validated.valid(Section(i))
      else
        Validated.invalidNel(
          Error(s"Out of range Section value '$i'"))

    cv match {
      case CellNumeric(d) =>
        if (d.isValidInt) {
          fromInt(d.toInt)
        } else {
          Validated.invalidNel(
            Error(s"Non-integer Section value '$d'"))
        }

      case CellString(s) =>
        try {
          fromInt(s.toInt)
        } catch {
          case _: NumberFormatException =>
            Validated.invalidNel(Error(s"Unrecognized Section value '$s"))
        }

      case _ =>
        Validated.invalidNel(Error("Invalid Section value type"))
    }
  }

  def parseSectionDivision(cv: CellValue):
      ValidatedNel[Error, List[Directions.Corner]] = {
    cv match {
      case CellString(s) =>
        try {
          Validated.valid(
            s.map(d => Directions.division(d.toString.toInt - 1)).toList)
        } catch {
          case _: Throwable =>
            Validated.invalidNel(
              Error(s"Unrecognized Section Division value '$s'"))
        }

      case _ =>
        Validated.invalidNel(Error(s"Invalid Section Division value type"))
    }

  }

  def apply(row: Map[String, CellValue]): ValidatedNel[Error, TRS] = {

    def get[A](name: String, parseFn: CellValue => ValidatedNel[Error, A]):
        ValidatedNel[Error, A] =
      Validated.fromOption(
        row.get(name),
        Error(s"Missing '$name' header")).
        toValidatedNel.
        andThen(parseFn)

    def getInt(name: String): ValidatedNel[Error, Int] =
      get(name, parseInt(name, _))

    def getFraction(name: String): ValidatedNel[Error, Fraction] =
      get(name, parseFraction(name, _))

    val state = get("State", parseState)
    val principalMeridian = get("Principal Meridian", parsePrincipalMeridian)
    val townshipNumber = getInt("Township")
    val rangeNumber = getInt("Range")
    val townshipDuplicate = getInt("Township Duplicate")
    val townshipFraction = getFraction("Township Fraction")
    val rangeFraction = getFraction("Range Fraction")
    val townshipDirection = get("Township Direction", parseTownshipDirection)
    val rangeDirection = get("Range Direction", parseRangeDirection)
    val sectionNumber = get("Section", parseSection)
    val sectionDivision = get("Section Division", parseSectionDivision)

    implicit val nelSemigroup: Semigroup[NonEmptyList[Error]] =
      SemigroupK[NonEmptyList].algebra[Error]

    Apply[ValidatedNel[Error, ?]].map11(
      state,
      principalMeridian,
      townshipNumber,
      townshipFraction,
      townshipDirection,
      rangeNumber,
      rangeFraction,
      rangeDirection,
      sectionNumber,
      sectionDivision,
      townshipDuplicate) {
      case (st, pm, tn, tf, td, rn, rf, rd, sn, sd, tdup) =>
        new TRS(st, pm, tn, tf, td, rn, rf, rd, sn, sd, tdup)
    }
  }

  def unapply(trs: TRS):
      Option[(States.State, TRS.PrincipalMeridian, Int, TRS.Fraction,
              Directions.NS, Int, TRS.Fraction, Directions.EW, TRS.Section,
              List[Directions.Corner], Int)] =
    Some((trs.state, trs.principalMeridian, trs.townshipNumber,
          trs.townshipFraction, trs.townshipDirection, trs.rangeNumber,
          trs.rangeFraction, trs.rangeDirection, trs.sectionNumber,
          trs.sectionDivision, trs.townshipDuplicate))

  final case class Error(message: String)
}
