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

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.Future
import scala.language.higherKinds

import cats._
import cats.implicits._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser.decode
import fr.hmil.roshttp.HttpRequest
import fr.hmil.roshttp.Protocol.HTTPS
import monix.execution.Scheduler

case class GetLatLonCoordinate(plssid: String, lat: Double, lon: Double)

case class GetLatLonResponse(
  trs: String,
  generatedplss: List[String],
  coordinates: List[GetLatLonCoordinate],
  status: String) {
  def isSuccessful: Boolean =
    status == "success"
}

object GetLatLon {
  val protocol = HTTPS
  val host = "gis.blm.gov"
  val path =
    "/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI" ++
      "/MapServer/exts/CadastralSpecialServices/GetLatLon"

  implicit val getLanLonCoordinateDecoder: Decoder[GetLatLonCoordinate] =
    deriveDecoder

  implicit val getLatLonResponseDecoder: Decoder[GetLatLonResponse] =
    deriveDecoder

  lazy val baseRequest =
    HttpRequest().
      withProtocol(protocol).
      withHost(host).
      withPath(path)

  def associateError[F[_]:Functor](vftrs: Vector[F[TRS]], th: Throwable):
      Vector[F[Either[Throwable, (Double, Double)]]] = {
    val eth: Either[Throwable, (Double, Double)] = Left(th)

    vftrs map (_ map (_ => eth))
  }

  def associateCoordinates[F[_]:Functor](
    vftrs: Vector[F[TRS]], gllr: GetLatLonResponse):
      Vector[F[Either[Throwable, (Double, Double)]]] = {
    if (gllr.isSuccessful) {
      val coords: Map[Int, Either[Throwable, (Double, Double)]] =
        gllr.coordinates.toVector.map { coord =>
          val i = gllr.generatedplss.indexOf(coord.plssid)

          (i -> Right((coord.lat, coord.lon)))
        }.toMap

      vftrs.zipWithIndex map {
        case (ftrs, i) =>
          val ftrs1: F[Either[Throwable, (Double, Double)]] =
            if (coords.contains(i))
              ftrs map (_ => coords(i))
            else
              ftrs map (_ =>
                Left(
                  new RuntimeException(
                    "BLM Navigator GetLatLon lookup failure")))
          ftrs1
      }
    } else {
      associateError(
        vftrs,
        new RuntimeException("BLM Navigator GetLatLon service failure"))
    }
  }

  def associateLatLonResponse[F[_]:Functor](
    vftrs: Vector[F[TRS]], rsp: Either[Throwable, GetLatLonResponse]):
      Vector[F[Either[Throwable, (Double, Double)]]] = {
    rsp.fold(
      err => associateError(vftrs, err),
      gllr => associateCoordinates(vftrs, gllr))
  }

  def sendRequest[F[_]:Comonad](
    sftrs: Seq[F[TRS]])(
    implicit timeout: FiniteDuration, sch: Scheduler):
      Future[Vector[F[Either[Throwable, (Double, Double)]]]] = {
    if (!sftrs.isEmpty) {
      val vftrs = sftrs.toVector

      val trsQuery = vftrs.map(_.extract.plssId).mkString("|")

      baseRequest.
        withTimeout(timeout).
        withQueryParameters("trs"-> trsQuery, "f" -> "pjson").
        send().
        map(Right(_)).
        recover {
          case th: Throwable => Left(th)
        }.
        map { eth =>
          val rsp =
            eth match {
              case Left(t@_) =>
                println(s"$t")
                Left(t)

              case Right(h@_) =>
                synchronized {
                  println(s"${h.body}")
                }
                decode[GetLatLonResponse](h.body) match {
                  case Left(err@_) => Left(err)
                  case Right(gllr@_) => Right(gllr)
                }
            }

          associateLatLonResponse(vftrs, rsp)
        }
    } else {
      Future.successful(Vector.empty)
    }
  }
}
