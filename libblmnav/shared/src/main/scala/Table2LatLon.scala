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

import scala.concurrent._
import scala.concurrent.duration._
import scala.language.higherKinds
import scala.util.{ Failure, Success }

import cats._
import cats.data._
import cats.implicits._
import monix.execution.Scheduler

object Table2LatLon {

  type LatLonResult = Either[NonEmptyList[Error], (Double, Double)]

  def apply[A](
    reader: TableReader[A],
    channel: Channel[IndexedValue[LatLonResult]],
    batchSize: Int)(
    implicit timeout: FiniteDuration, sch: Scheduler):
      A => Unit = { a =>
    type AccState = (reader.State, Vector[IndexedValue[TRS]])

    /* state transformer producing validated TRS values */
    val trsRecord =
      (reader.nextRecord map {
         case (index@_, vrow@_) =>
           val vrow1 =
             vrow.
               leftMap(_ map (TableReaderError(_): Error)).
               andThen { r =>
                 TRS(r).
                   leftMap(_ map (TRSError(_): Error))
               }

           IndexedValue(index, vrow1)
       })

    /* State transformer with reader state separated from reader value. This is done
     * to allow augmenting the state with accumulators to produce batched TRS
     * values after the original reader produces a None value (which would
     * otherwise make the accumulation vectors unavailable). */
    val trsRecord1 = loweredState(trsRecord)

    val M = MonadState[State[Option[reader.State], ?], Option[reader.State]]

    type Batches =
      (Vector[IndexedValue[NonEmptyList[Error]]], Vector[IndexedValue[TRS]])

    /* state transformer producing batched validated TRS values */
    val trsBatch =
      M.tailRecM[Batches,Batches]((Vector.empty, Vector.empty)) {
        case accs@(errs@_, trss@_) =>
          /* the following map is where we would fail to produce the remaining accumulated
           * values if the values produced by the state transformer were wrapped
           * in an Option (rather than Eval) */
          trsRecord1 map {
            case Some(iv) =>
              iv.extract match {
                case Validated.Invalid(err) =>
                  Left((errs :+ iv.as(err), trss))

                case Validated.Valid(trs) =>
                  val accs1 = (errs, trss :+ iv.as(trs))

                  Either.cond(accs1._2.length >= batchSize, accs1, accs1)
              }

            case None =>
              Right(accs)
          }
      }

    /* process batches of validated TRS values */
    val results =
      M.tailRecM[Future[Boolean], Future[Boolean]](
        Promise[Boolean].success(true).future) { f =>
        trsBatch map {
          case (errs@_, trss@_) =>
            /* send requests for validated TRS values */
            val p = Promise[Boolean]

            GetLatLon.
              sendRequest(trss).
              onComplete {
                case Success(ivs) =>
                  p.success(true)
                  ivs foreach { iv =>
                    iv.extract match {
                      case Right(ll) =>
                        channel.push(Some(iv.as(Right(ll))))
                      case Left(err) =>
                        channel.push(
                          Some(
                            iv.as(Left(NonEmptyList(ServiceError(err), Nil)))))
                    }
                  }
                case Failure(err) =>
                  p.success(false)
              }

            /* propagate TRS validation errors */
            errs foreach (err => channel.push(Some(err map (Left(_)))))

            val f1 = f flatMap (b => p.future map (b && _))

            Either.cond(trss.length == 0 && errs.length == 0, f1, f1)
        }
      }

    results.run(Some(reader.initial(a))).value._2 onComplete {
      case Success(_) => channel.push(None)
      case Failure(_) => assume(false)
    }
  }

  def loweredState[F[_]:FlatMap,S,A](st: StateT[F, S, A]): State[F[S], F[A]] = {
    State { fs =>
      val fsa = fs flatMap st.run

      (fsa map ( _._1), fsa map (_._2))
    }
  }

  sealed trait Error {
    def message: String
  }

  case class TableReaderError(err: TableReader.Error) extends Error {
    override def message: String =
      err match {
        case TableReader.InvalidHeader(columns@_) =>
          ""
        case TableReader.CellType(column@_, expected@_) =>
          ""
      }
  }

  case class TRSError(err: TRS.Error) extends Error {
    override def message: String = err.message
  }

  case class ServiceError(th: Throwable) extends Error {
    override def message: String = th.getMessage
  }
}
