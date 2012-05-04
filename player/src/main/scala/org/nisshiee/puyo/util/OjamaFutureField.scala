package org.nisshiee.puyo.util

import scalaz._, Scalaz._

import org.nisshiee.puyo.core._, Puyoz._
import PuyoHelper._

case class OjamaFutureField private (puyos: Map[InFieldPoint, Puyo])

object OjamaFutureField {

  def cancel: (List[Int], Int) => (List[Int], Int) = {
    case (Nil, n) => (Nil, n)
    case (h :: t, n) if n > h => (0.pure[List], 0) |+| cancel(t, n - h)
    case (h :: t, n) => ((h - n) :: t, 0)
  }

  def ojamaRows(count: Int)(implicit f: Field): Int = {
    val r = count / f.width
    if (count % f.width > f.width / 2)
      r + 1
    else
      r
  }

  def dropOjamaOne(
    puyos: Map[InFieldPoint, Puyo], x: Int
  )(implicit f: Field): Map[InFieldPoint, Puyo] = (for {
    y <- nextPuyoHeight(x, puyos)
    ifp <- Point(x, y).in
  } yield puyos + (ifp -> Ojama)) | puyos

  def dropOjamaOneline(
    puyos: Map[InFieldPoint, Puyo]
  )(implicit f: Field): Map[InFieldPoint, Puyo] =
    (0 until f.width).foldl(puyos)(dropOjamaOne)

  def dropOjama(
    puyos: Map[InFieldPoint, Puyo], rows: Int
  )(implicit f: Field): Map[InFieldPoint, Puyo] =
    (0 until rows).foldl(puyos) { (puyos, _) => dropOjamaOneline(puyos) }

  def apply(
    vf: VanishedFutureField
    ,ojamaQueue: List[Int]
    ,attack: Int
  )(implicit f: Field): (OjamaFutureField, List[Int], Int) =
    cancel(ojamaQueue, attack) match {
      case (h :: t, n) => dropOjama(vf.puyos, ojamaRows(h)) |> { puyos =>
        (OjamaFutureField(puyos), t, n)
      }
      case (Nil, n) => (OjamaFutureField(vf.puyos), Nil, n)
    }
}
