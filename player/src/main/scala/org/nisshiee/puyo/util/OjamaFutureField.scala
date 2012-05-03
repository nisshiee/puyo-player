package org.nisshiee.puyo.util

import scalaz._, Scalaz._

import org.nisshiee.puyo.core._, Puyoz._

case class OjamaFutureField private (puyos: Map[InFieldPoint, Puyo])

object OjamaFutureField {

  def apply(
    vf: VanishedFutureField
    ,ojamaQueue: List[Int]
    ,attack: Int
  )(implicit f: Field): (OjamaFutureField, List[Int], Int) =
    (OjamaFutureField(Map()), List(), 0)
}
