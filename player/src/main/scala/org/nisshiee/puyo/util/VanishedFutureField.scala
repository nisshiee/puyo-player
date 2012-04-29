package org.nisshiee.puyo.util

import scalaz._, Scalaz._

import org.nisshiee.puyo.core._, Puyoz._

case class VanishedFutureField private (puyos: Map[InFieldPoint, Puyo])

object VanishedFutureField {

  def apply(df: DroppedFutureField)(implicit f: Field): VanishedFutureField =
    VanishedFutureField(Map[InFieldPoint, Puyo]())
}
