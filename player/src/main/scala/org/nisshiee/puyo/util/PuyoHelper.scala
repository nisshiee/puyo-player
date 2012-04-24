package org.nisshiee.puyo.util

import scalaz._, Scalaz._

import org.nisshiee.puyo.core._, Puyoz._

object PuyoHelper {

  def ifpColumn(x: Int)(implicit f: Field): List[InFieldPoint] =
    (0 until f.height) ∘ { Point(x, _) } ∘ (_.in) |> (_.flatten) |> (_.toList)

  def nextPuyoHeight(x: Int, puyos: Map[InFieldPoint, Puyo])(implicit f: Field): Option[Int] =
    ifpColumn(x) find { puyos.get(_).isEmpty } map (_.p.y)

  def isEmptyColumn(x: Int, puyos: Map[InFieldPoint, Puyo])(implicit f: Field): Boolean =
    ifpColumn(x) forall { puyos.get(_).isEmpty }
}
