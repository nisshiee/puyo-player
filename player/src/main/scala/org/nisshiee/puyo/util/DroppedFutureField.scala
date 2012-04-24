package org.nisshiee.puyo.util

import scalaz._, Scalaz._

import org.nisshiee.puyo.core._, Puyoz._
import PuyoHelper._

case class DroppedFutureField private (puyos: Map[InFieldPoint, Puyo]) {

  def apply(ifp: InFieldPoint): Option[Puyo] = puyos.get(ifp)
}

object DroppedFutureField {

  def apply(pb: PuyoBlock, a: Action, puyos: Map[InFieldPoint, Puyo])(implicit f: Field): DroppedFutureField = {
    val droppings = a match {
      case Action(Up, c) => List((pb.base, c), (pb.sub, c))
      case Action(Down, c) => List((pb.sub, c), (pb.base, c))
      case Action(Right, c) => List((pb.base, c), (pb.sub, c + 1))
      case Action(Left, c) => List((pb.base, c), (pb.sub, c - 1))
    }

    def drop: (Map[InFieldPoint, Puyo], (Puyo, Int)) => Map[InFieldPoint, Puyo] = {
      case (puyos, (p, x)) => (for {
        y <- nextPuyoHeight(x, puyos)
        ifp <- Point(x, y).in
      } yield {
        puyos + (ifp -> p)
      }) | puyos
    }

    droppings.foldl(puyos)(drop) |> { DroppedFutureField(_) }
  }
}
  