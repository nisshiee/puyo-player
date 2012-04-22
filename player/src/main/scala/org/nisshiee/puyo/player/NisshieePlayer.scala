package org.nisshiee.puyo.player

import org.nisshiee.puyo.core._, Puyoz._
import scalaz._, Scalaz._

class NisshieePlayer(name: String) extends Player[Int](name) {

  def init = 1

  def action(
    myBoard: Board,
    enemyBoard: Board,
    turn: Int
  ): (Action, Int) = {
    implicit val f = myBoard.field
    "=== turn " |+| turn.toString |+| " ===" |> (_.println)
    Action.check(Right, 2) ∘ (a => (a, turn + 1)) err "error"
  }
}
