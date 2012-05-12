package org.nisshiee.puyo.player

import org.nisshiee.puyo.core._, Puyoz._
import org.nisshiee.puyo.util._, PuyoHelper._
import scalaz._, Scalaz._

case class Evaluation(searchPoint: Int, pruningPoint: Int)

case class FutureBoard(of: OjamaFutureField, q: List[Int], pbs: List[PuyoBlock], e: Evaluation)

class NisshieePlayer(name: String) extends Player[Int](name) {

  def init = 1

  val pruningCount = 5

  /**
   * 1ターン目の予測・評価
   */
  def first(c: (Action, Board)): FutureBoard = {
    val (action, board) = c
    implicit val f: Field = board.field
    val df = DroppedFutureField(board.current, action)
    val (vf, tmpAttack) = VanishedFutureField(df)
    val (of, queue, attack) = OjamaFutureField(vf, board.ojamaQueue, tmpAttack)

    val pbs = List(board.next, board.nextNext)

    val evaluation =
      if (isDead(df.puyos))
        Evaluation(attack, -10000)
      else {
        val highest = of.puyos.keys ∘ (_.p.y) |> (_.max)
        Evaluation(attack, -highest)
      }

    FutureBoard(of, queue, pbs, evaluation)
  }

  /**
   * 2ターン目以降の予測・評価を再帰的に計算
   */
  def future(fb: FutureBoard)(implicit f: Field): Evaluation = {
    Evaluation(0, 0)
  }

  def choose(b: Board)(implicit f: Field): Action = {
    // まず1ターン分の予測
    val firstChoices: List[(Action, FutureBoard)] =
      allChoices(b.current) ∘ (_ -> b) ∘ { c => first(c) |> (c._1 -> _) }
    val prunedChoices: List[(Action, FutureBoard)] =
      firstChoices sortBy {
        case (_, FutureBoard(_, _, _, Evaluation(_, p))) => p
      } take pruningCount

    // 計算並列化と、評価を1ターン目だけ別にする可能性を考慮し、
    // 1ターン目を評価してから、2ターン目以降を再起処理という流れに。
    (prunedChoices ∘ {
      case (a, fb) => (a, future(fb))
    } maxBy {
      case (_, Evaluation(s, _)) => s
    }) |> {
      case (a, _) => a
    }
  }

  def action(
    myBoard: Board,
    enemyBoard: Board,
    turn: Int
  ): (Action, Int) = {
    implicit val f = myBoard.field
    "=== turn " |+| turn.toString |+| " ===" |> (_.println)

    choose(myBoard) |> (_ -> (turn + 1))
  }
}
