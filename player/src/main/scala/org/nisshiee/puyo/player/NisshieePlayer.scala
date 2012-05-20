package org.nisshiee.puyo.player

import org.nisshiee.puyo.core._, Puyoz._
import org.nisshiee.puyo.util._, PuyoHelper._
import scalaz._, Scalaz._

case class Evaluation(searchPoint: Int, pruningPoint: Int)

case class FutureBoard(of: OjamaFutureField, q: List[Int], pbs: List[PuyoBlock], e: Evaluation)

class NisshieePlayer(name: String) extends Player[Int](name) {

  def init = 1

  val pruningCount = 8

  /**
   * 1ターン目の予測・評価
   */
  def first(c: (Action, Board)): FutureBoard = {
    val (action, board) = c
    implicit val f: Field = board.field
    val df = DroppedFutureField(board.current, action)
    val (vf, at) = VanishedFutureField(df)
    val (of, queue, _) = OjamaFutureField(vf, board.ojamaQueue, at)

    val pbs = List(board.next, board.nextNext)

    // 評価 ここから ----------
    val evaluation = {

      val dead = if (isDead(df.puyos)) -10000 else 0
      val connect = {
        val connected = connectPuyo(df.puyos).toList
        connected ∘ {
          case (s, _) => s.size
        } ∘ {
          case 2 => 1
          case i if i >= 3 => 2
          case _ => 0
        } |> (_.sum)
      }
      val maxY = of.puyos.keys ∘ (_.p.y) |> { ps =>
        if (ps.isEmpty) 0 else ps.max
      }
      val pessimisticMaxY = maxY + (queue.sum / f.width)
      val highest = {
        if (maxY > f.deadLine / 2)
          -maxY
        else
          0
      }
      val smallVanish = {
        val vanish = df.puyos.size - vf.puyos.size
        (pessimisticMaxY <= f.deadLine / 3 * 2, vanish > 0, at < 30) match {
          case (true, true, true) => -1
          case _ => 0
        }
      }
      val attack = if (at > 5) at else 0
      val nearDead = if (f.deadLine - pessimisticMaxY < 2) -1 else 0

      val searchPoint = 30 * attack + connect + 100 * nearDead + dead
      val pruningPoint = dead + 10 * connect + 10 * highest + 100 * smallVanish
      Evaluation(searchPoint, pruningPoint)
    }
    // 評価 ここまで ----------

    FutureBoard(of, queue, pbs, evaluation)
  }

  def futureStep(t: (FutureBoard, Action))(implicit f: Field): FutureBoard = t match {
    case (FutureBoard(lastOf, lastQ, pb :: nextPb, Evaluation(ls, _)), action) => {
      val df = DroppedFutureField(pb, action, lastOf)
      val (vf, at) = VanishedFutureField(df)
      val (of, queue, _) = OjamaFutureField(vf, lastQ, at)

      // 評価 ここから ----------
      val evaluation = {

        val dead = if (isDead(df.puyos)) -10000 else 0
        val connect = {
          val connected = connectPuyo(df.puyos).toList
          connected ∘ {
            case (s, _) => s.size
          } ∘ {
            case 2 => 1
            case i if i >= 3 => 2
            case _ => 0
          } |> (_.sum)
        }
        val highest = {
          val maxY = of.puyos.keys ∘ (_.p.y) |> { ps =>
            if (ps.isEmpty) 0 else ps.max
          }
          if (maxY > f.deadLine / 2)
            -maxY
          else
            0
        }
        val attack = if (at > 5) at else 0

        val searchPoint = 30 * attack + connect + dead
        val pruningPoint = dead + 10 * connect + 10 * highest
        Evaluation(ls + searchPoint, pruningPoint)
      }
      // 評価 ここまで ----------

      FutureBoard(of, queue, nextPb, evaluation)
    }
  }

  /**
   * 2ターン目以降の予測・評価を再帰的に計算
   */
  def future(fb: FutureBoard)(implicit f: Field): Evaluation =
    
    fb match {
      case FutureBoard(_, _, Nil, e) => e
      case FutureBoard(_, _, pb :: _, _) => {
        val choices = allChoices(pb) ∘ (fb -> _) ∘ futureStep
        val prunedChoices = choices sortBy {
          case FutureBoard(_, _, _, Evaluation(_, p)) => -p
        } take pruningCount

        prunedChoices ∘ future maxBy {
          case Evaluation(s, _) => s
        }
      }
    }

  def choose(b: Board)(implicit f: Field): Action = {
    // まず1ターン分の予測
    val firstChoices: List[(Action, FutureBoard)] =
      allChoices(b.current) ∘ (_ -> b) ∘ { c => first(c) |> (c._1 -> _) }
    val prunedChoices: List[(Action, FutureBoard)] =
      firstChoices sortBy {
        case (_, FutureBoard(_, _, _, Evaluation(_, p))) => -p
      } take pruningCount

    // 計算並列化と、評価を1ターン目だけ別にする可能性を考慮し、
    // 1ターン目を評価してから、2ターン目以降を再起処理という流れに。
    (prunedChoices.par map {
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
    choose(myBoard) |> (_ -> (turn + 1))
  }
}
