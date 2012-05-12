package org.nisshiee.puyo.util

import scalaz._, Scalaz._

import org.nisshiee.puyo.core._, Puyoz._
import PuyoHelper._

case class VanishedFutureField private (puyos: Map[InFieldPoint, Puyo]) {

  def apply(ifp: InFieldPoint): Option[Puyo] = puyos.get(ifp)
}

object VanishedFutureField {


  /**
   * (ぷよMap, 連鎖数) => (ぷよMap, 消去得点)
   */
  def chainStep(
    puyos: Map[InFieldPoint, Puyo], chainCount: Int
  )(implicit f: Field): Option[(Map[InFieldPoint, Puyo], Int)] = {
    val vanishing = connectPuyo(puyos) filter {
      case (ifps, _) => ifps.size >= 4
    }

    if (vanishing.isEmpty) None
    else {
      val vanishingColor = vanishing.foldl(List[InFieldPoint]()) {
        case (acc, (points, _)) => acc ++ points
      }
      val vanishingOjama = vanishingColor ∘ nextIfp |> (_.flatten) filter (puyos.get(_) ≟ Ojama.some)
      val nextPuyos =  puyos -- vanishingColor -- vanishingOjama |> dropAll
      val point = vanishPoint(vanishing, chainCount)
      (nextPuyos, point).some
    }
  }

  // ============================================================
  // ここから消去得点計算系関数
  // ============================================================

  def vanishCount: Set[(Set[InFieldPoint], Puyo)] => Int = { con =>
    con.toList ∘ { case (points, _) => points.size } |> (_.sum)
  }

  def chainBonus: Int => Int = {
    case 2 => 8
    case 3 => 16
    case c if c >= 4 => 32 * (c - 3)
    case _ => 0
  }

  def multiColorBonus: Set[(Set[InFieldPoint], Puyo)] => Int = { con =>
    con ∘ (_._2) |> (_.size) match {
      case 2 => 3
      case 3 => 6
      case 4 => 12
      case 5 => 24
      case _ => 0
    }
  }

  def manyConnectBonus: Set[(Set[InFieldPoint], Puyo)] => Int = { con =>
    con.toList ∘ { case (points, _) => points.size } ∘ {
      case c if (c >= 5 && c <= 10) => c - 3
      case c if c >= 11 => 10
      case _ => 0
    } |> (_.sum)
  }

  def vanishPoint: (Set[(Set[InFieldPoint], Puyo)], Int) => Int = {
    (con, chainCount) => {
      val bonus = (chainBonus(chainCount) + multiColorBonus(con) + manyConnectBonus(con)) match {
        case 0 => 1
        case b => b
      }
      vanishCount(con) * 10 * bonus
    }
  }

  // ============================================================
  // ここまで消去得点計算系関数
  // ============================================================

  def apply(df: DroppedFutureField)(implicit f: Field): (VanishedFutureField, Int) = {
    def chainrec(puyos: Map[InFieldPoint, Puyo], chainCount: Int): (Map[InFieldPoint, Puyo], Int) = {
      chainStep(puyos, chainCount) match {
        case Some(_ @ (next, point)) => chainrec(next, chainCount + 1) |> {
          case (resultPuyo, resultPoint) => (resultPuyo, resultPoint + point)
        }
        case None => (puyos, 0)
      }
    }

    val (nextPuyos, point) = chainrec(df.puyos, 1)
    if (nextPuyos.isEmpty)
      (VanishedFutureField(nextPuyos), (point + 2100) / 70)
    else
      (VanishedFutureField(nextPuyos), point / 70)
  }
}
