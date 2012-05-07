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

  def allIfp(implicit f: Field): List[InFieldPoint] = (for {
    x <- 0 until f.width
    ifp <- ifpColumn(x)
  } yield ifp).toList

  def nextIfp(base: InFieldPoint)(implicit f: Field): List[InFieldPoint] =
    List(Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0)) ∘ {
      r => base.p |+| r |> (_.in)
    } |> (_.flatten)

  def connectPuyo(puyos: Map[InFieldPoint, Puyo])(implicit f: Field): Set[(Set[InFieldPoint], Puyo)] = {
    def poprec: (
      (Map[InFieldPoint, Puyo], Set[InFieldPoint], Puyo)
      ,InFieldPoint
    ) => (Map[InFieldPoint, Puyo], Set[InFieldPoint], Puyo) = {
      case ((rest, acc, puyo), ifp) => rest.get(ifp) match {
        case Some(p) if (p ≟ puyo) => {
          val next = (rest - ifp, acc + ifp, puyo)
          nextIfp(ifp).foldl(next)(poprec)
        }
        case _ => (rest, acc, puyo)
      }
    }

    def foldfun: (
      (Map[InFieldPoint, Puyo], Set[(Set[InFieldPoint], Puyo)])
      ,InFieldPoint
    ) => (Map[InFieldPoint, Puyo], Set[(Set[InFieldPoint], Puyo)]) = {
      case ((rest, acc), cursor) =>
        rest.get(cursor) match {
          case Some(Ojama) => (rest, acc)
          case Some(puyo) => {
            val poprecAcc = (rest - cursor, Set(cursor), puyo)
            nextIfp(cursor).foldl(poprecAcc)(poprec) |> {
              case (rest, con, puyo) => (rest, acc + (con -> puyo))
            }
          }
          case None => (rest, acc)
        }
    }

    val acc = (puyos, Set[(Set[InFieldPoint], Puyo)]())
    allIfp.foldl(acc)(foldfun) |> { case (_, result) => result }
  }

  def dropAll(puyos: Map[InFieldPoint, Puyo])(implicit f: Field): Map[InFieldPoint, Puyo] = {
    def dropColumn(x: Int): List[(InFieldPoint, Puyo)] = {
      val l = ifpColumn(x) ∘ (puyos.get(_)) |> (_.flatten.zipWithIndex)
      l ∘ {
        case (puyo, y) => Point(x, y).in ∘ (_ -> puyo)
      } |> (_.flatten)
    }

    (for {
      x <- 0 until f.width
      t <- dropColumn(x)
    } yield t).toMap
  }

  def allChoices(pb: PuyoBlock)(implicit f: Field): List[Action] = {

    def forDirection: Direction => List[Action] = {
      case Right => (0 until (f.width - 1)) ∘ { x => Action.check(Right, x) } |> (_.toList.flatten)
      case Left => (1 until f.width) ∘ { x => Action.check(Left, x) } |> (_.toList.flatten)
      case d => (0 until f.width) ∘ { x => Action.check(d, x) } |> (_.toList.flatten)
    }

    def directions: PuyoBlock => List[Direction] = { pb =>
      if (pb.base ≟ pb.sub)
        List(Up, Right)
      else
        List(Up, Down, Right, Left)
    }

    directions(pb) >>= forDirection
  }
}
