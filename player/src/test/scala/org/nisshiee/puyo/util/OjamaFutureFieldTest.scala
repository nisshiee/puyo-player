package org.nisshiee.puyo.util

import org.specs2._, mock._
import scalaz._, Scalaz._

import org.nisshiee.puyo.core._, Puyoz._

class OjamaFutureFieldTest extends Specification { def is =

  "OjamaFutureFieldクラスのテスト"                          ^
    "VanishedFutureFieldからOjamaFutureFieldを算出"         ^
      "OjamaQueueが空のケース"                              ! SizedFieldMock().e1^
      "OjamaQueueから1つ消費されること"                     ! SizedFieldMock().e2^
      "おじゃまぷよが1個降る場合は無視"                     ! SizedFieldMock().e3^
      "おじゃまぷよが3個降る場合は無視"                     ! SizedFieldMock().e4^
      "おじゃまぷよが4個降る場合は1列降らす"                ! SizedFieldMock().e5^
      "おじゃまぷよが6個降る場合は1列降らす"                ! SizedFieldMock().e6^
      "おじゃまぷよが7個降る場合は1列降らす"                ! SizedFieldMock().e7^
      "おじゃまぷよが9個降る場合は1列降らす"                ! SizedFieldMock().e8^
      "おじゃまぷよが10個降る場合は2列降らす"               ! SizedFieldMock().e9^
      "おじゃまぷよが12個降る場合は2列降らす"               ! SizedFieldMock().e10^
      "おじゃまぷよが15個降る場合は2列降らす"               ! SizedFieldMock().e11^
      "おじゃまぷよが16個降る場合は3列降らす"               ! SizedFieldMock().e12^
      "おじゃまぷよを30個降らす"                            ! SizedFieldMock().e13^
      "OjamaQueueが空なら全弾攻撃になる"                    ! SizedFieldMock().e14^
      "OjamaQueueの1番目の一部を相殺"                       ! SizedFieldMock().e15^
      "OjamaQueueの1番目は空、2番目の一部を相殺"            ! SizedFieldMock().e16^
      "OjamaQueueの1,2番目は空、3番目の一部を相殺"          ! SizedFieldMock().e17^
      "OjamaQueueの1番目を全部、2番目の一部を相殺"          ! SizedFieldMock().e18^
      "OjamaQueueの1,2番目を全部、3番目の一部を相殺"        ! SizedFieldMock().e19^
      "OjamaQueueの1,2,3番目全部でぴったり相殺"             ! SizedFieldMock().e20^
      "OjamaQueueの1,2,3番目全部相殺＆余剰分攻撃"           ! SizedFieldMock().e21^
                                                            p^
    "cancel関数のテスト"                                    ^
      "OjamaQueueが空なら全弾攻撃になる"                    ! e22^
      "OjamaQueueの1番目の一部を相殺"                       ! e23^
      "OjamaQueueの1番目は空、2番目の一部を相殺"            ! e24^
      "OjamaQueueの1,2番目は空、3番目の一部を相殺"          ! e25^
      "OjamaQueueの1番目を全部、2番目の一部を相殺"          ! e26^
      "OjamaQueueの1,2番目を全部、3番目の一部を相殺"        ! e27^
      "OjamaQueueの1,2,3番目全部でぴったり相殺"             ! e28^
      "OjamaQueueの1,2,3番目全部相殺＆余剰分攻撃"           ! e29^
                                                            end

  case class SizedFieldMock() extends Mockito {

    implicit val f = mock[Field]
    f.width returns 6
    f.height returns 14
    f.deadLine returns 12

    val vf = mock[VanishedFutureField]

    private def setPuyo(puyos: Map[InFieldPoint, Puyo]) = {
      vf.puyos returns puyos
      vf.apply(any[InFieldPoint]) answers {
        case ifp: InFieldPoint => puyos.get(ifp)
        case _ => None
      }
    }

    def e1 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(0, 0, 0, 0)
      val attack = 0

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== puyos) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e2 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(0, 5, 0, 0)
      val attack = 0

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== puyos) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e3 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(1, 0, 0, 0)
      val attack = 0

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== puyos) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e4 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(3, 0, 0, 0)
      val attack = 0

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== puyos) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e5 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(4, 0, 0, 0)
      val attack = 0

      val expected = puyos ++ Map(
        Point(0, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 0).in ∘ (_ -> Ojama) err "error"
      )

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== expected) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e6 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(6, 0, 0, 0)
      val attack = 0

      val expected = puyos ++ Map(
        Point(0, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 0).in ∘ (_ -> Ojama) err "error"
      )

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== expected) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e7 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(7, 0, 0, 0)
      val attack = 0

      val expected = puyos ++ Map(
        Point(0, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 0).in ∘ (_ -> Ojama) err "error"
      )

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== expected) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e8 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(9, 0, 0, 0)
      val attack = 0

      val expected = puyos ++ Map(
        Point(0, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 0).in ∘ (_ -> Ojama) err "error"
      )

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== expected) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e9 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(10, 0, 0, 0)
      val attack = 0

      val expected = puyos ++ Map(
        Point(0, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(0, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 1).in ∘ (_ -> Ojama) err "error"
      )

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== expected) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e10 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(12, 0, 0, 0)
      val attack = 0

      val expected = puyos ++ Map(
        Point(0, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(0, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 1).in ∘ (_ -> Ojama) err "error"
      )

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== expected) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e11 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(15, 0, 0, 0)
      val attack = 0

      val expected = puyos ++ Map(
        Point(0, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(0, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 1).in ∘ (_ -> Ojama) err "error"
      )

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== expected) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e12 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(16, 0, 0, 0)
      val attack = 0

      val expected = puyos ++ Map(
        Point(0, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(0, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(0, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 3).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 2).in ∘ (_ -> Ojama) err "error"
      )

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== expected) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e13 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(30, 0, 0, 0)
      val attack = 0

      val expected = puyos ++ Map(
        Point(0, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(0, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(0, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 3).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 2).in ∘ (_ -> Ojama) err "error"
        ,Point(0, 3).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 3).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 4).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 3).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 3).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 3).in ∘ (_ -> Ojama) err "error"
        ,Point(0, 4).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 4).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 5).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 4).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 4).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 4).in ∘ (_ -> Ojama) err "error"
      )

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== expected) and
        (nextQueue must_== ojamaQueue.tail) and
        (attack must_== 0)
    }

    def e14 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(0, 0, 0, 0)
      val attack = 10

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== puyos) and
        (nextQueue must_== ojamaQueue.tail) and
        (atk must_== 10)
    }

    def e15 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(15, 0, 0, 0)
      val attack = 10

      val expected = puyos ++ Map(
        Point(0, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(1, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 0).in ∘ (_ -> Ojama) err "error"
        ,Point(5, 0).in ∘ (_ -> Ojama) err "error"
      )

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== expected) and
        (nextQueue must_== ojamaQueue.tail) and
        (atk must_== 0)
    }

    def e16 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(0, 15, 0, 0)
      val attack = 10

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== puyos) and
        (nextQueue must_== List(5, 0, 0)) and
        (atk must_== 0)
    }

    def e17 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(0, 0, 15, 0)
      val attack = 10

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== puyos) and
        (nextQueue must_== List(0, 5, 0)) and
        (atk must_== 0)
    }

    def e18 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(5, 15, 0, 0)
      val attack = 10

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== puyos) and
        (nextQueue must_== List(10, 0, 0)) and
        (atk must_== 0)
    }

    def e19 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(5, 5, 10, 0)
      val attack = 15

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== puyos) and
        (nextQueue must_== List(0, 5, 0)) and
        (atk must_== 0)
    }

    def e20 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(5, 5, 5, 0)
      val attack = 15

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== puyos) and
        (nextQueue must_== List(0, 0, 0)) and
        (atk must_== 0)
    }

    def e21 = {
      val puyos = Map(
        Point(2, 0).in ∘ (_ -> Red) err "error"
      )
      setPuyo(puyos)
      val ojamaQueue = List(5, 5, 5, 0)
      val attack = 30

      val (of, nextQueue, atk) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== puyos) and
        (nextQueue must_== List(0, 0, 0)) and
        (atk must_== 15)
    }
  }

  def e22 = OjamaFutureField.cancel(List(0, 0, 0, 0), 10) must_== (List(0, 0, 0, 0), 10)

  def e23 = OjamaFutureField.cancel(List(20, 0, 0, 0), 10) must_== (List(10, 0, 0, 0), 0)

  def e24 = OjamaFutureField.cancel(List(0, 20, 0, 0), 10) must_== (List(0, 10, 0, 0), 0)

  def e25 = OjamaFutureField.cancel(List(0, 0, 20, 0), 10) must_== (List(0, 0, 10, 0), 0)

  def e26 = OjamaFutureField.cancel(List(5, 15, 0, 0), 10) must_== (List(0, 10, 0, 0), 0)

  def e27 = OjamaFutureField.cancel(List(5, 5, 15, 0), 15) must_== (List(0, 0, 10, 0), 0)

  def e28 = OjamaFutureField.cancel(List(5, 5, 5, 0), 15) must_== (List(0, 0, 0, 0), 0)

  def e29 = OjamaFutureField.cancel(List(5, 5, 5, 0), 20) must_== (List(0, 0, 0, 0), 5)
}
