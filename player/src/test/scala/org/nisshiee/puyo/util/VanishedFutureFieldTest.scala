package org.nisshiee.puyo.util

import org.specs2._, mock._
import scalaz._, Scalaz._

import org.nisshiee.puyo.core._, Puyoz._

class VanishedFutureFieldTest extends Specification { def is =

  "VanishedFutureFieldクラスのテスト"                       ^
    "DroppedFutureFieldからVanishedFutureFieldを算出"       ^
      "1つもぷよが消えないパターン"                         ! SizedFieldMock().e1^
      "色ぷよが4個1組消えて落下なし"                        ! SizedFieldMock().e2^
      "色ぷよが5個1組消えて落下なし"                        ! SizedFieldMock().e3^
      "色ぷよ4個がおじゃま1個ごと消えて落下なし"            ! SizedFieldMock().e4^
      "色ぷよ4個2組消えて落下なし"                          ! SizedFieldMock().e5^
      "色ぷよ4個1組消えて1個落下、連鎖なし"                 ! SizedFieldMock().e6^
      "2連鎖"                                               ! SizedFieldMock().e7^
      "2連鎖ダブル"                                         ! SizedFieldMock().e8^
      "3連鎖"                                               ! SizedFieldMock().e9^
      "10連鎖"                                              ! SizedFieldMock().e10^
      "全消し"                                              ! SizedFieldMock().e11^
                                                            end

  case class SizedFieldMock() extends Mockito {

    implicit val f = mock[Field]
    f.width returns 6
    f.height returns 14
    f.deadLine returns 12

    val df = mock[DroppedFutureField]

    private def setPuyo(puyos: Map[InFieldPoint, Puyo]) = {
      df.puyos returns puyos
      df.apply(any[InFieldPoint]) answers {
        case ifp: InFieldPoint => puyos.get(ifp)
        case _ => None
      }
    }

    def e1 = {
      val puyos = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
        ,Point(2, 0).in ∘ (_ -> Blue) err "error"
      )
      setPuyo(puyos)

      val (vf, ojama) = VanishedFutureField(df)
      (vf.puyos must_== puyos) and (ojama must_== 0)
    }

    def e2 = {
      val puyos = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
        ,Point(0, 0).in ∘ (_ -> Red) err "error"
        ,Point(1, 1).in ∘ (_ -> Red) err "error"
        ,Point(1, 2).in ∘ (_ -> Red) err "error"
        ,Point(2, 0).in ∘ (_ -> Blue) err "error"
      )
      setPuyo(puyos)

      val expected = Map(
        Point(2, 0).in ∘ (_ -> Blue) err "error"
      )

      val (vf, ojama) = VanishedFutureField(df)
      (vf.puyos must_== expected) and (ojama must_== 0)
    }

    def e3 = {
      val puyos = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
        ,Point(0, 0).in ∘ (_ -> Red) err "error"
        ,Point(0, 1).in ∘ (_ -> Red) err "error"
        ,Point(1, 1).in ∘ (_ -> Red) err "error"
        ,Point(1, 2).in ∘ (_ -> Red) err "error"
        ,Point(2, 0).in ∘ (_ -> Blue) err "error"
      )
      setPuyo(puyos)

      val expected = Map(
        Point(2, 0).in ∘ (_ -> Blue) err "error"
      )

      val (vf, ojama) = VanishedFutureField(df)
      (vf.puyos must_== expected) and (ojama must_== 1)
    }

    def e4 = {
      val puyos = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
        ,Point(0, 0).in ∘ (_ -> Red) err "error"
        ,Point(1, 1).in ∘ (_ -> Red) err "error"
        ,Point(1, 2).in ∘ (_ -> Red) err "error"
        ,Point(0, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(2, 0).in ∘ (_ -> Blue) err "error"
      )
      setPuyo(puyos)

      val expected = Map(
        Point(2, 0).in ∘ (_ -> Blue) err "error"
      )

      val (vf, ojama) = VanishedFutureField(df)
      (vf.puyos must_== expected) and (ojama must_== 0)
    }

    def e5 = {
      val puyos = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
        ,Point(0, 0).in ∘ (_ -> Red) err "error"
        ,Point(1, 1).in ∘ (_ -> Red) err "error"
        ,Point(1, 2).in ∘ (_ -> Red) err "error"
        ,Point(2, 0).in ∘ (_ -> Blue) err "error"
        ,Point(2, 1).in ∘ (_ -> Blue) err "error"
        ,Point(2, 2).in ∘ (_ -> Blue) err "error"
        ,Point(3, 0).in ∘ (_ -> Blue) err "error"
        ,Point(4, 0).in ∘ (_ -> Green) err "error"
      )
      setPuyo(puyos)

      val expected = Map(
        Point(4, 0).in ∘ (_ -> Green) err "error"
      )

      val (vf, ojama) = VanishedFutureField(df)
      (vf.puyos must_== expected) and (ojama must_== 3)
    }

    def e6 = {
      val puyos = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
        ,Point(0, 0).in ∘ (_ -> Red) err "error"
        ,Point(1, 1).in ∘ (_ -> Red) err "error"
        ,Point(1, 2).in ∘ (_ -> Red) err "error"
        ,Point(2, 0).in ∘ (_ -> Blue) err "error"
        ,Point(1, 3).in ∘ (_ -> Green) err "error"
      )
      setPuyo(puyos)

      val expected = Map(
        Point(2, 0).in ∘ (_ -> Blue) err "error"
        ,Point(1, 0).in ∘ (_ -> Green) err "error"
      )

      val (vf, ojama) = VanishedFutureField(df)
      (vf.puyos must_== expected) and (ojama must_== 0)
    }

    def e7 = {
      val puyos = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
        ,Point(0, 0).in ∘ (_ -> Red) err "error"
        ,Point(1, 1).in ∘ (_ -> Red) err "error"
        ,Point(1, 2).in ∘ (_ -> Red) err "error"
        ,Point(2, 0).in ∘ (_ -> Blue) err "error"
        ,Point(2, 1).in ∘ (_ -> Blue) err "error"
        ,Point(3, 0).in ∘ (_ -> Blue) err "error"
        ,Point(1, 4).in ∘ (_ -> Blue) err "error"
        ,Point(1, 3).in ∘ (_ -> Green) err "error"
      )
      setPuyo(puyos)

      val expected = Map(
        Point(1, 0).in ∘ (_ -> Green) err "error"
      )

      val (vf, ojama) = VanishedFutureField(df)
      (vf.puyos must_== expected) and (ojama must_== 5)
    }

    def e8 = {
      val puyos = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
        ,Point(2, 0).in ∘ (_ -> Red) err "error"
        ,Point(2, 1).in ∘ (_ -> Red) err "error"
        ,Point(2, 2).in ∘ (_ -> Red) err "error"
        ,Point(2, 3).in ∘ (_ -> Blue) err "error"
        ,Point(3, 0).in ∘ (_ -> Blue) err "error"
        ,Point(4, 0).in ∘ (_ -> Blue) err "error"
        ,Point(5, 0).in ∘ (_ -> Blue) err "error"
        ,Point(2, 4).in ∘ (_ -> Green) err "error"
        ,Point(3, 1).in ∘ (_ -> Green) err "error"
        ,Point(4, 1).in ∘ (_ -> Green) err "error"
        ,Point(5, 1).in ∘ (_ -> Green) err "error"
        ,Point(2, 5).in ∘ (_ -> Yellow) err "error"
      )
      setPuyo(puyos)

      val expected = Map(
        Point(2, 0).in ∘ (_ -> Yellow) err "error"
      )

      val (vf, ojama) = VanishedFutureField(df)
      (vf.puyos must_== expected) and (ojama must_== 13)
    }

    def e9 = {
      val puyos = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
        ,Point(2, 0).in ∘ (_ -> Red) err "error"
        ,Point(2, 1).in ∘ (_ -> Red) err "error"
        ,Point(2, 2).in ∘ (_ -> Red) err "error"
        ,Point(2, 3).in ∘ (_ -> Blue) err "error"
        ,Point(3, 0).in ∘ (_ -> Blue) err "error"
        ,Point(3, 1).in ∘ (_ -> Blue) err "error"
        ,Point(3, 2).in ∘ (_ -> Blue) err "error"
        ,Point(3, 3).in ∘ (_ -> Green) err "error"
        ,Point(4, 0).in ∘ (_ -> Green) err "error"
        ,Point(4, 1).in ∘ (_ -> Green) err "error"
        ,Point(4, 2).in ∘ (_ -> Green) err "error"
        ,Point(5, 0).in ∘ (_ -> Yellow) err "error"
      )
      setPuyo(puyos)

      val expected = Map(
        Point(5, 0).in ∘ (_ -> Yellow) err "error"
      )

      val (vf, ojama) = VanishedFutureField(df)
      (vf.puyos must_== expected) and (ojama must_== 14)
    }

    def e10 = {
      val puyos = Map(
        Point(0, 0).in ∘ (_ -> Green) err "error"
        ,Point(0, 1).in ∘ (_ -> Green) err "error"
        ,Point(0, 2).in ∘ (_ -> Green) err "error"
        ,Point(0, 3).in ∘ (_ -> Blue) err "error"
        ,Point(0, 4).in ∘ (_ -> Blue) err "error"
        ,Point(0, 5).in ∘ (_ -> Blue) err "error"
        ,Point(0, 6).in ∘ (_ -> Green) err "error"
        ,Point(0, 7).in ∘ (_ -> Green) err "error"
        ,Point(0, 8).in ∘ (_ -> Yellow) err "error"
        ,Point(0, 9).in ∘ (_ -> Purple) err "error"
        ,Point(1, 0).in ∘ (_ -> Yellow) err "error"
        ,Point(1, 1).in ∘ (_ -> Yellow) err "error"
        ,Point(1, 2).in ∘ (_ -> Yellow) err "error"
        ,Point(1, 3).in ∘ (_ -> Red) err "error"
        ,Point(1, 4).in ∘ (_ -> Yellow) err "error"
        ,Point(1, 5).in ∘ (_ -> Yellow) err "error"
        ,Point(1, 6).in ∘ (_ -> Yellow) err "error"
        ,Point(1, 7).in ∘ (_ -> Blue) err "error"
        ,Point(2, 0).in ∘ (_ -> Red) err "error"
        ,Point(2, 1).in ∘ (_ -> Red) err "error"
        ,Point(2, 2).in ∘ (_ -> Red) err "error"
        ,Point(2, 3).in ∘ (_ -> Blue) err "error"
        ,Point(2, 4).in ∘ (_ -> Red) err "error"
        ,Point(2, 5).in ∘ (_ -> Red) err "error"
        ,Point(2, 6).in ∘ (_ -> Red) err "error"
        ,Point(2, 7).in ∘ (_ -> Yellow) err "error"
        ,Point(3, 0).in ∘ (_ -> Blue) err "error"
        ,Point(3, 1).in ∘ (_ -> Blue) err "error"
        ,Point(3, 2).in ∘ (_ -> Blue) err "error"
        ,Point(3, 3).in ∘ (_ -> Green) err "error"
        ,Point(3, 4).in ∘ (_ -> Blue) err "error"
        ,Point(3, 5).in ∘ (_ -> Blue) err "error"
        ,Point(3, 6).in ∘ (_ -> Blue) err "error"
        ,Point(3, 7).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 8).in ∘ (_ -> Red) err "error"
        ,Point(4, 0).in ∘ (_ -> Green) err "error"
        ,Point(4, 1).in ∘ (_ -> Green) err "error"
        ,Point(4, 2).in ∘ (_ -> Green) err "error"
        ,Point(4, 3).in ∘ (_ -> Yellow) err "error"
        ,Point(4, 4).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 5).in ∘ (_ -> Ojama) err "error"
        ,Point(4, 6).in ∘ (_ -> Blue) err "error"
        ,Point(5, 0).in ∘ (_ -> Yellow) err "error"
        ,Point(5, 1).in ∘ (_ -> Yellow) err "error"
        ,Point(5, 2).in ∘ (_ -> Yellow) err "error"
      )
      setPuyo(puyos)

      val expected = Map(
        Point(0, 0).in ∘ (_ -> Purple) err "error"
      )

      val (vf, ojama) = VanishedFutureField(df)
      (vf.puyos must_== expected) and (ojama must_== 536)
    }

    def e11 = {
      val puyos = Map(
        Point(1, 0).in ∘ (_ -> Blue) err "error"
        ,Point(2, 0).in ∘ (_ -> Blue) err "error"
        ,Point(1, 1).in ∘ (_ -> Blue) err "error"
        ,Point(2, 1).in ∘ (_ -> Blue) err "error"
      )
      setPuyo(puyos)

      val expected = Map[InFieldPoint, Puyo]()

      val (vf, ojama) = VanishedFutureField(df)
      (vf.puyos must_== expected) and (ojama must_== 30)
    }

  }
}
