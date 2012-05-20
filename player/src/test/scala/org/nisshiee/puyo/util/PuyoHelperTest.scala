package org.nisshiee.puyo.util

import org.specs2._, mock._
import scalaz._, Scalaz._

import org.nisshiee.puyo.core._
import PuyoHelper._

class PuyoHelperTest extends Specification { def is =

  "PuyoHelperオブジェクトのテスト"                          ^
    "connectPuyo関数のテスト"                               ^
      "いろいろ含むケース"                                  ! SizedFieldMock().e1^
                                                            p^
    "dropAll関数のテスト"                                   ^
      "ぷよ1個が1マス落ちるケース"                          ! SizedFieldMock().e2^
      "ぷよ1個が2マス落ちるケース"                          ! SizedFieldMock().e3^
      "ぷよ2個が1マス落ちるケース"                          ! SizedFieldMock().e4^
      "ぷよ1個が1マス、1個が2マス落ちるケース"              ! SizedFieldMock().e5^
      "別の列で別の位置から落ちるケース"                    ! SizedFieldMock().e6^
      "いろいろなパターンが混在するケース"                  ! SizedFieldMock().e7^
      "動くぷよが無いケース"                                ! SizedFieldMock().e8^
                                                            end

  case class SizedFieldMock() extends Mockito {

    implicit val f = mock[Field]
    f.width returns 6
    f.height returns 14

    def e1 = {
      val puyos: Map[InFieldPoint, Puyo] = Map(
        Point(0, 0).in ∘ { p => (p -> Red) } err "error"
        ,Point(0, 1).in ∘ { p => (p -> Red) } err "error"
        ,Point(0, 2).in ∘ { p => (p -> Red) } err "error"
        ,Point(0, 3).in ∘ { p => (p -> Red) } err "error"
        ,Point(0, 4).in ∘ { p => (p -> Ojama) } err "error"
        ,Point(0, 5).in ∘ { p => (p -> Ojama) } err "error"
        ,Point(0, 6).in ∘ { p => (p -> Ojama) } err "error"
        ,Point(0, 7).in ∘ { p => (p -> Ojama) } err "error"

        ,Point(1, 0).in ∘ { p => (p -> Blue) } err "error"
        ,Point(1, 1).in ∘ { p => (p -> Purple) } err "error"
        ,Point(1, 2).in ∘ { p => (p -> Purple) } err "error"
        ,Point(1, 3).in ∘ { p => (p -> Blue) } err "error"
        ,Point(1, 4).in ∘ { p => (p -> Blue) } err "error"
        ,Point(1, 5).in ∘ { p => (p -> Yellow) } err "error"
        ,Point(1, 6).in ∘ { p => (p -> Purple) } err "error"

        ,Point(2, 0).in ∘ { p => (p -> Blue) } err "error"
        ,Point(2, 1).in ∘ { p => (p -> Yellow) } err "error"
        ,Point(2, 2).in ∘ { p => (p -> Yellow) } err "error"
        ,Point(2, 3).in ∘ { p => (p -> Yellow) } err "error"
        ,Point(2, 4).in ∘ { p => (p -> Blue) } err "error"
        ,Point(2, 5).in ∘ { p => (p -> Blue) } err "error"
        ,Point(2, 6).in ∘ { p => (p -> Red) } err "error"

        ,Point(3, 0).in ∘ { p => (p -> Blue) } err "error"
        ,Point(3, 1).in ∘ { p => (p -> Green) } err "error"
        ,Point(3, 2).in ∘ { p => (p -> Green) } err "error"
        ,Point(3, 3).in ∘ { p => (p -> Yellow) } err "error"
        ,Point(3, 4).in ∘ { p => (p -> Yellow) } err "error"
        ,Point(3, 5).in ∘ { p => (p -> Red) } err "error"
        ,Point(3, 6).in ∘ { p => (p -> Red) } err "error"
        ,Point(3, 7).in ∘ { p => (p -> Blue) } err "error"
        ,Point(3, 8).in ∘ { p => (p -> Red) } err "error"

        ,Point(4, 0).in ∘ { p => (p -> Blue) } err "error"
        ,Point(4, 1).in ∘ { p => (p -> Red) } err "error"
        ,Point(4, 2).in ∘ { p => (p -> Green) } err "error"
        ,Point(4, 3).in ∘ { p => (p -> Blue) } err "error"
        ,Point(4, 4).in ∘ { p => (p -> Blue) } err "error"
        ,Point(4, 5).in ∘ { p => (p -> Red) } err "error"
        ,Point(4, 6).in ∘ { p => (p -> Purple) } err "error"
        ,Point(4, 7).in ∘ { p => (p -> Blue) } err "error"
        ,Point(4, 8).in ∘ { p => (p -> Red) } err "error"

        ,Point(5, 0).in ∘ { p => (p -> Red) } err "error"
        ,Point(5, 1).in ∘ { p => (p -> Red) } err "error"
        ,Point(5, 2).in ∘ { p => (p -> Green) } err "error"
        ,Point(5, 3).in ∘ { p => (p -> Blue) } err "error"
        ,Point(5, 4).in ∘ { p => (p -> Blue) } err "error"
        ,Point(5, 5).in ∘ { p => (p -> Yellow) } err "error"
        ,Point(5, 6).in ∘ { p => (p -> Yellow) } err "error"
        ,Point(5, 7).in ∘ { p => (p -> Yellow) } err "error"
        ,Point(5, 8).in ∘ { p => (p -> Red) } err "error"
      )
      val expected: Set[(Set[InFieldPoint], Puyo)] = Set(
        Set(
          Point(0, 0).in err "error"
          ,Point(0, 1).in err "error"
          ,Point(0, 2).in err "error"
          ,Point(0, 3).in err "error"
        ) -> Red
        ,Set(
          Point(1, 0).in err "error"
          ,Point(2, 0).in err "error"
          ,Point(3, 0).in err "error"
          ,Point(4, 0).in err "error"
        ) -> Blue
        ,Set(
          Point(1, 1).in err "error"
          ,Point(1, 2).in err "error"
        ) -> Purple
        ,Set(
          Point(1, 3).in err "error"
          ,Point(1, 4).in err "error"
          ,Point(2, 4).in err "error"
          ,Point(2, 5).in err "error"
        ) -> Blue
        ,Set(
          Point(1, 5).in err "error"
        ) -> Yellow
        ,Set(
          Point(1, 6).in err "error"
        ) -> Purple
        ,Set(
          Point(2, 1).in err "error"
          ,Point(2, 2).in err "error"
          ,Point(2, 3).in err "error"
          ,Point(3, 3).in err "error"
          ,Point(3, 4).in err "error"
        ) -> Yellow
        ,Set(
          Point(2, 6).in err "error"
          ,Point(3, 5).in err "error"
          ,Point(3, 6).in err "error"
          ,Point(4, 5).in err "error"
        ) -> Red
        ,Set(
          Point(3, 1).in err "error"
          ,Point(3, 2).in err "error"
          ,Point(4, 2).in err "error"
          ,Point(5, 2).in err "error"
        ) -> Green
        ,Set(
          Point(3, 7).in err "error"
          ,Point(4, 7).in err "error"
        ) -> Blue
        ,Set(
          Point(3, 8).in err "error"
          ,Point(4, 8).in err "error"
          ,Point(5, 8).in err "error"
        ) -> Red
        ,Set(
          Point(4, 1).in err "error"
          ,Point(5, 0).in err "error"
          ,Point(5, 1).in err "error"
        ) -> Red
        ,Set(
          Point(4, 3).in err "error"
          ,Point(4, 4).in err "error"
          ,Point(5, 3).in err "error"
          ,Point(5, 4).in err "error"
        ) -> Blue
        ,Set(
          Point(4, 6).in err "error"
        ) -> Purple
        ,Set(
          Point(5, 5).in err "error"
          ,Point(5, 6).in err "error"
          ,Point(5, 7).in err "error"
        ) -> Yellow
      )

      connectPuyo(puyos) must_== expected
    }

    def e2 = {
      val puyos: Map[InFieldPoint, Puyo] = Map(
        Point(1, 1).in ∘ (_ -> Red) err "error"
      )

      val expected: Map[InFieldPoint, Puyo] = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
      )

      dropAll(puyos) must_== expected
    }

    def e3 = {
      val puyos: Map[InFieldPoint, Puyo] = Map(
        Point(1, 2).in ∘ (_ -> Red) err "error"
      )

      val expected: Map[InFieldPoint, Puyo] = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
      )

      dropAll(puyos) must_== expected
    }

    def e4 = {
      val puyos: Map[InFieldPoint, Puyo] = Map(
        Point(1, 1).in ∘ (_ -> Red) err "error"
        ,Point(1, 2).in ∘ (_ -> Blue) err "error"
      )

      val expected: Map[InFieldPoint, Puyo] = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
        ,Point(1, 1).in ∘ (_ -> Blue) err "error"
      )

      dropAll(puyos) must_== expected
    }

    def e5 = {
      val puyos: Map[InFieldPoint, Puyo] = Map(
        Point(1, 1).in ∘ (_ -> Red) err "error"
        ,Point(1, 3).in ∘ (_ -> Blue) err "error"
      )

      val expected: Map[InFieldPoint, Puyo] = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
        ,Point(1, 1).in ∘ (_ -> Blue) err "error"
      )

      dropAll(puyos) must_== expected
    }

    def e6 = {
      val puyos: Map[InFieldPoint, Puyo] = Map(
        Point(1, 1).in ∘ (_ -> Red) err "error"
        ,Point(2, 0).in ∘ (_ -> Blue) err "error"
        ,Point(2, 3).in ∘ (_ -> Green) err "error"
      )

      val expected: Map[InFieldPoint, Puyo] = Map(
        Point(1, 0).in ∘ (_ -> Red) err "error"
        ,Point(2, 0).in ∘ (_ -> Blue) err "error"
        ,Point(2, 1).in ∘ (_ -> Green) err "error"
      )

      dropAll(puyos) must_== expected
    }

    def e7 = {
      val puyos: Map[InFieldPoint, Puyo] = Map(
        Point(0, 1).in ∘ (_ -> Red) err "error"
        ,Point(1, 2).in ∘ (_ -> Blue) err "error"
        ,Point(2, 1).in ∘ (_ -> Green) err "error"
        ,Point(2, 4).in ∘ (_ -> Yellow) err "error"
        ,Point(3, 0).in ∘ (_ -> Purple) err "error"
        ,Point(3, 4).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 5).in ∘ (_ -> Red) err "error"
      )

      val expected: Map[InFieldPoint, Puyo] = Map(
        Point(0, 0).in ∘ (_ -> Red) err "error"
        ,Point(1, 0).in ∘ (_ -> Blue) err "error"
        ,Point(2, 0).in ∘ (_ -> Green) err "error"
        ,Point(2, 1).in ∘ (_ -> Yellow) err "error"
        ,Point(3, 0).in ∘ (_ -> Purple) err "error"
        ,Point(3, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 2).in ∘ (_ -> Red) err "error"
      )

      dropAll(puyos) must_== expected
    }

    def e8 = {
      val puyos: Map[InFieldPoint, Puyo] = Map(
        Point(0, 0).in ∘ (_ -> Red) err "error"
        ,Point(1, 0).in ∘ (_ -> Blue) err "error"
        ,Point(2, 0).in ∘ (_ -> Green) err "error"
        ,Point(2, 1).in ∘ (_ -> Yellow) err "error"
        ,Point(3, 0).in ∘ (_ -> Purple) err "error"
        ,Point(3, 1).in ∘ (_ -> Ojama) err "error"
        ,Point(3, 2).in ∘ (_ -> Red) err "error"
      )

      dropAll(puyos) must_== puyos
    }
  }
}
