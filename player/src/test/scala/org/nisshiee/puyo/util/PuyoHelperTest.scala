package org.nisshiee.puyo.util

import org.specs2._, mock._
import scalaz._, Scalaz._

import org.nisshiee.puyo.core._
import PuyoHelper._

class PuyoHelperTest extends Specification { def is =

  "PuyoHelperオブジェクトのテスト"                          ^
    "ifpColmn関数のテスト - MiniField(height = 3)"          ^
      "(3, 0) - (3, 2)"                                     ! MiniFieldMock().e1^
                                                            p^
    "nextPuyoHeight関数のテスト - MiniField(height = 3)"    ^
      "Y=0まで積まれてたらSome(1)"                          ! MiniFieldMock().e2^
      "その列には積まれてなかったらSome(0)"                 ! MiniFieldMock().e3^
      "最上段まで積まれてたらNone"                          ! MiniFieldMock().e4^
                                                            p^
    "isEmptyColumn関数のテスト - MiniField(height = 3)"     ^
      "Empty"                                               ! MiniFieldMock().e5^
      "Not Empty"                                           ! MiniFieldMock().e6^
                                                            end

  case class MiniFieldMock() extends Mockito {

    implicit val f = mock[Field]
    f.width returns 6
    f.height returns 3

    val puyos: Map[InFieldPoint, Puyo] = List(
      Point(3, 0).in ∘ { p => (p -> Red) } err "error"
      ,Point(0, 0).in ∘ { p => (p -> Red) } err "error"
      ,Point(0, 1).in ∘ { p => (p -> Red) } err "error"
      ,Point(0, 2).in ∘ { p => (p -> Red) } err "error"
    ).toMap
    f.puyos returns puyos
    f.apply(any[InFieldPoint]) answers {
      case ifp: InFieldPoint => puyos.get(ifp)
    }

    def e1 = ifpColumn(3) must_== List(
      Point(3, 0).in err "error"
      ,Point(3, 1).in err "error"
      ,Point(3, 2).in err "error"
    )

    def e2 = nextPuyoHeight(3, puyos) must beSome.which(1 ≟)

    def e3 = nextPuyoHeight(5, puyos) must beSome.which(0 ≟)

    def e4 = nextPuyoHeight(0, puyos) must beNone

    def e5 = isEmptyColumn(5, puyos) must beTrue

    def e6 = isEmptyColumn(3, puyos) must beFalse
  }
}
