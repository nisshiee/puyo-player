package org.nisshiee.puyo.util

import org.specs2._, mock._
import scalaz._, Scalaz._

import org.nisshiee.puyo.core._, Puyoz._

class DroppedFutureFieldTest extends Specification { def is =

  "DroppedFutureFieldクラスのテスト"                        ^
    "FieldからDroppedFutureFieldを算出"                     ^
      "空FieldにPuyoBlockをUp向きに落とす"                  ! EmptyFieldMock().e1^
      "空FieldにPuyoBlockをDown向きに落とす"                ! EmptyFieldMock().e2^
      "空FieldにPuyoBlockをRight向きに落とす"               ! EmptyFieldMock().e3^
      "空FieldにPuyoBlockをLeft向きに落とす"                ! EmptyFieldMock().e4^
      "Up向きに落とし、ぷよの上に乗るケース"                ! PuyoFieldMock().e5^
      "Down向きに落とし、ぷよの上に乗るケース"              ! PuyoFieldMock().e6^
      "Right向きに落とし、ちぎれずにぷよの上に乗るケース"   ! PuyoFieldMock().e7^
      "Left向きに落とし、ちぎれずにぷよの上に乗るケース"    ! PuyoFieldMock().e8^
      "Right向きに落とし、ちぎれてぷよの上に乗るケース"     ! PuyoFieldMock().e9^
      "Left向きに落とし、ちぎれてぷよの上に乗るケース"      ! PuyoFieldMock().e10^
                                                            end

  case class EmptyFieldMock() extends Mockito {

    implicit val f = mock[Field]
    f.width returns 6
    f.height returns 14
    f.deadLine returns 12
    f.apply(any[InFieldPoint]) returns None
    f.puyos returns Map[InFieldPoint, Puyo]()

    def e1 = (for {
      a <- Action.check(Up, 2)
      redpoint <- Point(2, 0).in
      bluepoint <- Point(2, 1).in
      ff = DroppedFutureField(PuyoBlock(Red, Blue), a)
      assertred <- ff(redpoint) ∘ (_ ≟ Red)
      assertblue <- ff(bluepoint) ∘ (_ ≟ Blue)
    } yield assertred && assertblue) must beSome.which(identity)

    def e2 = (for {
      a <- Action.check(Down, 2)
      redpoint <- Point(2, 1).in
      bluepoint <- Point(2, 0).in
      ff = DroppedFutureField(PuyoBlock(Red, Blue), a)
      assertred <- ff(redpoint) ∘ (_ ≟ Red)
      assertblue <- ff(bluepoint) ∘ (_ ≟ Blue)
    } yield assertred && assertblue) must beSome.which(identity)

    def e3 = (for {
      a <- Action.check(Right, 2)
      redpoint <- Point(2, 0).in
      bluepoint <- Point(3, 0).in
      ff = DroppedFutureField(PuyoBlock(Red, Blue), a)
      assertred <- ff(redpoint) ∘ (_ ≟ Red)
      assertblue <- ff(bluepoint) ∘ (_ ≟ Blue)
    } yield assertred && assertblue) must beSome.which(identity)

    def e4 = (for {
      a <- Action.check(Left, 2)
      redpoint <- Point(2, 0).in
      bluepoint <- Point(1, 0).in
      ff = DroppedFutureField(PuyoBlock(Red, Blue), a)
      assertred <- ff(redpoint) ∘ (_ ≟ Red)
      assertblue <- ff(bluepoint) ∘ (_ ≟ Blue)
    } yield assertred && assertblue) must beSome.which(identity)
  }

  case class PuyoFieldMock() extends Mockito {

    implicit val f = mock[Field]
    f.width returns 6
    f.height returns 14
    f.deadLine returns 12

    val puyos = List(
      Point(2, 0).in ∘ { p => (p -> Red) },
      Point(3, 0).in ∘ { p => (p -> Blue) },
      Point(4, 0).in ∘ { p => (p -> Green) },
      Point(4, 1).in ∘ { p => (p -> Yellow) }
    ).flatten.toMap

    f.apply(any[InFieldPoint]) answers {
      case ifp: InFieldPoint => puyos.get(ifp)
      case _ => None
    }
    f.puyos returns puyos

    def e5 = (for {
      a <- Action.check(Up, 2)
      redpoint <- Point(2, 1).in
      bluepoint <- Point(2, 2).in
      ff = DroppedFutureField(PuyoBlock(Red, Blue), a)
      assertred <- ff(redpoint) ∘ (_ ≟ Red)
      assertblue <- ff(bluepoint) ∘ (_ ≟ Blue)
    } yield assertred && assertblue) must beSome.which(identity)

    def e6 = (for {
      a <- Action.check(Down, 2)
      redpoint <- Point(2, 2).in
      bluepoint <- Point(2, 1).in
      ff = DroppedFutureField(PuyoBlock(Red, Blue), a)
      assertred <- ff(redpoint) ∘ (_ ≟ Red)
      assertblue <- ff(bluepoint) ∘ (_ ≟ Blue)
    } yield assertred && assertblue) must beSome.which(identity)

    def e7 = (for {
      a <- Action.check(Right, 2)
      redpoint <- Point(2, 1).in
      bluepoint <- Point(3, 1).in
      ff = DroppedFutureField(PuyoBlock(Red, Blue), a)
      assertred <- ff(redpoint) ∘ (_ ≟ Red)
      assertblue <- ff(bluepoint) ∘ (_ ≟ Blue)
    } yield assertred && assertblue) must beSome.which(identity)

    def e8 = (for {
      a <- Action.check(Left, 3)
      redpoint <- Point(3, 1).in
      bluepoint <- Point(2, 1).in
      ff = DroppedFutureField(PuyoBlock(Red, Blue), a)
      assertred <- ff(redpoint) ∘ (_ ≟ Red)
      assertblue <- ff(bluepoint) ∘ (_ ≟ Blue)
    } yield assertred && assertblue) must beSome.which(identity)

    def e9 = (for {
      a <- Action.check(Right, 4)
      redpoint <- Point(4, 2).in
      bluepoint <- Point(5, 0).in
      ff = DroppedFutureField(PuyoBlock(Red, Blue), a)
      assertred <- ff(redpoint) ∘ (_ ≟ Red)
      assertblue <- ff(bluepoint) ∘ (_ ≟ Blue)
    } yield assertred && assertblue) must beSome.which(identity)

    def e10 = (for {
      a <- Action.check(Left, 4)
      redpoint <- Point(4, 2).in
      bluepoint <- Point(3, 1).in
      ff = DroppedFutureField(PuyoBlock(Red, Blue), a)
      assertred <- ff(redpoint) ∘ (_ ≟ Red)
      assertblue <- ff(bluepoint) ∘ (_ ≟ Blue)
    } yield assertred && assertblue) must beSome.which(identity)
  }
}
