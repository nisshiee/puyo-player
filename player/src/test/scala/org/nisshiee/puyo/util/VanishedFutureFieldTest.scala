package org.nisshiee.puyo.util

import org.specs2._, mock._
import scalaz._, Scalaz._

import org.nisshiee.puyo.core._, Puyoz._

class VanishedFutureFieldTest extends Specification { def is =

  "VanishedFutureFieldクラスのテスト"                       ^
    "DroppedFutureFieldからVanishedFutureFieldを算出"       ^
      "1つもぷよが消えないパターン"                         ! SizedFieldMock().e1^
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

      VanishedFutureField(df).puyos must_== puyos
    }
  }
}
