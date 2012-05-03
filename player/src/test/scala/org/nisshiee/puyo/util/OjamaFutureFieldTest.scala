package org.nisshiee.puyo.util

import org.specs2._, mock._
import scalaz._, Scalaz._

import org.nisshiee.puyo.core._, Puyoz._

class OjamaFutureFieldTest extends Specification { def is =

  "OjamaFutureFieldクラスのテスト"                          ^
    "VanishedFutureFieldからOjamaFutureFieldを算出"         ^
      "OjamaQueueが空のケース"                              ! SizedFieldMock().e1^
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

      val (of, nextQueue) = OjamaFutureField(vf, ojamaQueue, attack)
      (of.puyos must_== puyos) and (nextQueue must_== ojamaQueue.tail)
    }
  }
}
