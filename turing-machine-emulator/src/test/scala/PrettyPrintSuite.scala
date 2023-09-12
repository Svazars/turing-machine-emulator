package nsu.turing

import PrettyPrint.tapeSegment
import TestsCommon._
import base._

import org.scalatest.funsuite.AnyFunSuite

class PrettyPrintSuite extends AnyFunSuite {

  test("Empty tape") {
    val R_EMPTY: RightSidedTape = RightSidedTape(f)
    assert(tapeSegment(R_EMPTY, 0, 0) == "#")
    assert(tapeSegment(R_EMPTY, 30, 30) == "#")
    assert(tapeSegment(R_EMPTY, 0, 5) == "#|#|#|#|#|#")
    assert(tapeSegment(R_EMPTY, -2, 5) == " | |#|#|#|#|#|#")
  }

  test("Non empty tape") {
    val TAPE = LeftSidedTape(f)
      .withModifiedMemory(Position(0), a)
      .withModifiedMemory(Position(-1), b)
      .withModifiedMemory(Position(-2), c)
    assert(tapeSegment(TAPE, -3, 0) == "#|c|b|a")
  }

  test("Rules") {
    assert(Rule(Q, P, List(f), List(f), List(NoMove)).toString == "Q: # -> #H P")
    assert(Rule(Q, P, List(f), List(f), List(MoveRight)).toString == "Q: # -> #R P")
    assert(Rule(Q, P, List(f), List(f), List(MoveLeft)).toString == "Q: # -> #L P")

    assert(Rule(F, Q, List(a, b, f), List(f, b, a), List(NoMove, MoveRight, MoveLeft)).toString == "F: a, b, # -> #H, bR, aL Q")
  }
}