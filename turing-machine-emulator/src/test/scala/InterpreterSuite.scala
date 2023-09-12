package nsu.turing

import TestsCommon._
import base._
import interpreter.{Interpreter, OutOfIterations, Stepping}

import org.scalatest.funsuite.AnyFunSuite

class InterpreterSuite_Stepping extends AnyFunSuite {

  test("Always left on right-infinite tape") {
    val e1 = Interpreter.initialContext(ALWAYS_LEFT_PROGRAM, START, STOP, RightSidedTape(DEFAULT_SYMBOL))
    (Interpreter.step(e1): @unchecked) match {
      case Stepping.OutOfTape(_, e2) =>
        assert(e2 == e1)
    }
  }

  test("Invert on empty tape") {
    val e1 = Interpreter.initialContext(INVERT_PROGRAM, START, STOP, RightSidedTape(DEFAULT_SYMBOL))

    (Interpreter.step(e1): @unchecked) match {
      case Stepping.Success(_, e2) =>
        assert(e2.prevContext.get == e1)
        (Interpreter.step(e2): @unchecked) match {
          case Stepping.Terminated(_, e3) =>
            assert(e3.prevContext.get == e2)
        }
    }
  }

  test("Invert on 101") {
    val initialTape = RightSidedTape(DEFAULT_SYMBOL)
      .withModifiedMemory(Position(1), l1)
      .withModifiedMemory(Position(2), l0)
      .withModifiedMemory(Position(3), l1)

    val e1 = Interpreter.initialContext(INVERT_PROGRAM, START, STOP, initialTape)

    assert(PrettyPrint.tapeSegment(e1.tapes.head, 0, 4) == "#|1|0|1|#")

    (Interpreter.step(e1): @unchecked) match {
      case Stepping.Success(_, e2) =>
        assert(PrettyPrint.tapeSegment(e2.tapes.head, 0, 4) == "#|1|0|1|#")
        (Interpreter.step(e2): @unchecked) match {
          case Stepping.Success(_, e3) =>
            assert(PrettyPrint.tapeSegment(e3.tapes.head, 0, 4) == "#|0|0|1|#")
            (Interpreter.step(e3): @unchecked) match {
              case Stepping.Success(_, e4) =>
                assert(PrettyPrint.tapeSegment(e4.tapes.head, 0, 4) == "#|0|1|1|#")
                (Interpreter.step(e4): @unchecked) match {
                  case Stepping.Success(_, e5) =>
                    assert(PrettyPrint.tapeSegment(e5.tapes.head, 0, 4) == "#|0|1|0|#")
                    (Interpreter.step(e5): @unchecked) match {
                      case Stepping.Terminated(_, e6) =>
                        assert(PrettyPrint.tapeSegment(e6.tapes.head, 0, 4) == "#|0|1|0|#")
                    }
                }
            }
        }
    }
  }

  test("Not found rule with empty program") {
    val EMPTY_PROGRAM = Program(BINARY_ALPHABET)
    val e1 = Interpreter.initialContext(EMPTY_PROGRAM, START, STOP)
    (Interpreter.step(e1): @unchecked) match {
      case Stepping.RuleNotFound(e2) =>
        assert(e1 == e2)
    }
  }

  test("Not found rule on first step") {
    val PARTIAL_PROGRAM = Program(BINARY_ALPHABET,
      Rule(START, STOP, List(l0), List(l0), List(NoMove))
    )

    val e1 = Interpreter.initialContext(PARTIAL_PROGRAM, START, STOP, InfiniteTape(DEFAULT_SYMBOL))
    (Interpreter.step(e1): @unchecked) match {
      case Stepping.RuleNotFound(e2) =>
        assert(e1 == e2)
    }
  }

  test("Not found rule on second step") {
    val PARTIAL_PROGRAM = Program(BINARY_ALPHABET,
      Rule(START, START, List(DEFAULT_SYMBOL), List(l0), List(NoMove))
    )
    val e1 = Interpreter.initialContext(PARTIAL_PROGRAM, START, STOP, InfiniteTape(DEFAULT_SYMBOL))
    assert(PrettyPrint.tapeSegment(e1.tapes.head, 0, 2) == "#|#|#")
    (Interpreter.step(e1): @unchecked) match {
      case Stepping.Success(_, e2) =>
        assert(PrettyPrint.tapeSegment(e2.tapes.head, 0, 2) == "0|#|#")
        (Interpreter.step(e2): @unchecked) match {
          case Stepping.RuleNotFound(e3) =>
            assert(e2 == e3)
        }
    }
  }
}

class InterreterSuite_Execution extends AnyFunSuite {

  test("Busy beaver 3") {

  }

  test("Multi-tape addition") {

  }

  test("Out of iterations: 0") { assertThrows[IllegalArgumentException] { testOoIN(0) } }
  test("Out of iterations: 1") { assertThrows[IllegalArgumentException] { testOoIN(0) } }
  test("Out of iterations: 3  ") { testOoIN(3) }
  test("Out of iterations: 10 ") { testOoIN(10) }
  test("Out of iterations: 100") { testOoIN(100) }

  private def testOoIN(N: Int): Unit = {
    val SPIN_PROGRAM = Program(BINARY_ALPHABET,
      Rule(START, START, List(DEFAULT_SYMBOL), List(DEFAULT_SYMBOL), List(MoveRight))
    )

    val e1 = Interpreter.initialContext(SPIN_PROGRAM, START, STOP, InfiniteTape(DEFAULT_SYMBOL))
    (Interpreter.execute(e1, N, {_ => }): @unchecked) match {
      case ooi: OutOfIterations =>
        val trace = Interpreter.fullTraceOf(ooi)
        assert(trace.length == N)
        for ((e, i) <- trace.zipWithIndex) {
          assert(e.head.currentState == START)
          assert(e.head.positions.head == Position(i))
          assert(PrettyPrint.tapeSegment(e.tapes.head, 0, N) == PrettyPrint.tapeSegment(InfiniteTape(DEFAULT_SYMBOL), 0, N))
        }
    }
  }
}

