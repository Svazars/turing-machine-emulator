package nsu.turing

import nsu.turing.SamplesRunner.doRun
import org.scalatest.funsuite.AnyFunSuite

object SamplesRunner extends AnyFunSuite {
  def doRun(cmdLine: String, expectedOut: String): Unit = {
    val (options, args) = CmdParser.parseCmd(cmdLine.split(" "))
    val (t, c) = TestsCommon.logger(
      Set(
        Some(LoggingLevel.IMPORTANT),
        Some(LoggingLevel.IMPORTANT_NONEWLINE),
        if (options.contains(Options.TRACE)) Some(LoggingLevel.TRACE) else None,
        if (options.contains(Options.VERBOSE)) Some(LoggingLevel.VERBOSE) else None,
      ).flatten
    )
    Main.doMain(c, options, args)
    assert(t.string() == expectedOut)
  }
}

class SamplesSuite extends AnyFunSuite {

  test("HelloWorld_1d") {
    doRun("--program samples/HelloWorld_1d/hello.tur --input #", "Reached terminal state successfully!\n")
    doRun("--program samples/HelloWorld_1d/hello.tur", "Cannot run turing machine: input is not provided. Consider adding --input\n")

    doRun("--program samples/HelloWorld_1d/hello.tur --input !", "Cannot run turing machine with input !: Symbol(!) is not compatible with alphabet Symbol(world),Symbol(h),Symbol(e),Symbol(o),Symbol(l),Symbol(#)\n")
    doRun("--program samples/HelloWorld_1d/hello.tur --input h", "Error: rule not found\n")
  }

  test("HelloWorld_2d") {
    doRun("--program samples/HelloWorld_2d/hello.tur --input #|#", "Reached terminal state successfully!\n")
  }

  test("Add_1_1d") {
    val EXPECTED =
      """Running 12 tests:
        |  1 / 12: test_empty ...success in 3 iterations.
        |  2 / 12: test_zero ...success in 4 iterations.
        |  3 / 12: test_1 ...success in 5 iterations.
        |  4 / 12: test_2 ...success in 4 iterations.
        |  5 / 12: test_3 ...success in 6 iterations.
        |  6 / 12: test_4 ...success in 4 iterations.
        |  7 / 12: test_5 ...success in 5 iterations.
        |  8 / 12: test_6 ...success in 4 iterations.
        |  9 / 12: test_7 ...success in 7 iterations.
        |  10 / 12: test_127 ...success in 11 iterations.
        |  11 / 12: test_567 ...success in 7 iterations.
        |  12 / 12: test_1023 ...success in 14 iterations.
        |""".stripMargin
    doRun("--program samples/Add_1_1d/Add.tur --test samples/Add_1_1d/Add.test", EXPECTED)
  }
}


class SampleWithTraces extends AnyFunSuite {

  test("HelloWorld_1d trace") {
    val EXPECTED =
      """Start execution from state: START
        | | | | | | | | | | | | | | | | | | | | |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |START: # -> hR H
        | | | | | | | | | | | | | | | | | | | |h|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |H: # -> eR E
        | | | | | | | | | | | | | | | | | | |h|e|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |E: # -> lR L
        | | | | | | | | | | | | | | | | | |h|e|l|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |L: # -> lR LL
        | | | | | | | | | | | | | | | | |h|e|l|l|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |LL: # -> oR O
        | | | | | | | | | | | | | | | |h|e|l|l|o|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |O: # -> eR WORLD
        | | | | | | | | | | | | | | |h|e|l|l|o|e|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |WORLD: # -> #H OTHER_STATE
        | | | | | | | | | | | | | | |h|e|l|l|o|e|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |OTHER_STATE: # -> #H non_capital_letters_state
        | | | | | | | | | | | | | | |h|e|l|l|o|e|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |non_capital_letters_state: # -> #L returned_left
        | | | | | | | | | | | | | | | |h|e|l|l|o|e|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |returned_left: e -> worldH STOP
        | | | | | | | | | | | | | | | |h|e|l|l|o|world|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                            ^
        |Reached terminal state successfully!
        |""".stripMargin

    doRun("--program samples/HelloWorld_1d/hello.tur --input # -trace", EXPECTED)
  }

  test("HelloWorld_2d trace") {
    val EXPECTED =
      """Start execution from state: START
        |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#| | | | | | | | | | | | | | | | | | | |
        |                                        ^
        | | | | | | | | | | | | | | | | | | | | |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |START: #, # -> #L, #R PRINT_1
        |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#| | | | | | | | | | | | | | | | | | |
        |                                        ^
        | | | | | | | | | | | | | | | | | | | |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |PRINT_1: #, # -> oL, wR PRINT_2
        |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|o|#| | | | | | | | | | | | | | | | | |
        |                                        ^
        | | | | | | | | | | | | | | | | | | |#|w|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |PRINT_2: #, # -> lL, oR PRINT_3
        |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|l|o|#| | | | | | | | | | | | | | | | |
        |                                        ^
        | | | | | | | | | | | | | | | | | |#|w|o|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |PRINT_3: #, # -> lL, rR PRINT_4
        |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|l|l|o|#| | | | | | | | | | | | | | | |
        |                                        ^
        | | | | | | | | | | | | | | | | |#|w|o|r|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |PRINT_4: #, # -> eL, lR PRINT_5
        |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|e|l|l|o|#| | | | | | | | | | | | | | |
        |                                        ^
        | | | | | | | | | | | | | | | |#|w|o|r|l|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |PRINT_5: #, # -> hL, dR STOP
        |#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|h|e|l|l|o|#| | | | | | | | | | | | | |
        |                                        ^
        | | | | | | | | | | | | | | |#|w|o|r|l|d|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#|#
        |                                        ^
        |Reached terminal state successfully!
        |""".stripMargin

    doRun("--program samples/HelloWorld_2d/hello.tur --input #|# -trace", EXPECTED)
  }
}
