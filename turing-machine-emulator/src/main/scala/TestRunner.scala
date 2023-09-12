package nsu.turing

import CmdParser.processInput
import base._
import interpreter._
import parser.Parser

import scala.io.Source

object TestRunner {

  def do_tests(output: (String, LoggingLevel.LoggingLevel) => Unit, testFile: String, desc: RunDescription, maxIter: Int): Unit = {
    val testContent = Parser.parseTestFile(Source.fromFile(testFile).getLines())

    output(s"Running ${testContent.size} tests:", LoggingLevel.IMPORTANT)
    for (((n, i, o), testId) <- testContent.zipWithIndex) {
      output(s"  ${testId + 1} / ${testContent.size}: $n ...", LoggingLevel.IMPORTANT_NONEWLINE)
      val expected = o match {
        case "ERROR" => None
        case _ => Some(processInput(o, desc.tapeSeparator, desc.cellSeparator))
      }
      val input = processInput(i, desc.tapeSeparator, desc.cellSeparator)

      runTest(output, desc, maxIter, input, expected) match {
        case Left(its) => output(s"success in $its iterations.", LoggingLevel.IMPORTANT)
        case Right((msg, trace)) => output("failed!", LoggingLevel.IMPORTANT)
          output("Failure: " + msg, LoggingLevel.IMPORTANT)
          output("Error trace:", LoggingLevel.IMPORTANT)
          output("============", LoggingLevel.IMPORTANT)
          trace foreach { tr =>
            output("", LoggingLevel.IMPORTANT)
            tr.tapes.zip(tr.head.positions).foreach { case (tape, pos) =>
              output(PrettyPrint.computationState(tape, pos), LoggingLevel.IMPORTANT)
            }
          }

          output("============", LoggingLevel.IMPORTANT)
          return
      }
    }
  }

  def runTest(output: (String, LoggingLevel.LoggingLevel) => Unit, desc: RunDescription, maxIter: Int, input: Array[List[Symbol]], expected: Option[Array[List[Symbol]]]): Either[Int, (String, List[ExecutionContext])] = {

    def compare(tape: Tape, output: List[Symbol]): Boolean = {
      val ds = tape.defaultSymbol
      for (p <- Position.interval(tape.leftmostUsed(), tape.rightmostUsed())) {
        if ((p < Position.ZERO) || (p >= Position(output.length))) {
          if (tape.read(p) != ds) {
            return false
          }
        } else {
          if (tape.read(p) != output(p.offs.toInt)) {
            return false
          }
        }
      }
      true
    }

    val emulation = Interpreter.execute(
      Interpreter.initialContext(desc.td.program, desc.startState, desc.terminalState, Interpreter.tapesWithData(desc.td.tapes, input)),
      maxIter,
      { s => output(s, LoggingLevel.TRACE) }
    )

    val trace = Interpreter.fullTraceOf(emulation)
    emulation match {
      case Success(s) =>
        expected match {
          case Some(expectedData) =>
            if (s.context.tapes.zip(expectedData).forall { case (t, d) => compare(t, d) }) {
              Left(trace.length)
            } else {
              Right(("Actual output differs from expected", trace))
            }

          case None => Right(("Reached termination state while expected ERROR.", trace))
        }

      case OutOfIterations(s) =>
        Right((s"Performed ${trace.length} transitions and not reached termination or error state. Maybe you should increase ${Arguments.MAX_TRANSITIONS.name}?", trace))

      case err => expected match {
        case Some(e) => Right((s"Encountered error $err while expected to finish test", trace))
        case _ => Left(trace.length)
      }
    }
  }
}
