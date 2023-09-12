package nsu.turing

import nsu.turing.TestsCommon.{DEFAULT_SYMBOL, INVERT_PROGRAM}
import nsu.turing.base.RightSidedTape
import nsu.turing.parser.Parser
import org.scalatest.funsuite.AnyFunSuite

class ParserSuite extends AnyFunSuite {

  test("Invert from samples") {
    val SOURCE =
      """
        |alphabet = [ #, 0, 1]
        |tapes = [ right ]
        |
        |START:
        |    # -> #R INVERT
        |    0 -> #H ERROR
        |    1 -> #H ERROR
        |
        |INVERT:
        |    # -> #H STOP
        |    0 -> 1R INVERT
        |    1 -> 0R INVERT
        |
        |""".stripMargin

    val desc = Parser.parse(SOURCE.split("\n").iterator)
    assert(desc.program == INVERT_PROGRAM)
    assert(desc.tapes == List(RightSidedTape(DEFAULT_SYMBOL)))
  }

  test("Empty source") {
    assertThrows[IllegalArgumentException] {
      Parser.parse(Iterator.empty)
    }
  }

  test("Blank source") {
    assertThrows[IllegalArgumentException] { Parser.parse(List("", "        ", "").iterator) }
    assertThrows[IllegalArgumentException] { Parser.parse(List("", "// commented        ", "            // dsdf  //").iterator) }
  }

  test("No tapes") {
    assertThrows[IllegalArgumentException] { Parser.parse(List("alphabet = [x, y, z]").iterator) }
  }

  test("No program") {
    assertThrows[IllegalArgumentException] { Parser.parse(List("alphabet = [x, y, z]", "tapes = [left]").iterator) }
  }

  test("Alphabet & tapes switch") {
    assertThrows[IllegalArgumentException] { Parser.parse(List("tapes = [left]", "alphabet = [x, y, z]").iterator) }
  }
}
