package nsu.turing

import org.scalatest.funsuite.AnyFunSuite

class MainSuite extends AnyFunSuite{

  test("Run with no params") {
    val (t, c) = TestsCommon.fullyVerboseLogger()
    Main.doMain(c, Set.empty, Map.empty)
    val real = t.string()
    val expected = (Seq("No arguments for emulator provided.\n\n\n") ++ Main.help()).mkString  + "\n"
    assert(real == expected)
  }

  test("Run with help") {
    val (t, c) = TestsCommon.fullyVerboseLogger()
    Main.doMain(c, Set(Options.HELP), Map.empty)
    val EXPECTED =
      """-help          Prints this help.
        |
        |-verbose       Enables internal emulator logging. Intended for debug only.
        |
        |-trace         Logs every transition executed by a program. Intended for debug only.
        |
        |--input-tape-separator Input is split by given separator and resulting strings are considered to be the content of tapes. By default is equal to '|'.
        |
        |--input-cell-separator Tape content is split by given separator and resulting strings are considered to be the symbols of tapes. By default separator is empty.
        |Example #1:
        |    --input "ff,ss,tt" --input-cell-separator ","
        |corresponds to
        || ff | ss | tt | # | # |...
        |
        |Example #2:
        |    --input "ff,ss,tt"
        |corresponds to
        || f | f | , | s | s | , | t | t | # | # |...
        |
        |
        |--input        Input to the program as a string. By default, each character assumed to be a separate symbol (1-char alphabets are the most common case), special symbol '|' treated as a tape separator.
        |For example, `#01##01#' corresponds to the following 1-tape configuration:
        || # | 0 | 1 | # | # | 0 | 1 | # | # | # | ...
        |
        |and '#01#|#01#' corresponds to the following 2-tape configuration:
        || # | 0 | 1 | # | # ...
        || # | 0 | 1 | # | # ...
        |
        |Read help for --input-tape-separator and --input-cell-separator if you are interested in alphabets with multi-char symbols.
        |
        |--program      Relative or absolute path to the file with Turing machine program.
        |
        |--test         Relative or absolute path to the test file.
        |
        |--max-transitions Positive integer value defining max number of transitions for emulation. Default value is 10000.
        |
        |--start-state   Start state of emulation. Default value is START.
        |
        |--termination-state Termination state of emulation. Default value is STOP.
        |""".stripMargin
    assert(t.string() == EXPECTED)
  }
}
