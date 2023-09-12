package nsu.turing

import org.scalatest.funsuite.AnyFunSuite

class CmdParsingSuite extends AnyFunSuite {

  test("Empty params") {
    val (opts, args) = CmdParser.parseCmd(Array.empty)
    assert(opts.isEmpty)
    assert(args.isEmpty)
  }

  test("Help param") {
    val (opts, args) = CmdParser.parseCmd(Array(Options.HELP.name))
    assert(opts == Set(Options.HELP))
    assert(args.isEmpty)
  }
}
