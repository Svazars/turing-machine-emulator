package nsu.turing

import CmdParser._
import LoggingLevel.{IMPORTANT, IMPORTANT_NONEWLINE, LoggingLevel, TRACE, VERBOSE}
import base.State
import interpreter._
import parser.{Parser, TuringMachineDescription}

import scala.util.matching.Regex

case class RunDescription(td: TuringMachineDescription, startState: State, terminalState: State, tapeSeparator: String, cellSeparator: Option[String])

object LoggingLevel extends Enumeration {
  type LoggingLevel = Value
  val IMPORTANT, IMPORTANT_NONEWLINE, TRACE, VERBOSE = Value
}

object Main {

  def main(args: Array[String]): Unit = {
    val (o, a) = CmdParser.parseCmd(args)

    val logger = (s: String, l: LoggingLevel) => {
      l match {
        case IMPORTANT => println(s)
        case IMPORTANT_NONEWLINE => print(s)
        case TRACE if o.contains(Options.TRACE) => println(s)
        case VERBOSE if o.contains(Options.VERBOSE)=> println(s)
        case _ => // ignore
      }
    }

    doMain(logger, o, a)
  }

  def help(): String = {
    val CMD_SIZE = 15
    val MAX_LINE_SIZE = 100

    def formatOptName(o: String): String = o.padTo(CMD_SIZE, " ") mkString ""
    def format(str: String) = {
      str
      // str.split("\n") map {
      //   _.grouped(MAX_LINE_SIZE).map(_.trim).mkString("\n")
      // } mkString "\n"
    }

    (Options.list ++ Arguments.list).map(o => format(formatOptName(o.name) + o.help)) mkString("\n\n")
  }

  def doMain(output: (String, LoggingLevel.LoggingLevel) => Unit, o: Set[ProgramOption], a: Map[ProgramArg[_], String]): Unit = {
    def printHelp(): Unit = output(help(), IMPORTANT)

    if (o.contains(Options.HELP)) {
      printHelp()
      return
    }

    if (a.isEmpty) {
      output("No arguments for emulator provided.\n\n", IMPORTANT)
      printHelp()
      return
    }

    if (!a.contains(Arguments.PROGRAM)) {
      output(s"Program file not provided. Consider adding ${Arguments.PROGRAM.name} option.", IMPORTANT)
      return
    }

    val startState = Arguments.START_STATE(a.getOrElse(Arguments.START_STATE, DEFAULT_START_STATE))
    output(s"Start state: $startState", VERBOSE)

    val termState = Arguments.TERMINATION_STATE(a.getOrElse(Arguments.TERMINATION_STATE, DEFAULT_TERMINATION_STATE))
    output(s"Termination state: $termState", VERBOSE)

    val programFilePath = a(Arguments.PROGRAM)
    output(s"Program path: $programFilePath", VERBOSE)

    val description = Parser.parse(programFilePath)
    output(s"Parsed program from $programFilePath", VERBOSE)
    output(s"program: [${description.program}]", VERBOSE)
    output(s"tapes: [${description.tapes mkString ","}]", VERBOSE)

    val maxIter = Arguments.MAX_TRANSITIONS(a.getOrElse(Arguments.MAX_TRANSITIONS, DEFAULT_MAX_TRANSITIONS))
    output(s"maxIter: $maxIter", VERBOSE)

    val tapeSeparator = Regex.quote(a.getOrElse(Arguments.INPUT_TAPE_SEPARATOR, DEFAULT_TAPE_SEPARATOR))
    output(s"tape-separator: $tapeSeparator", VERBOSE)

    val cellSeparator = a.get(Arguments.INPUT_CELL_SEPARATOR)
    output(s"cell-separator: $cellSeparator", VERBOSE)

    val rdesc = RunDescription(description, startState, termState, tapeSeparator, cellSeparator)

    if (a.contains(Arguments.TEST)) {
      val testFile = a(Arguments.TEST)
      output(s"Running tests for $programFilePath from $testFile", VERBOSE)
      TestRunner.do_tests(output, testFile, rdesc, maxIter)
    } else {
      output("Parsing input argument.", VERBOSE)
      if (!a.contains(Arguments.INPUT)) {
        output(s"Cannot run turing machine: input is not provided. Consider adding ${Arguments.INPUT.name}", IMPORTANT)
        return
      }
      val input = a(Arguments.INPUT)
      output(s"Raw input: `$input`", VERBOSE)

      val tapesContent = processInput(input, tapeSeparator, cellSeparator)
      val alphabet = description.program.alphabet
      tapesContent.flatten.toSet.find(s => !alphabet.symbols.contains(s)) match {
        case Some(unexpectedSymbol) =>
          output(s"Cannot run turing machine with input $input: $unexpectedSymbol is not compatible with alphabet ${alphabet.symbols.mkString(",")}", IMPORTANT)
          return

        case _ =>
      }

      val PRINTABLE_CELL_SEPARATOR = "|"
      output(s"Input:", VERBOSE)
      for (tape <- tapesContent) {
        output(s"  ${PRINTABLE_CELL_SEPARATOR + (tape mkString PRINTABLE_CELL_SEPARATOR) + PRINTABLE_CELL_SEPARATOR}", VERBOSE)
      }

      val start = Interpreter.initialContext(description.program, startState, termState, Interpreter.tapesWithData(description.tapes, tapesContent))
      Interpreter.execute(start, maxIter, {s => output(s, TRACE)}) match {
        case Success(_) => output("Reached terminal state successfully!", IMPORTANT)
        case OutOfTape(_) => output("Error: out-of-tape access", IMPORTANT)
        case RuleNotFound(_) => output("Error: rule not found", IMPORTANT)
        case OutOfIterations(_) => output(s"Error: not finished in $maxIter iterations", IMPORTANT)
      }
    }
  }
}
