package nsu.turing

import base.State

import nsu.turing.CmdParser._

import scala.collection.mutable

sealed trait ProgramOption {
  def name: String
  def help: String
}

case class ProgramOptionImpl(name: String, help: String) extends ProgramOption

// defines mapping from String argument to target type (e.g. fileName => File instance)
sealed trait ProgramArg[T] extends (String => T) with ProgramOption
case class StringArg(name: String, help: String) extends ProgramArg[String] {
  override def apply(v1: String): String = v1
}
case class PositiveIntArg(name: String, help: String) extends ProgramArg[Int] {
  override def apply(v1: String): Int = Integer.parseInt(v1).ensuring(_ > 0)
}
case class StateArg(name: String, help: String) extends ProgramArg[State] {
  override def apply(v1: String): State = State(v1)
}

case class LazyList[T]() {
  private var initializationForbidden = false
  private var tempBuf = mutable.Buffer[T]()

  def add(o: T): T = {
    assert(!initializationForbidden)
    tempBuf.append(o)
    o
  }

  def list(): List[T] = {
    initializationForbidden = true
    val r = tempBuf.toList
    tempBuf = null
    r
  }
}

object Options {
  private val registry = LazyList[ProgramOptionImpl]()

  val HELP: ProgramOptionImpl = registry.add(ProgramOptionImpl("-help", "Prints this help."))
  val VERBOSE: ProgramOptionImpl = registry.add(ProgramOptionImpl("-verbose", "Enables internal emulator logging. Intended for debug only."))
  val TRACE: ProgramOptionImpl = registry.add(ProgramOptionImpl("-trace", "Logs every transition executed by a program. Intended for debug only."))

  lazy val list: List[ProgramOptionImpl] = registry.list()
}

object Arguments {
  private val registry = LazyList[ProgramArg[_]]()

  private val INPUT_ARG_NAME =  "--input"

  val INPUT_TAPE_SEPARATOR: StringArg = registry.add(StringArg("--input-tape-separator",
    " Input is split by given separator and resulting strings are " +
    s"considered to be the content of tapes. By default is equal to '$DEFAULT_TAPE_SEPARATOR'."))
    .asInstanceOf[StringArg]

  private val INPUT_CELL_SEPARATOR_NAME = "--input-cell-separator"
  val INPUT_CELL_SEPARATOR: StringArg = registry.add(StringArg(INPUT_CELL_SEPARATOR_NAME,
    " Tape content is split by given separator and resulting strings are " +
    s"considered to be the symbols of tapes. By default separator is empty." +
    s"\nExample #1:\n" +
    s"    $INPUT_ARG_NAME " + "\"ff,ss,tt\"" + s" $INPUT_CELL_SEPARATOR_NAME " + "\",\"\n" +
    "corresponds to\n" +
    "| ff | ss | tt | # | # |...\n" +
    s"\nExample #2:\n" +
    s"    $INPUT_ARG_NAME " + "\"ff,ss,tt\"\n" +
    "corresponds to\n" +
    "| f | f | , | s | s | , | t | t | # | # |...\n"
    ))
    .asInstanceOf[StringArg]

  val INPUT: ProgramArg[String] = registry.add(StringArg(INPUT_ARG_NAME,
    "Input to the program as a string. " +
      "By default, each character assumed to be a separate symbol " +
      s"(1-char alphabets are the most common case), special symbol '$DEFAULT_TAPE_SEPARATOR' " +
      "treated as a tape separator.\n" +
      s"For example, `#01##01#' corresponds to the following 1-tape configuration:\n" +
      "| # | 0 | 1 | # | # | 0 | 1 | # | # | # | ...\n\n" +
      s"and '#01#|#01#' corresponds to the following 2-tape configuration:\n" +
      s"| # | 0 | 1 | # | # ...\n" +
      s"| # | 0 | 1 | # | # ...\n\n" +
      s"Read help for ${INPUT_TAPE_SEPARATOR.name} and ${INPUT_CELL_SEPARATOR.name} " +
      "if you are interested in alphabets with multi-char symbols."))
    .asInstanceOf[ProgramArg[String]]


  val PROGRAM: StringArg = registry.add(StringArg("--program",
    "Relative or absolute path to the file with Turing machine program."))
    .asInstanceOf[StringArg]

  val TEST: StringArg = registry.add(StringArg("--test",
    "Relative or absolute path to the test file."))
    .asInstanceOf[StringArg]

  val MAX_TRANSITIONS: PositiveIntArg = registry.add(PositiveIntArg("--max-transitions",
    " Positive integer value defining max number of transitions for emulation. " +
    s"Default value is $DEFAULT_MAX_TRANSITIONS."))
    .asInstanceOf[PositiveIntArg]

  val START_STATE: StateArg = registry.add(StateArg("--start-state",
    s" Start state of emulation. Default value is $DEFAULT_START_STATE."))
    .asInstanceOf[StateArg]

  val TERMINATION_STATE: StateArg = registry.add(StateArg("--termination-state",
    s" Termination state of emulation. Default value is $DEFAULT_TERMINATION_STATE."))
    .asInstanceOf[StateArg]

  lazy val list: List[ProgramArg[_]] = registry.list()
}