package nsu.turing

import LoggingLevel.{IMPORTANT, IMPORTANT_NONEWLINE, LoggingLevel, TRACE, VERBOSE}
import base._

import scala.collection.mutable

object TestsCommon {

  case class TestLogger() {
    private val buf = mutable.Buffer[(String, LoggingLevel)]()
    def string(): String = {

      val r = new StringBuilder()
      for ((s, l) <- buf) {
        l match {
          case IMPORTANT => r.append(s + "\n")
          case IMPORTANT_NONEWLINE => r.append(s)
          case TRACE => r.append(s + "\n")
          case VERBOSE => r.append(s + "\n")
        }
      }
      r.toString()
    }

    def apply(s: String, l: LoggingLevel) = buf.append((s, l))
  }

  def fullyVerboseLogger(): (TestLogger, (String, LoggingLevel) => Unit) = logger(LoggingLevel.values)

  def logger(allowed: Set[LoggingLevel]): (TestLogger, (String, LoggingLevel) => Unit) = {
    val logger = TestLogger()
    val c = (s: String, l: LoggingLevel) => {
      if (allowed.contains(l)) logger(s, l)
      ()
    }
    (logger, c)
  }

  val DEFAULT_SYMBOL: Symbol = Symbol("#")
  val l0: Symbol = Symbol("0")
  val l1: Symbol = Symbol("1")
  val BINARY_ALPHABET: Alphabet = Alphabet(Set(l0, l1, DEFAULT_SYMBOL), DEFAULT_SYMBOL)

  val VERY_LARGE_INT: BigInt = BigInt(1_000_000_000L) * BigInt(1_000_000_000L) * BigInt(1_000_000_000L)
  val FAR_RIGHT: Position = Position(VERY_LARGE_INT)
  val FAR_LEFT: Position = Position(-1 * VERY_LARGE_INT)

  val f: Symbol = Symbol("#")
  val a: Symbol = Symbol("a")
  val b: Symbol = Symbol("b")
  val c: Symbol = Symbol("c")

  val Q: State = State("Q")
  val P: State = State("P")
  val F: State = State("F")

  val START: State = State("START")
  val STOP : State = State("STOP")
  val ERROR: State = State("ERROR")

  private val INVERT = State("INVERT")
  private val E = List(DEFAULT_SYMBOL)
  private val Z = List(l0)
  private val O = List(l1)
  private val R = List(MoveRight)
  private val L = List(MoveLeft)
  private val H = List(NoMove)

  val ALWAYS_LEFT_PROGRAM: Program = Program(
    BINARY_ALPHABET,
    /*
    START:
            # -> #L START
            0 -> 0L START
            1 -> 1L START
    */
    Rule(START, START, E, E, L),
    Rule(START, START, Z, Z, L),
    Rule(START, START, O, O, L),
  )

  val INVERT_PROGRAM: Program = Program(
    BINARY_ALPHABET,
    /*
    START:
            # -> #R INVERT
            0 -> #H ERROR
            1 -> #H ERROR
    */
    Rule(START, INVERT, E, E, R),
    Rule(START, ERROR, Z, E, H),
    Rule(START, ERROR, O, E, H),
    /*
    INVERT:
            # -> #H STOP
            0 -> 1R INVERT
            1 -> 0R INVERT
    */
    Rule(INVERT, STOP, E, E, H),
    Rule(INVERT, INVERT, Z, O, R),
    Rule(INVERT, INVERT, O, Z, R),
  )
}
