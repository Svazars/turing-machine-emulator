package nsu.turing.parser

import nsu.turing.base._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

case class TuringMachineDescription(program: Program, tapes: List[Tape])

case class PartialRule(readSymbols: Iterable[Symbol], writeSymbols: Iterable[Symbol], directions: Iterable[Direction], toState: State)

object Parser {

  def parse(filePath: String): TuringMachineDescription = parse(Source.fromFile(filePath).getLines())

  def parse(content: Iterator[String]): TuringMachineDescription = parseImpl(content.flatMap(ignoreEmptyAndTrailingComments))

  private def parseImpl(content: Iterator[String]): TuringMachineDescription = {
    val alphabet = Atoms.parseAlphabet(content)
    val tapes = Atoms.parseTapes(content, alphabet.defaultSymbol).toList
    val rules = parseProgram(content).toList

    TuringMachineDescription(Program(alphabet, requireUnique(rules)), tapes)
  }

  private def trimAndIgnoreEmpty(str: String): Option[String] = Some(str.trim).flatMap(s => if (s.isEmpty) None else Some(s))

  private def ignoreEmptyAndTrailingComments(str: String): Option[String] = trimAndIgnoreEmpty(
    str.indexOf("//") match {
      case -1 => str
      case n: Int => str.substring(0, n)
    }
  )

  private def parseProgram(data: Iterator[String]): Iterable[Rule] = {
    require(data.hasNext, "File does not contain any program rule")

    def makeRule(header: String, pRule: PartialRule): Rule = Rule(
      State(header.dropRight(1)),
      pRule.toState, pRule.readSymbols.toList, pRule.writeSymbols.toList, pRule.directions.toList
    )

    val rules = ArrayBuffer[Rule]()
    var header = data.next
    do {
      val (rhs, nextHeader) = Atoms.parseBlock(header, data)
      for (r <- rhs) {
        val partialRule = Atoms.parsePartialRule(r)
        rules.append(makeRule(header, partialRule))
      }

      nextHeader match {
        case Some(h) => header = h
        case None => return rules
      }
    } while (true)

    throw new UnsupportedOperationException("Should not reach here")
  }

  private def requireUnique[T](elements: Iterable[T]): Set[T] = {
    val unique = elements.toSet
    if (elements.size != unique.size) {
      val (duplicate, _) = elements.zipWithIndex.groupMap(_._1)(_._2).find(_._2.size > 1).get
      throw new UnsupportedOperationException(s"Duplicated $duplicate")
    }
    unique
  }

  private object Atoms {

    def parseAlphabet(data: Iterator[String]): Alphabet = {
      require(data.hasNext, "First non-blank line should define alphabet")
      val rhs = getRvalueFromAssignment(data.next, "alphabet")
      val symbols = parseArray(rhs).map(Symbol.apply)
      Alphabet(requireUnique(symbols), symbols.head)
    }

    private def getRvalueFromAssignment(assignment: String, lvalue: String): String = {
      assignment.split("=") match {
        case Array(head, tail) if head.trim == lvalue => tail
        case _ => throw new IllegalArgumentException(s"$assignment is not a valid assignment to $lvalue")
      }
    }

    private def parseArray(rawStr: String): Iterable[String] = {
      (rawStr.indexOf('['), rawStr.indexOf(']')) match {
        case (-1, _) => throw new UnsupportedOperationException(s"$rawStr is not a valid array: '[' missing")
        case (_, -1) => throw new UnsupportedOperationException(s"$rawStr is not a valid array: ']' missing")
        case (l, r) if l < r => rawStr.substring(l + 1, r).split(',').flatMap(trimAndIgnoreEmpty)
        case _ => throw new UnsupportedOperationException(s"$rawStr is not a valid array")
      }
    }

    def parseTapes(data: Iterator[String], defSymbol: Symbol): Iterable[Tape] = {
      require(data.hasNext, "Second non-blank line should define tapes")
      val rhs = getRvalueFromAssignment(data.next, "tapes")
      parseArray(rhs).map(parseTape(defSymbol))
    }

    private def parseTape(default: Symbol)(str: String): Tape = str match {
      case "left" => LeftSidedTape(default)
      case "right" => RightSidedTape(default)
      case "dual" => InfiniteTape(default)

      case _ => throw new UnsupportedOperationException(s"$str is not a valid tape type")
    }

    def parseBlock(header: String, data: Iterator[String]): (List[String], Option[String]) = {
      if (!isBlockStart(header)) {
        throw new UnsupportedOperationException(s"$header is not a valid block start")
      }

      val rules = ArrayBuffer[String]()
      while (data.hasNext) {
        val rule = data.next
        if (isBlockStart(rule)) {
          return (rules.toList, Some(rule))
        }

        rules.append(rule)
      }

      (rules.toList, None)
    }

    private def isBlockStart(str: String): Boolean = str.trim.endsWith(":")

    def parsePartialRule(rule: String): PartialRule = {
      rule.split("->") match {
        case Array(rawReadSymbols, rawOther) =>
          val readSymbols = rawReadSymbols.split(",").flatMap(trimAndIgnoreEmpty).map(Symbol.apply)

          val others = rawOther.split(" ")
          others.splitAt(others.length - 1) match {
            case (ws, Array(s)) =>
              val toState = State(s)
              val (writeSymbols, directions) = (ws mkString "").split(',').map(str => {
                str.splitAt(str.length - 1) match {
                  case (s, d) => (Symbol(s), parseDirection(d))
                }
              }).unzip

              PartialRule(readSymbols, writeSymbols, directions, toState)
          }

        case _ => throw new UnsupportedOperationException(s"$rule is not valid rule")
      }
    }

    private def parseDirection(c: String): Direction = c.trim match {
      case "R" => MoveRight
      case "L" => MoveLeft
      case "H" => NoMove
    }
  }

  def parseTestFile(content: Iterator[String]): List[(String, String, String)] = {
    content.flatMap(ignoreEmptyAndTrailingComments).map { line: String =>
        val t = line.split(":")
        val name = t(0).trim
        val tt = t(1).split("->")
        val input = tt(0).trim
        val output = tt(1).trim

        (name, input, output)

    }.toList
  }
}
