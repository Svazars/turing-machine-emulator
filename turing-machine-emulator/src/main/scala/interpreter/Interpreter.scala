package nsu.turing
package interpreter

import base._

import nsu.turing.interpreter.ExecutionContext.isOutOfTape
import nsu.turing.interpreter.Stepping.StepResult

import scala.annotation.tailrec
import scala.collection.mutable

case class ExecutionContext(program: Program, head: Head, terminalState: State, tapes: List[Tape], prevContext: Option[ExecutionContext]) {
  require(head.positions.length == dimension, s"Dimension = $dimension, head.positions = ${head.positions.length}")
  require(!isOutOfTape(head.positions, tapes))
  require(tapes.length == dimension)
  require(prevContext.forall(_.program == program))
  require(prevContext.forall(_.terminalState == terminalState))

  def dimension: Int = program.dimension
}

object ExecutionContext {
  def isOutOfTape(positions: List[Position], tapes: List[Tape]): Boolean = positions.zip(tapes).exists {case (p, t) => !t.isValidPosition(p) }
}

object Stepping {
  sealed trait StepResult {
    def context: ExecutionContext
  }
  case class Success(rule: Rule, context: ExecutionContext) extends StepResult
  case class Terminated(rule: Rule, context: ExecutionContext) extends StepResult
  case class OutOfTape(rule: Rule, context: ExecutionContext) extends StepResult
  case class RuleNotFound(context: ExecutionContext) extends StepResult
}

sealed trait ExecutionResult
case class Success(lastStep: Stepping.Terminated) extends ExecutionResult
case class OutOfTape(lastStep: Stepping.OutOfTape) extends ExecutionResult
case class RuleNotFound(lastStep: Stepping.RuleNotFound) extends ExecutionResult
case class OutOfIterations(lastStep: Stepping.Success) extends ExecutionResult

object Interpreter {

  def tapesWithData(tapes: List[Tape], data: Array[List[Symbol]]): Iterator[Tape] = tapes.zip(data).iterator.map {
    case (t, c) => c.zipWithIndex.foldLeft(t) { case (t, (c, p)) => t.withModifiedMemory(Position(p), c) }
  }

  def initialContext(p: Program, s: State, t: State, tapes: Tape*): ExecutionContext = initialContext(p, s, t, tapes.iterator)
  def initialContext(p: Program, s: State, t: State, tapes: Iterator[Tape]): ExecutionContext = initialContext(p, s, t, { _ => tapes.next()})

  def initialContext(p: Program, s: State, t: State, tapeGen: Unit => Tape): ExecutionContext = {
    val dim = p.dimension
    val tapes = (0 until dim).map(_ => tapeGen()).toList
    val positions = (0 until dim).map(_ => Position.ZERO).toList

    ExecutionContext(p, Head(s, positions), t, tapes, Option.empty)
  }

  def step(context: ExecutionContext): StepResult = step(context, {_ => })

  def step(context: ExecutionContext, progressLog: String => Unit): StepResult = {
    val (state, positions, tapes, program, terminalState) = (context.head.currentState, context.head.positions, context.tapes, context.program, context.terminalState)
    assert(state != terminalState)

    val currentSymbols = positions.zipWithIndex.map { case (pos, tapeNum) => tapes(tapeNum).read(pos) }

    program.findRule(state, currentSymbols) match {
      case Some(rule) =>
        assert(state == rule.from)
        assert(rule.read == currentSymbols)
        assert(positions.length == rule.dimension)
        assert(tapes.length == rule.dimension)

        val newTapes = tapes.zip(positions).zip(rule.write).map {
          case ((tape, pos), writeSymbol) => tape.withModifiedMemory(pos, writeSymbol)
        }

        val newPositions = rule.move.zip(positions).map {
          case (direction, oldPosition) => direction.computePosition(oldPosition)
        }

        progressLog(rule.toString)
        logState(newTapes, newPositions, progressLog)

        if (isOutOfTape(newPositions, newTapes)) {
          assert(isOutOfTape(newPositions, tapes)) // paranoia: out-of-tape property should be the same on every iteration (do not allow rules that change left-infinite tape to right-infinite)
          return Stepping.OutOfTape(rule, context)
        }

        val newHead = Head(rule.to, newPositions)
        val newContext = ExecutionContext(
          program = program,
          head = newHead,
          terminalState = terminalState,
          tapes = newTapes,
          prevContext = Some(context)
        )

        if (rule.to == terminalState) {
          Stepping.Terminated(rule, newContext)
        } else {
          Stepping.Success(rule, newContext)
        }

      case None => Stepping.RuleNotFound(context)
    }
  }

  def fullTraceOf(result: ExecutionResult): List[ExecutionContext] = {
    val s = result match {
      case Success(s) => s
      case OutOfTape(s) => s
      case RuleNotFound(s) => s
      case OutOfIterations(s) => s
    }
    val stack = mutable.Stack[ExecutionContext]()
    traceOf(s.context, stack)
    stack.toList
  }

  @tailrec
  private def traceOf(context: ExecutionContext, stack: mutable.Stack[ExecutionContext]): Unit = {
    stack.push(context)
    context.prevContext match {
      case Some(prev) => traceOf(prev, stack)
      case None =>
    }
  }

  private def logState(e: ExecutionContext, progressLog: String => Unit): Unit = logState(e.tapes, e.head.positions, progressLog)

  private def logState(ts: List[Tape], ps: List[Position], progressLog: String => Unit): Unit = ts.zip(ps).foreach {
    case (t, p) => progressLog(PrettyPrint.computationState(t, p))
  }

  def execute(startContext: ExecutionContext, maxTraceLength: Int, progressLog: String => Unit): ExecutionResult = {
    require(maxTraceLength >= 2)
    progressLog(s"Start execution from state: ${startContext.head.currentState.name}")
    logState(startContext, progressLog)
    executeImpl(startContext, 1, maxTraceLength, progressLog)
  }

  @tailrec
  private def executeImpl(startContext: ExecutionContext, currentIter: Int, maxIter: Int, progressLog: String => Unit): ExecutionResult = {
    require(maxIter >= 1)
    require((1 <= currentIter) && (currentIter <= maxIter))

    step(startContext, progressLog) match {
      case s @ Stepping.Success(rule, context) =>
        if (currentIter == maxIter - 1) {
          OutOfIterations(s)
        } else {
          executeImpl(context, currentIter + 1, maxIter, progressLog)
        }

      case t: Stepping.Terminated => Success(t)
      case oot: Stepping.OutOfTape => OutOfTape(oot)
      case rnf: Stepping.RuleNotFound => RuleNotFound(rnf)
    }
  }
}

