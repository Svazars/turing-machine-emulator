package nsu.turing.base

abstract class Tape(val defaultSymbol: Symbol, private val memoryState: Map[Position, Symbol]) {
  def read(pos: Position): Symbol = {
    if (!isValidPosition(pos)) {
      throw new IllegalArgumentException(s"idx $pos is not valid")
    }

    memoryState.getOrElse(pos, defaultSymbol)
  }

  def withModifiedMemory(pos: Position, newValue: Symbol): Tape = {
    if (!isValidPosition(pos)) {
      throw new IllegalArgumentException(s"""idx $pos is not valid"""")
    }

    this.withMemory(pos, newValue)
  }

  def leftmostUsed(): Position = memoryState.keys.min
  def rightmostUsed(): Position = memoryState.keys.max

  def isValidPosition(pos: Position): Boolean
  protected def withMemory(pos: Position, sym: Symbol): Tape
}

case class InfiniteTape(override val defaultSymbol: Symbol, memoryState: Map[Position, Symbol]) extends Tape(defaultSymbol, memoryState) {
  override def isValidPosition(pos: Position): Boolean = true
  override protected def withMemory(pos: Position, sym: Symbol): Tape = InfiniteTape(defaultSymbol, memoryState + (pos -> sym))
}

object InfiniteTape {
  def apply(defaultSymbol: Symbol): InfiniteTape = InfiniteTape(defaultSymbol, Map(Position.ZERO -> defaultSymbol))
}

// infinite to the right
case class RightSidedTape(override val defaultSymbol: Symbol, memoryState: Map[Position, Symbol]) extends Tape(defaultSymbol, memoryState) {
  override def isValidPosition(pos: Position): Boolean = pos >= Position.ZERO
  override protected def withMemory(pos: Position, sym: Symbol): Tape = RightSidedTape(defaultSymbol, memoryState + (pos -> sym))
}

object RightSidedTape {
  def apply(defaultSymbol: Symbol): RightSidedTape = RightSidedTape(defaultSymbol, Map(Position.ZERO -> defaultSymbol))
}

// infinite to the left
case class LeftSidedTape(override val defaultSymbol: Symbol, memoryState: Map[Position, Symbol]) extends Tape(defaultSymbol, memoryState) {
  override def isValidPosition(pos: Position): Boolean = pos <= Position.ZERO
  override protected def withMemory(pos: Position, sym: Symbol): Tape = LeftSidedTape(defaultSymbol, memoryState + (pos -> sym))
}

object LeftSidedTape {
  def apply(defaultSymbol: Symbol): LeftSidedTape = LeftSidedTape(defaultSymbol, Map(Position.ZERO -> defaultSymbol))
}