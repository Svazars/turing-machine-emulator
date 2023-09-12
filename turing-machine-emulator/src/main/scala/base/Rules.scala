package nsu.turing.base

import nsu.turing.PrettyPrint

sealed trait Direction {
  def computePosition(p: Position): Position
}

case object MoveRight extends Direction {
  override def computePosition(p: Position): Position = Position(p.offs + 1)
}

case object MoveLeft extends Direction {
  override def computePosition(p: Position): Position = Position(p.offs - 1)
}

case object NoMove extends Direction {
  override def computePosition(p: Position): Position = p
}

case class Rule(from: State, to: State, read: List[Symbol], write: List[Symbol], move: List[Direction]) {
  require(write.length == dimension)
  require(move.length == dimension)

  def dimension: Int = read.length

  override def toString: String = PrettyPrint.rule(this)
}
