package nsu.turing

import base._


object PrettyPrint {

  def computationState(t: Tape, p: Position): String = {
    def spaces(n: Int) = Iterator.fill(n)(" ") mkString ""

    val (lb, rb) = t match {
      case LeftSidedTape(_, _) => (t.leftmostUsed(), Position.ZERO)
      case RightSidedTape(_, _) =>  (Position.ZERO, t.rightmostUsed())
      case InfiniteTape(_, _) => (t.leftmostUsed(), t.rightmostUsed())
    }

    val leftBorder = Position.min(Position(lb.offs - 1), Position(p.offs - 20))
    val rightBorder = Position.max(Position(rb.offs + 1), Position(p.offs + 20))

    val p1 = tapeSegment(t, leftBorder, p) + "|"
    val p2 = tapeSegment(t, Position(p.offs + 1), rightBorder).stripSuffix(" ")

    p1 + p2 + "\n" + spaces(p1.length - 2) + "^"
  }

  def tapeSegment(tape: Tape, fromInclusive: BigInt, toInclusive: BigInt): String = tapeSegment(tape, Position(fromInclusive), Position(toInclusive))

  def tapeSegment(tape: Tape, fromInclusive: Position, toInclusive: Position): String = {
    require(fromInclusive <= toInclusive)
    def idx2symbol(i: Position): String = if (tape.isValidPosition(i)) tape.read(i).value else " "
    Position.interval(fromInclusive, toInclusive) map idx2symbol mkString "|"
  }

  def direction(d: Direction): String = d match {
      case NoMove => "H"
      case MoveLeft => "L"
      case MoveRight => "R"
    }


  private val comma = ", "

  def rule(r: Rule): String = s"${r.from.name}: ${r.read.map(_.value).mkString(comma)} -> " +
      s"${r.write.zipWithIndex.map { case (s, i) => s.value + "" + direction(r.move(i)) } mkString comma} ${r.to.name}"



}
