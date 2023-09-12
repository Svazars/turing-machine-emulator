package nsu.turing.base

case class Position(offs: BigInt) extends Ordered[Position] {
  override def compare(that: Position): Int = this.offs.compare(that.offs)
}

object Position {
  val ZERO: Position = Position(0)

  def min(xs: Position*): Position = xs.min
  def max(xs: Position*): Position = xs.max

  def interval(fromInclusive: Position, toInclusive: Position): Iterator[Position] = {
    new Iterator[Position] {
      val stopVal: BigInt = toInclusive.offs + 1
      var currentVal: BigInt = fromInclusive.offs

      override def hasNext: Boolean = currentVal != stopVal

      override def next(): Position = {
        val r = currentVal
        currentVal = currentVal + 1
        Position(r)
      }
    }
  }
}

case class Head(currentState: State, positions: List[Position])
