package nsu.turing.base

import scala.collection.{immutable, mutable}

case class Program(alphabet: Alphabet, rules: immutable.Set[Rule]) {
  require(rules.flatMap(_.read).forall(alphabet.symbols.contains),
    s"Program reads symbols not from an alphabet." +
    s"\nAlphabet = $alphabet. " +
    s"\nError rules = ${rules.filter {!_.read.forall(alphabet.symbols.contains)}}")

  require(rules.flatMap(_.write).forall(alphabet.symbols.contains),
    s"Program writes symbols not from an alphabet." +
    s"\nAlphabet = $alphabet. " +
    s"\nError rules = ${rules.filter {!_.write.forall(alphabet.symbols.contains)}}")

  require(rules.forall(_.dimension == dimension), "Program contains rules with different dimensions.")

  def dimension: Int = rules.headOption.map(_.dimension).getOrElse(0)

  private val searchMap = Program.buildSearchMap(rules)

  def findRule(currentState: State, currentSymbols: Seq[Symbol]): Option[Rule] = searchMap.get((currentState, currentSymbols))
}

object Program {

  def apply(alphabet: Alphabet, rules: Rule*): Program = Program(alphabet, rules.toSet)

  private def buildSearchMap(rules: immutable.Set[Rule]): immutable.Map[(State, Seq[Symbol]), Rule] = {
    val unique = mutable.Map[(State, Seq[Symbol]), Rule]()
    rules.foreach { r =>
      val key: (State, List[Symbol]) = (r.from, r.read)
      if (unique.contains(key)) {
        throw new IllegalArgumentException(s"Conflicting rules: $r and ${unique(key)}")
      } else {
        unique.put(key, r)
      }
    }
    unique.toMap
  }
}