package nsu.turing.base

import scala.collection.immutable

case class Symbol(value: String)
case class State(name: String)
case class Alphabet(symbols: immutable.Set[Symbol], defaultSymbol: Symbol) {
  require(symbols.contains(defaultSymbol))
}

object Alphabet {
  def apply(defaultSymbol: Symbol, restSymbols: Symbol*): Alphabet = Alphabet(restSymbols.appended(defaultSymbol).toSet, defaultSymbol)
}
