package nsu.turing

import TestsCommon._
import base.Position.ZERO
import base._

import org.scalatest.funsuite.AnyFunSuite

class BaseSuite extends AnyFunSuite {

  test("Position") {
    assert(Position(0) == ZERO)
    assert(Position(0) < Position(1))
    assert(Position(10) < Position(10_000))
    assert(FAR_LEFT < FAR_RIGHT)

    assert(Position.min(Position(-5), Position(0), Position(7)) == Position(-5))
    assert(Position.max(Position(-15), Position(0), Position(17)) == Position(17))

    assert(Position.interval(Position(-2), Position(3)).toList == List(Position(-2), Position(-1), Position(0), Position(1), Position(2), Position(3)))
  }

  test("Alphabet") {
    Alphabet(DEFAULT_SYMBOL, l0, l1)
    Alphabet(DEFAULT_SYMBOL, l0, l1, l0)
    Alphabet(l0, l1, DEFAULT_SYMBOL)
    Alphabet(DEFAULT_SYMBOL)
    Alphabet(DEFAULT_SYMBOL, DEFAULT_SYMBOL)

    assertThrows[IllegalArgumentException] {
      Alphabet(Set(l0), DEFAULT_SYMBOL)
    }
  }

  test("Left Tape") {
    val L = LeftSidedTape(DEFAULT_SYMBOL)

    assert(L.defaultSymbol == DEFAULT_SYMBOL)
    assert(L.read(ZERO) == DEFAULT_SYMBOL)
    assert(L.read(FAR_LEFT) == DEFAULT_SYMBOL)

    assert(L.leftmostUsed() == ZERO)
    assert(L.rightmostUsed() == ZERO)

    assert(L.isValidPosition(ZERO))
    assert(L.isValidPosition(Position(-1)))
    assert(L.isValidPosition(FAR_LEFT))

    assert(!L.isValidPosition(Position(1)))
    assert(!L.isValidPosition(FAR_RIGHT))

    val L2 = L.withModifiedMemory(FAR_LEFT, l0)
    assert(L2.read(ZERO) == DEFAULT_SYMBOL)
    assert(L2.read(FAR_LEFT) == l0)
    assert(L2.read(Position(FAR_LEFT.offs + 1)) == DEFAULT_SYMBOL)
    assert(L2.read(Position(FAR_LEFT.offs - 1)) == DEFAULT_SYMBOL)

    assert(L2.leftmostUsed() == FAR_LEFT)
    assert(L2.rightmostUsed() == ZERO)

    assertThrows[IllegalArgumentException] { L2.withModifiedMemory(FAR_RIGHT, DEFAULT_SYMBOL) }
    assertThrows[IllegalArgumentException] { L2.read(FAR_RIGHT) }
  }

  test("Right Tape") {
    val R = RightSidedTape(DEFAULT_SYMBOL)

    assert(R.defaultSymbol == DEFAULT_SYMBOL)
    assert(R.read(ZERO) == DEFAULT_SYMBOL)
    assert(R.read(FAR_RIGHT) == DEFAULT_SYMBOL)

    assert(R.leftmostUsed() == ZERO)
    assert(R.rightmostUsed() == ZERO)

    assert(R.isValidPosition(ZERO))
    assert(R.isValidPosition(Position(1)))
    assert(R.isValidPosition(FAR_RIGHT))

    assert(!R.isValidPosition(Position(-1)))
    assert(!R.isValidPosition(FAR_LEFT))

    val R2 = R.withModifiedMemory(FAR_RIGHT, l1)
    assert(R2.read(ZERO) == DEFAULT_SYMBOL)
    assert(R2.read(FAR_RIGHT) == l1)
    assert(R2.read(Position(FAR_RIGHT.offs + 1)) == DEFAULT_SYMBOL)
    assert(R2.read(Position(FAR_RIGHT.offs - 1)) == DEFAULT_SYMBOL)

    assert(R2.leftmostUsed() == ZERO)
    assert(R2.rightmostUsed() == FAR_RIGHT)

    assertThrows[IllegalArgumentException] { R2.withModifiedMemory(FAR_LEFT, DEFAULT_SYMBOL) }
    assertThrows[IllegalArgumentException] { R2.read(FAR_LEFT) }
  }

  test("Dual Tape") {
    val D = InfiniteTape(DEFAULT_SYMBOL)

    assert(D.defaultSymbol == DEFAULT_SYMBOL)
    assert(D.read(ZERO) == DEFAULT_SYMBOL)
    assert(D.read(FAR_LEFT) == DEFAULT_SYMBOL)
    assert(D.read(FAR_RIGHT) == DEFAULT_SYMBOL)

    assert(D.leftmostUsed() == ZERO)
    assert(D.rightmostUsed() == ZERO)

    assert(D.isValidPosition(ZERO))
    assert(D.isValidPosition(Position(1)))
    assert(D.isValidPosition(FAR_RIGHT))
    assert(D.isValidPosition(Position(-1)))
    assert(D.isValidPosition(FAR_LEFT))

    val D2 = D
      .withModifiedMemory(FAR_LEFT, l0)
      .withModifiedMemory(FAR_RIGHT, l1)

    assert(D2.read(ZERO) == DEFAULT_SYMBOL)
    assert(D2.read(FAR_LEFT) == l0)
    assert(D2.read(Position(FAR_LEFT.offs - 1)) == DEFAULT_SYMBOL)
    assert(D2.read(Position(FAR_LEFT.offs + 1)) == DEFAULT_SYMBOL)

    assert(D2.read(FAR_RIGHT) == l1)
    assert(D2.read(Position(FAR_RIGHT.offs + 1)) == DEFAULT_SYMBOL)
    assert(D2.read(Position(FAR_RIGHT.offs - 1)) == DEFAULT_SYMBOL)

    assert(D2.leftmostUsed() == FAR_LEFT)
    assert(D2.rightmostUsed() == FAR_RIGHT)
  }

  test("Rules") {
    val r1 = Rule(Q, P, List(a), List(b), List(NoMove))
    assert(r1.dimension == 1)

    val r2 = Rule(F, Q, List(a, b), List(b, b), List(MoveLeft, MoveRight))
    assert(r2.dimension == 2)

    val alist = List(a)
    val r3 = Rule(Q, Q, alist, alist, List(NoMove))

    assertThrows[IllegalArgumentException] { Rule(Q, P, List(a), List(a, b), List(NoMove)) }
    assertThrows[IllegalArgumentException] { Rule(Q, P, List(a, b), List(a), List(NoMove)) }
    assertThrows[IllegalArgumentException] { Rule(Q, P, List(a), List(b), List(NoMove, MoveRight)) }
  }

  test("Program") {
    {
      val p = Program(BINARY_ALPHABET)
      assert(p.dimension == 0)
      assert(p.findRule(Q, List(a)).isEmpty)
    }

    {
      val r = Rule(P, Q, List(l0), List(l1), List(MoveRight))
      val p = Program(BINARY_ALPHABET, r)
      assert(p.dimension == 1)
      assert(p.findRule(Q, List(l0)).isEmpty)
      assert(p.findRule(Q, List(a)).isEmpty)
      assert(p.findRule(P, List(a)).isEmpty)
      assert(p.findRule(P, List(l1)).isEmpty)
      assert(p.findRule(P, List(l0)).get == r)
    }

    {
      val r1 = Rule(P, Q, List(l0, l0), List(l1, l1), List(NoMove, NoMove))
      val r2 = Rule(P, Q, List(l1, l1), List(l1, l1), List(MoveRight, MoveRight))
      val p = Program(BINARY_ALPHABET, r1, r2)
      assert(p.dimension == 2)
      assert(p.findRule(P, List(l0)).isEmpty)
      assert(p.findRule(Q, List(l0, l0)).isEmpty)
      assert(p.findRule(P, List(l0, l0)).get == r1)
      assert(p.findRule(P, List(l0, l1)).isEmpty)
      assert(p.findRule(P, List(l1, l1)).get == r2)
    }

    // read symbols
    assertThrows[IllegalArgumentException] { Program(BINARY_ALPHABET, Rule(P, Q, List(a), List(l0), List(NoMove))) }
    assertThrows[IllegalArgumentException] { Program(BINARY_ALPHABET, Rule(P, Q, List(l0, b), List(l0, l1), List(NoMove, NoMove))) }

    // write symbols
    assertThrows[IllegalArgumentException] { Program(BINARY_ALPHABET, Rule(P, Q, List(l0), List(a), List(NoMove))) }
    assertThrows[IllegalArgumentException] { Program(BINARY_ALPHABET, Rule(P, Q, List(l0, l1), List(l0, b), List(NoMove, NoMove))) }

    // diff dimensions
    val r1 = Rule(P, Q, List(l0), List(l0), List(NoMove))
    val r2 = Rule(P, Q, List(l1, l1), List(l0, l0), List(NoMove, NoMove))
    assertThrows[IllegalArgumentException] { Program(BINARY_ALPHABET, r1, r2) }

    // conflicting rules
    val r1_c1 = Rule(P, Q, List(l0), List(l0), List(MoveRight))
    val r1_c2 = Rule(P, Q, List(l0), List(l1), List(NoMove))
    assertThrows[IllegalArgumentException] { Program(BINARY_ALPHABET, r1, r1_c1) }
    assertThrows[IllegalArgumentException] { Program(BINARY_ALPHABET, r1, r1_c2) }
  }
}
