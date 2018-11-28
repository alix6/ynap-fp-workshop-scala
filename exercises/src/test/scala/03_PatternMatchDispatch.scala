package exercises

import minitest._

/*
 * Pattern match enable the structural recurtion
 * a fancy name to express a way to distch logic
 * by type and data. It goes hand in hand with ADT
 * specially Sum Type. Think, how we can implement
 * some special logic `foo` for an "exclusive-or"
 * data type?
 */

object PatternMatchDispatch extends SimpleTestSuite {

  /*
   * TODO: rewrite the dispatch logic
   *       from polymorphic dispatch (a fundamental OOP technique)
   *       to pattern match dispatch.
   *       Keep tests green.
   */

  def turnRight(d: Direction): Direction ={
    d match {
      case N() => E()
      case E() => S()
      case W() => N()
      case S() => W()
    }
  }
  def turnLeft(d: Direction): Direction ={
    d match {
      case N() => W()
      case W() => S()
      case S() => E()
      case E() => N()
    }
  }
  sealed trait Direction
  case class N() extends Direction
  case class E() extends Direction
  case class W() extends Direction
  case class S() extends Direction

  test("turn right") {
    assertEquals(turnRight(N()), E())
    assertEquals(turnRight(E()), S())
    assertEquals(turnRight(S()), W())
    assertEquals(turnRight(W()), N())
  }

  test("turn left") {
    assertEquals(turnLeft(N()), W())
    assertEquals(turnLeft(W()), S())
    assertEquals(turnLeft(S()), E())
    assertEquals(turnLeft(E()), N())
  }
}
