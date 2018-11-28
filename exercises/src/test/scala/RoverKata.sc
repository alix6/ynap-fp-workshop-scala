package exercises.answers

import minitest._

/*
 * Pattern match enable the structural recurtion
 * a fancy name to express a way to distch logic
 * by type and data. It goes hand in hand with ADT
 * specially Sum Type. Think, how we can implement
 * some special logic `foo` for an "exclusive-or"
 * data type?
 */

object RoverKata extends SimpleTestSuite {

  case class Position( x:Int, y:Int ){
    def copy(position: Position) : Position = {
      Position(position.x,position.y)
    }
  }

  def getXPos(position: Position): Int = {
    position.x
  }

  case class Rover(P: Position, direction: Direction)

  sealed trait Commands
  case object Forward extends Commands
  case object Backward extends Commands
  case object Right extends Commands
  case object Left extends Commands

  def moveBackward(rover: Rover) : Rover = {

  }

  def move(rover: Rover, commands: Commands): Rover = {
    commands match{
      case Forward => moveForward(rover)
      case Backward => moveBackward(rover)
      case Left =>
    }
  }
  def moveForward(rover : Rover): Rover = {

    rover.direction match {
      case N() => Position
      case E() =>
      case W() =>
      case S() =>
    }
  }
  def moveLeft(rover : Rover): Rover = {
    Rover(rover.P, rover.direction.turnLeft)
  }
  def moveRight(rover : Rover): Rover = {
    Rover(rover.P, rover.direction.turnRight)
  }

  sealed trait Direction {
    def turnRight: Direction = this match {
      case N() => E()
      case E() => S()
      case S() => W()
      case W() => N()
    }
    def turnLeft: Direction = this match {
      case N() => W()
      case W() => S()
      case S() => E()
      case E() => N()
    }
  }
  case class N() extends Direction
  case class E() extends Direction
  case class W() extends Direction
  case class S() extends Direction

  // Alternative style, same implementation
  // but place it in the companion object
  object Direction {

    def turnRight(current: Direction): Direction =
      current match {
        case N() => E()
        case E() => S()
        case S() => W()
        case W() => N()
      }

    def turnLeft(current: Direction): Direction =
      current match {
        case N() => W()
        case W() => S()
        case S() => E()
        case E() => N()
      }
  }

  test("turn right") {
    assertEquals(N().turnRight, E())
    assertEquals(E().turnRight, S())
    assertEquals(S().turnRight, W())
    assertEquals(W().turnRight, N())
  }

  test("turn left") {
    assertEquals(N().turnLeft, W())
    assertEquals(W().turnLeft, S())
    assertEquals(S().turnLeft, E())
    assertEquals(E().turnLeft, N())
  }
}
