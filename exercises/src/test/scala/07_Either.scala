package exercises

import minitest._

/*
 * Functions can't always return a value.
 * In this scenario they are called: partial functions.
 * We can convert them into total functions
 * with the introduction of effects.
 *
 *  f:  InType => Effect[OutType]
 */

object BuiltinEitherTests extends SimpleTestSuite {

  /*
   * TODO: remove all nulls
   */

  case class Qty(value: Int)

  case class InvalidQtyException(value: String) extends RuntimeException(s"not an int: $value")

  def toQty(value: String): Qty =
    if (value.matches("^[0-9]+$")) Qty(value.toInt)
    else throw InvalidQtyException(value)

  test("valid qty") {
    assertEquals(toQty("100"), Qty(100))
  }

  test("invalid qty") {
    assertEquals(toQty("asd"), null)
    assertEquals(toQty("1 0 0"), null)
    assertEquals(toQty(""), null)
    assertEquals(toQty("-10"), null)
  }
}