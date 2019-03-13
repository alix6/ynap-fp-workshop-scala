package exercises
import exercises.Concrete.BoxInt

object Concrete {
  case class BoxInt(value: Int)
  case class BoxString(value: String)
  case class BoxDouble(value: Double)

  trait Deserializer[A]{
    def deserialize(input:String):A
    def serialize(input: A): String
  }

  implicit val s2bi = new Deserializer[BoxInt] {
    def deserialize(input: String): BoxInt =
      BoxInt(input.toInt)

    def serialize(input: BoxInt): String =
      input.value.toString
  }

  trait Domain [A] {
    def logic(input: A):A
  }

  implicit val dni = Domain[BoxInt]{
    def logic (input:BoxInt): BoxInt=
      BoxInt(neutralize(input.value))
  }
  def execute[A](input: String)(implicit d:Deserializer[A], l:Domain[A]): String = {
    val v = d.deserialize(input)
    val r= l.logic(v)
    d.deserialize(r)
  }


  def neutralize(x: Int): Int = 0
  def double(x: Int): Int = x*x

  trait Printer[A]{
    def print(a:A):String
  }

  implicit val printForBoxInt = new Printer[BoxInt]{
    def print(a:BoxInt):String = a.value.toString()
  }

  implicit val printForBoxString = (x:String) => x

  def print[A](a:A)(implicit s:Printer[A]):String=
    s.print(a)

  trait Summable[A]{
    def plus (x:A, y:A):A
  }

}





object Main extends App {
  import Concrete._
  val r= execute[BoxInt]("42");
  println(r);
  println("Hello!")
}
