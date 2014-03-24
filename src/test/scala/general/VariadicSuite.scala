package general

import org.scalatest.FunSuite

class VariadicSuite extends FunSuite {

  def f1(id: String, x: Int*): Unit = {
    println(s"$id a x=$x")
    x.foreach(a => println(s"$id b a=$a"))
  }

  test("Simple") {
    f1("A", 1, 2, 3)
  }

  test("From List") {
    val l = List(1, 3, 4)
    f1("B", l: _*)
  }

  test("From Seq") {
    val l = Seq(1, 3, 4)
    f1("B", l: _*)
  }

}