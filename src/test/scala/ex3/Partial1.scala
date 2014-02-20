package ex3

import org.scalatest.FunSuite

class Partial1 extends FunSuite {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)

  def f5 = partial1(5, sum)
  def f6 = partial1(6, sum)

  def sum(a: Int, b: Int): Int = a + b
  def add5(a: Int): Int = f5(a)
  def add6(a: Int): Int = partial1(6, sum)(a)

  test("performance") {
    performance("f5", f5)
    performance("f6", f6)
    performance("add5", add5)
    performance("add6", add6)
  }

  case class Cont(a: Int, b: Int, c: Int)
  val data = (-4 to 6) map (i => Cont(i, i + 5, i + 6))

  data foreach (c => {
    test(s"calc ${c.a}") {
      assert(f5(c.a) === c.b)
      assert(add6(c.a) === c.c)
    }
  })

  def performance(name: String, f: Int => Int): Unit = {
    var cnt = 0L
    val start = System.currentTimeMillis
    while (cnt < 100000000L) {
      f(0)
      cnt += 1
    }
    val dur = System.currentTimeMillis - start
    println(f"performance of $name%7s is $dur")
  }
}

