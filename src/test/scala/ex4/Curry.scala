package ex4

import scala.Function2
import org.scalatest.FunSuite

class Curry extends FunSuite {

  test("Curry") {

    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
      new Function1[A, Function1[B, C]] {
        def apply(a: A): Function1[B, C] = f(a, _)
      }
    }

    def sum(a: Int, b: Int): Int = a + b

    val f = curry(sum)
    val g = f(5)
    val b = g(7)

    assert(b == 12)

  }

}