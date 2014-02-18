package ex5

import org.scalatest.FunSuite

class Uncurry extends FunSuite {

  test("Uncurry") {

    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
      new Function1[A, Function1[B, C]] {
        def apply(a: A): Function1[B, C] = f(a, _)
      }
    }

    def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
      new Function2[A, B, C] {
        def apply(a: A, b: B): C = {
          val g: B => C = f(a)
          g(b)
        }
      }
    }

    def sum(a: Int, b: Int): Int = a + b

    val curried = curry(sum)

    assert(curried(3)(3) === 6)
    assert(curried(5)(3) === 8)

    val sum1 = uncurry(curried)
    
    assert(sum1(1,3) === 4)
    assert(sum1(1,5) === 6)
    
  }

}