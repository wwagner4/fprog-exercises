package ex5

import org.scalatest.FunSuite

class Uncurry extends FunSuite {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    new Function1[A, Function1[B, C]] {
      def apply(a: A): Function1[B, C] = f(a, _)
    }
  }

  
  // Three versions for uncurry
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    new Function2[A, B, C] {
      def apply(a: A, b: B): C = {
        val g: B => C = f(a)
        g(b)
      }
    }
  }

  def uncurry1[A, B, C](f: A => (B => C)): (A, B) => C = {
    (a: A, b: B) =>
      {
        val g: B => C = f(a)
        g(b)
      }
  }

  def uncurry2[A, B, C](f: A => (B => C)): (A, B) => C = (a, b) => f(a)(b)

  def sum(a: Int, b: Int): Int = a + b

  case class FunDesc[A, B, C](desc: String, f: (A => (B => C)) => (A, B) => C)

  val funDescs = List(
    FunDesc[Int, Int, Int]("A", uncurry),
    FunDesc[Int, Int, Int]("B", uncurry1),
    FunDesc[Int, Int, Int]("C", uncurry2))

  funDescs.foreach(fd => {
    test(s"Uncurry ${fd.desc}") {

      val curried = curry(sum)

      assert(curried(3)(3) === 6)
      assert(curried(5)(3) === 8)

      val sum1 = fd.f(curried)

      assert(sum1(1, 3) === 4)
      assert(sum1(1, 5) === 6)
    }
  })
}