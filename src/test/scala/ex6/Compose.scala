package ex6

import org.scalatest.GivenWhenThen
import org.scalatest.FeatureSpec

class Compose extends FeatureSpec with GivenWhenThen {

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))

  feature("Concatenate") {
    scenario("Concatenate simple math functions") {

      Given("two math functions")
      def add5(a: Int): Int = a + 5
      def mul10(a: Int): Int = a * 10

      When("composing add5 and mul10")
      val f = compose(mul10, add5)

      Then("f(5) should result in 100")
      assert(f(5) === 100)
    }
  }

}