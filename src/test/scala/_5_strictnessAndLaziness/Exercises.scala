package _5_strictnessAndLaziness

import org.scalatest.FunSuite
import org.scalatest.Matchers._

import fprog._

class Exercises extends FunSuite {

  // Chattering operators 
  implicit class TestInt(i: Int) {
    def /#(b: Int) = { println(s"# $i/$b"); i / b }
    def +#(b: Int) = { println(s"# $i+$b"); i + b }
  }
  implicit class TestDouble(i: Double) {
    def /#(b: Double) = { println(s"# $i/$b"); i / b }
    def +#(b: Double) = { println(s"# $i+$b"); i + b }
  }

  test("Exercise 1 toList") {
    import Stream._

    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.toList
      l should be (List(1, 2, 3))
    }
    {
      val s: Stream[Int] = cons(3, empty)
      val l = s.toList
      l should be (List(3))
    }
    {
      val s: Stream[Int] = empty
      val l = s.toList
      l should be (Nil)
    }
  }

  test("Exercise 2 take") {
    import Stream._

    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.take(1).toList
      l should be (List(1))
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.take(5).toList
      l should be (List(1, 2, 3))
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.take(0).toList
      l should be (Nil)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.take(-1).toList
      l should be (Nil)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.take(-100).toList
      l should be (Nil)
    }
  }

  test("Exercise 3 takeWhile") {
    import Stream._
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.takeWhile((i: Int) => i < 3).toList
      l should be (List(1, 2))
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.takeWhile((i: Int) => i % 2 != 0).toList
      l should be (List(1))
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.takeWhile((i: Int) => false).toList
      l should be(Nil)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.takeWhile((i: Int) => true).toList
      l should be(List(1, 2, 3))
    }
  }

  // Implement forAll, which checks that all elements in the
  // match a given predicate. Your implementation should Stream terminate the
  // traversal as soon as it encounters a non-matching value.
  test("Exercise 4 forAll") {
    import Stream._
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val re = s.forAll((i: Int) => i > 0)
      re should be(true)
    }
    {
      val s: Stream[Int] = empty
      val re = s.forAll((i: Int) => true)
      re should be(true)
    }
    {
      val s: Stream[Int] = empty
      val re = s.forAll((i: Int) => false)
      re should be(true)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val re = s.forAll((i: Int) => i > 1)
      re should be(false)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val re = s.forAll((i: Int) => true)
      re should be(true)
    }
  }

  test("foldRight") {
    import Stream._
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val re = s.foldRight(0)((a, b) => a + b)
      re should be(6)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val re = s.foldRight(0)(_ +# _)
      re should be(6)
    }
    {
      val s: Stream[Double] = cons(2.0, cons(3.0, empty))
      val re = s.foldRight(1.0)((a, b) => b / a)
      re should be(0.166666 +- 0.01)
    }
  }
  
  test("Exercise 5 forAll") {
    import Stream._
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val re = s.forAll((i: Int) => i > 0)
      assert(true === re)
    }
  }
}