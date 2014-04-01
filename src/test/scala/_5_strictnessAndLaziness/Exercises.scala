package _5_strictnessAndLaziness

import org.scalatest.FunSuite

class Exercises extends FunSuite {

  case class Cons[+A](head: A, tail: Stream[A])

  trait Stream[+A] {
    def uncons: Option[Cons[A]]
    def isEmpty: Boolean = uncons.isEmpty

    // Exercise 1: convert a stream to a list.
    def toList: List[A] = {
      def _toList(uc: Option[Cons[A]]): List[A] = {
        uc match {
          case None => Nil
          case Some(c) => c.head :: _toList(c.tail.uncons)
        }
      }
      if (isEmpty) Nil
      else _toList(uncons)
    }

    def take(n: Int): Stream[A] = {
      def _take(n: Int, uc: Stream[A]): Stream[A] = {
        if (n <= 0) Stream.empty
        else if (uc.isEmpty) Stream.empty
        else uc.uncons match {
          case None => Stream.empty
          case Some(c) => Stream.cons(c.head, _take(n - 1, c.tail))
        }
      }
      if (isEmpty) Stream.empty
      else _take(n, this)
    }
    def takeWhile(f: A => Boolean): Stream[A] = {
      def _takeWhile(uc: Stream[A]): Stream[A] = {
        if (uc.isEmpty) Stream.empty
        else uc.uncons match {
          case None => Stream.empty
          case Some(c) => {
            if (f(c.head)) Stream.cons(c.head, _takeWhile(c.tail))
            else Stream.empty
          }
        }
      }
      if (isEmpty) Stream.empty
      else _takeWhile(this)
    }
    // Implement forAll, which checks that all elements in the
    // match a given predicate. Your implementation should Stream terminate the
    // traversal as soon as it encounters a non-matching value.
    def forAll(p: A => Boolean): Boolean = {
      def _forAll(uc: Stream[A]): Boolean = {
        if (uc.isEmpty) true
        else uc.uncons match {
          case None => true
          case Some(c) => {
            if (p(c.head)) _forAll(c.tail)
            else false
          }
        }
      }
      if (isEmpty) true
      else _forAll(this)
    }
  }
  object Stream {

    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None
      }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some(Cons(hd, tl))
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

  }

  test("Exercise 1 toList") {
    import Stream._

    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.toList
      assert(List(1, 2, 3) === l)
    }
    {
      val s: Stream[Int] = cons(3, empty)
      val l = s.toList
      assert(List(3) === l)
    }
    {
      val s: Stream[Int] = empty
      val l = s.toList
      assert(Nil === l)
    }
  }

  test("Exercise 2 take") {
    import Stream._

    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.take(1).toList
      assert(List(1) === l)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.take(5).toList
      assert(List(1, 2, 3) === l)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.take(0).toList
      assert(Nil === l)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.take(-1).toList
      assert(Nil === l)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.take(-100).toList
      assert(Nil === l)
    }
  }

  test("Exercise 3 takeWhile") {
    import Stream._
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.takeWhile((i: Int) => i < 3).toList
      assert(List(1, 2) === l)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.takeWhile((i: Int) => i % 2 != 0).toList
      assert(List(1) === l)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.takeWhile((i: Int) => false).toList
      assert(Nil === l)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val l = s.takeWhile((i: Int) => true).toList
      assert(List(1, 2, 3) === l)
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
      assert(true === re)
    }
    {
      val s: Stream[Int] = empty
      val re = s.forAll((i: Int) => true)
      assert(true === re)
    }
    {
      val s: Stream[Int] = empty
      val re = s.forAll((i: Int) => false)
      assert(true === re)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val re = s.forAll((i: Int) => i > 1)
      assert(false === re)
    }
    {
      val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
      val re = s.forAll((i: Int) => true)
      assert(true === re)
    }
  }

}