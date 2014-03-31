package _5_strictnessAndLaziness

import org.scalatest.FunSuite

class Exercises extends FunSuite {
  
  case class Cons[+A](head: A, tail: Stream[A])

  trait Stream[+A] {
    def uncons: Option[Cons[A]]
    def isEmpty: Boolean = uncons.isEmpty

    // Exercise 1: convert a stream to a list.
    def toList: List[A] = {
      def tl(uc: Option[Cons[A]]): List[A] = {
        uc match {
          case None => Nil
          case Some(c) => c.head :: tl(c.tail.uncons)
        }
      }
      if (isEmpty) Nil
      else tl(uncons)
    }
    
    def take(n: Int): Stream[A] = {
      def tl(n: Int, uc: Stream[A]): Stream[A] = {
        if (n <= 0) Stream.empty
        else if (uc.isEmpty) Stream.empty
        else uc.uncons match {
          case None => Stream.empty
          case Some(c) => Stream.cons(c.head, tl(n -1, c.tail))
        }
      }
      if (isEmpty) Stream.empty
      else tl(n, this)
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
      assert(List(1,2,3) === l)
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

}