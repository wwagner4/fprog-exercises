package _5_strictnessAndLaziness

import org.scalatest.FunSuite

class Exercises extends FunSuite {

  trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty

    // Exercise 1: convert a stream to a list.
    def toList[A]: List[A] = {
      def _toList(s: Option[(A, Stream[A])]): List[A] = {
        if (s.isEmpty) Nil
        else s.get._1 :: _toList(s.get._2.uncons)
      }
      if (isEmpty) Nil
      else ???
    }
  }

  object Stream {

    def empty[A]: Stream[A] =
      new Stream[A] {
        def uncons = None
      }

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      new Stream[A] {
        lazy val uncons = Some((hd, tl))
      }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))

  }

  test("Exercise 1") {
    import Stream._

    val s: Stream[Int] = cons(1, cons(2, cons(3, empty)))
    val string = s.toList.mkString(",")
    assert("1, 2, 3" === string)
  }

}