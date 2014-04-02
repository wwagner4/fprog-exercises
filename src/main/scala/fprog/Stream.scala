package fprog

object Stream {

  case class Cons[A](head: A, tail: Stream[A])

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

trait Stream[A] {
  import Stream._

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

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    uncons match {
      case Some(Cons(head, tail)) => f(head, tail.foldRight(z)(f))
      case None => z
    }
  }

  // EXERCISE 5: Use foldRight to implement takeWhile. This will
  // construct a stream incrementally, and only if the values in the result are demanded
  // by some other expression.
  def takeWhile1(f: A => Boolean): Stream[A] = {
    foldRight(Stream.empty[A])((a, uf) => {
      // println(s"a:$a uf:$uf") <-- Cannot explain this output
      if (f(a)) Stream.cons(a, uf.takeWhile(f))
      else uf.takeWhile(_ => false)
    })
  }

  // EXERCISE 6: Implement map, filter, append, and flatMap using
  // foldRight.
  def map[B](f: A => B): Stream[B] = ???
  
  def filter(f: A => Boolean): Stream[A] = ???
  
  def append(bs: Stream[A]): Stream[A] = ???

  def flatMap[B](f: A => Stream[B]): Stream[A] = ???
  
}
  
