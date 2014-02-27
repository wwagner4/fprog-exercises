package ex2

import scala.annotation.tailrec

object Sorted extends App {

  val l1 = List(1, 2, 3, 4, 5)
  val l2 = List(1, 4, 3, 2, 5)
  val l2a = List(1, 4, 4, 5, 6)
  val l3 = List("a", "b", "c", "d")
  val l4 = List("a", "c", "x", "d")

  println(s"l1 ${isSorted(l1, compInt)}")
  println(s"l2 ${isSorted(l2, compInt)}")
  println(s"l2a ${isSorted(l2a, compInt)}")
  println(s"l3 ${isSorted(l3, compString)}")
  println(s"l4 ${isSorted(l4, compString)}")
  
  def compString(a: String, b: String): Boolean = a < b
  def compInt(a: Int, b: Int): Boolean = a < b

  @tailrec
  def isSorted[T](l: List[T], f: (T, T) => Boolean): Boolean = {
    l match {
      case Nil => true
      case a :: Nil => true
      case a :: b :: rest => {
        if (f(a, b)) isSorted(b :: rest, f)
        else false
      }
    }
  }

}