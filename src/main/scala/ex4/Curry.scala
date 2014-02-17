package ex4

object Curry extends App {

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = ???
  
  
  def sum(a: Int, b: Int): Int = a + b
  
  val f = curry(sum)
  val g = f(5)
  val b = g(7)  
  
  println(b)
}