package ex3

object Partial1 extends App {

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)

  def f5 = partial1(5, sum)
  def f6 = partial1(6, sum)

  def sum(a: Int, b: Int): Int = a + b
  def add5(a: Int): Int = f5(a)
  def add6(a: Int): Int = partial1(6, sum)(a)
  
  performance("f5", f5)
  performance("f6", f6)
  performance("add5", add5)
  performance("add6", add6)

  def print: Unit = {
    (0 to 6).foreach(v => {
      val r5 = f5(v)
      val r6 = add6(v)
      val line = "%5d %5d %5d" format (v, r5, r6)
      println(line)
    })
  }

  def performance(name: String, f: Int => Int): Unit = {
    var cnt = 0L
    val start = System.currentTimeMillis
    while (cnt < 10000000L) {
      f(0)
      cnt += 1
    }
    val dur = System.currentTimeMillis - start
    println(s"$name $dur")
  }
}

