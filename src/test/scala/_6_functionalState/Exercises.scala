package _6_functionalState

import org.scalatest.FunSuite

class Exercises_6 extends FunSuite {

  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng =>
      {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }
  }
  trait RNG {
    def nextInt: (Int, RNG)
    val int: Rand[Int] = _.nextInt
    def unit[A](a: A): Rand[A] = rng => (a, rng)
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt = {
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
      }
    }

  }

  test("RNG") {
    val rng1 = RNG.simple(92387923L)
    val (v1, rng2) = rng1.nextInt
    println(v1)
    val (v2, _) = rng2.nextInt
    println(v2)
  }

  // EXERCISE 1: Write a function to generate a random positive integer. Note:
  // you can use x.abs to take the absolute value of an Int, x. Make sure to handle
  // the corner case Int.MinValue, which doesn't have a positive counterpart.

  def posInt(rng: RNG): (Int, RNG) = {
    val r1 = rng.nextInt
    val r = r1._1 match {
      case Int.MinValue => Int.MaxValue
      case x => math.abs(x)
    }
    (r, r1._2)
  }

  test(s"Ex 1") {
    var rng1: RNG = RNG.simple(92387923L)
    (1 to 100) foreach (i => {
      val (v1, rng2) = posInt(rng1)
      println(v1)
      assert(v1 >= 0, s"$v1 < 0")
      rng1 = rng2
    })
  }

  // EXERCISE 2: Write a function to generate a Double between 0 and 1, not
  // including 1. Note: you can use Int.MaxValue to obtain the maximum positive
  // integer value and you can use x.toDouble to convert an Int, x, to a Double.
  def double(rng: RNG): (Double, RNG) = {
    val r1 = rng.nextInt
    val r = r1._1 match {
      case Int.MinValue => Int.MaxValue
      case x => math.abs(x)
    }
    (r.toDouble / Int.MaxValue, r1._2)
  }

  test(s"Ex 2") {
    var rng1: RNG = RNG.simple(92387923L)
    (1 to 100) foreach (i => {
      val (v1, rng2) = double(rng1)
      println(v1)
      assert(v1 >= 0.0 && v1 <= 1.0, s"$v1 illegal value")
      rng1 = rng2
    })
  }

  // EXERCISE 4: Write a function to generate a list of random integers
  def ints(cnt: Int)(rng: RNG): (List[Int], RNG) = {
    def _ints(c: Int, rng: RNG): (List[Int], RNG) = {
      if (c <= 0) (Nil, rng)
      else {
        val (rv, rng1) = rng.nextInt
        val (lv, rng2) = _ints(c - 1, rng1)
        (rv :: lv, rng1)
      }
    }
    _ints(cnt, rng)
  }

  test(s"Ex 4") {
    var rng1: RNG = RNG.simple(92387923L)
    (1 to 100) foreach (i => {
      val (v1, rng2) = ints(10)(rng1)
      println(v1)
      assert(v1.toSet.size == 10)
      rng1 = rng2
    })
  }
  
  test("RNG int") {
    val rng0 = RNG.simple(203498L)
    val x = rng0.int(rng0)._1
    println(s"- RNG int: $x")
  }

  // EXERCISE 5: Use map to generate an Int between 0 and n, inclusive:
  def positiveMax(n: Int): Rand[Int] = {
    val rng0 = RNG.simple(203498L)
    val rng1 = RNG.simple(203498L) 
    //rng1.int(rng0)(i: Int => i % n)
    null // TODO Continue here
  }
  test(s"Ex 5") {
    val rng0 = RNG.simple(92387923L)
    val (x, rng1) = positiveMax(10)(rng0)
  }

}    
    

