package _6_functionalState

import org.scalatest.FunSuite

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 982792L + 9238479L) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

}

class Exercises extends FunSuite {
  
  test("RNG") {
    val x1 = RNG.simple(92387923L)
    val (v1, x2) = x1.nextInt
    println(v1)
    val (v2, x3) = x2.nextInt
    println(v2)
  }

}