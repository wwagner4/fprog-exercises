package candydisp

import org.scalatest.FunSuite

class CandyDispSuite extends FunSuite {

  trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def simulateMachine(m: Machine, is: List[Input]): Int = {
    is match {
      case Nil => m.coins
      case i :: rest => simulateMachine(act(m, i), rest)
    }
  }

  def act(m: Machine, i: Input): Machine = {
    if (m.candies <= 0) m
    else if (m.locked) {
      i match {
        case Coin => Machine(false, m.candies, m.coins + 1);
        case Turn => m
      }
    } else {
      i match {
        case Coin => m
        case Turn => Machine(true, m.candies - 1, m.coins);
      }
    }
  }

  test("turn the knob several times on a locked machine will do nothing") {
    val is = List[Input](Turn, Turn, Turn, Turn, Turn, Turn, Turn)
    assert(simulateMachine(Machine(true, 10, 8), is) == 8)
  }

  test("inserting coins several times on a locked machine will increase the number of coins") {
    val is = List[Input](Coin, Coin, Coin, Coin, Coin)
    assert(simulateMachine(Machine(true, 10, 8), is) == 9)
  }

  test("inserting coins and turning some n times will increase the amount for n") {
    val is = (1 to 5).toList.flatMap(_ => List(Coin, Turn))
    assert(simulateMachine(Machine(true, 10, 8), is) == 8 + 5)
  }

  test("insert coin in locked nonempty machine will unlock it") {
    val m = Machine(true, 10, 0)
    val Machine(locked, candies, coins) = act(m, Coin)
    assert(locked == false)
    assert(candies == 10)
    assert(coins == 1)
  }
  test("insert coin in locked empty machine will do nothing") {
    val m = Machine(true, 0, 0)
    val Machine(locked, candies, coins) = act(m, Coin)
    assert(locked == true)
    assert(candies == 0)
    assert(coins == 0)
  }
  test("turn lock on an unlocked machine dispenses candy and become locked") {
    val m = Machine(false, 10, 2)
    val Machine(locked, candies, coins) = act(m, Turn)
    assert(locked == true)
    assert(candies == 9)
    assert(coins == 2)
  }
  test("turn lock on an locked will do nothing") {
    val m = Machine(true, 10, 2)
    val Machine(locked, candies, coins) = act(m, Turn)
    assert(locked == true)
    assert(candies == 10)
    assert(coins == 2)
  }
  test("insert coin an locked will do nothing") {
    val m = Machine(true, 10, 2)
    val Machine(locked, candies, coins) = act(m, Turn)
    assert(locked == true)
    assert(candies == 10)
    assert(coins == 2)
  }
  test("insert coin an empty machine will do nothing") {
    val m = Machine(true, 0, 2)
    val Machine(locked, candies, coins) = act(m, Coin)
    assert(locked == true)
    assert(candies == 0)
    assert(coins == 2)
  }
  test("turning the knob on an empty machine will do nothing") {
    val m = Machine(true, 0, 2)
    val Machine(locked, candies, coins) = act(m, Turn)
    assert(locked == true)
    assert(candies == 0)
    assert(coins == 2)
  }

}