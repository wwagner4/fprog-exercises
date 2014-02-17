package ex1

object Fibo extends App {

  (0 to 18).foreach(v => println(f"$v%5d => ${fibo(v)}%5d ${fibotr(v)}%5d"))

  def fibo(v: Int): Int = {
    v match {
      case 0 => 0
      case 1 => 1
      case x => fibo(x - 1) + fibo(x - 2)
    }
  }

  def fibotr(v: Int): Int = {
    @annotation.tailrec
    def ftr(v: Int, acc1: Int, acc2: Int): Int = {
      v match {
        case 0 => acc2
        case x => ftr(x - 1, acc2, acc1 + acc2)
      }
    }
    ftr(v, 1, 0)
  }

}