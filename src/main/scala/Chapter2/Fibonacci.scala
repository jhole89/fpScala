package Chapter2

import scala.annotation.tailrec

object Fibonacci extends App {

  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, previous: Int, current: Int): Int = {
      if (n <= 0) previous
      else go(n-1, current, current + previous)
    }
    go(n, 0, 1)
  }

  private def formatFib(n: Int): String =
    "The Fibonacci value at %d is %d".format(n, fib(n))

  println(formatFib(args(0).toInt))

}
