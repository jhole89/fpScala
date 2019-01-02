package Chapter2

import scala.annotation.tailrec

object Sorted extends App {

  def isSorted[A](as: Array[A], greaterThan: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (greaterThan(as(n), as(n + 1))) false
      else loop(n + 1)

    loop(0)

  }

  println(isSorted(Array(1,2,3,4,5), (x: Int, y: Int) => x > y))
  println(isSorted(Array("a", "b", "c"), (x: String, y:String) => x > y))
}
