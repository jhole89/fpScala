package Chapter3

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => sys.error("Tail of empty list")
      case Cons(_, t) => t
    }

  def setHead[A](as: List[A], newVal: A): List[A] =
    as match {
      case Nil => sys.error("setHead called on empty List")
      case Cons(_, t) => Cons(newVal, t)
    }

  def drop[A](as: List[A], n: Int): List[A] =
    if (n == 0) as
    else as match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => as
    }

  def init[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  @tailrec
  def foldLeft[A, B](as: List[A], terminalValue: B)(f: (B, A) => B): B =
    as match {
      case Nil => terminalValue
      case Cons(h, t) => foldLeft(t, f(terminalValue, h))(f)
    }

  def sumFold(as: List[Int]): Int =
    foldLeft(as, 0)((x, y) => x + y)

  def prodFold(as: List[Double]): Double =
    foldLeft(as, 1.0)((x, y) => x * y)

  def lengthFold[A](as: List[A]): Int =
    foldLeft(as, terminalValue = 0)((acc, _) => acc + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

  def foldRightViaLeft[A, B](as: List[A], terminalValue: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), terminalValue)((b: B, a: A) => f(a,b))

  def concat[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil:List[A])(append)

  def addOne(as: List[Int]): List[Int] =
    foldRight(as, Nil: List[Int])((h,t) => Cons(h + 1, t))

  def doubleToString(as: List[Double]): List[String] =
    foldRight(as, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def removeOdds(integers: List[Int]): List[Int] =
    filter(integers)(x => x % 2 == 0)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def flatFilter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def addPairwise(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  }

