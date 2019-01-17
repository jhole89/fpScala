package Chapter5

import annotation.tailrec

trait Stream[+A] {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case _ => List()
  }

  def toListTR: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(f: A => Boolean): Boolean =
    foldRight(true)((a, b) => f(a) && f(b))

  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (h, t) =>
        if (f(h)) cons(h, t)
        else empty
    )

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])( (h, _) => Some(h) )

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])( (h, t) => cons(f(h), t) )

  def filter[B](f: A => Boolean): Stream[A] =
    foldRight(empty[A])(
      (h, t) =>
        if(f(h)) cons(h, t)
        else t
    )

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)( (h, t) => cons(h, t) )

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])( (h, t) => f(h).append(t) )

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = cons(n, from(n+1))

  def fibs: Stream[Int] = {
    def go(initial: Int, accumulator: Int): Stream[Int] =
      cons(initial, go(accumulator, initial + accumulator))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }

  def fibsViaUnfold: Stream[Int] =
    unfold((0,1)){
      case (init, acc) =>
        Some((init,(acc, init + acc)))
    }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n+1)))

  def constViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a,a)))

  val ones: Stream[Int] = unfold(1)(_ => Some(1,1))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1) => Some((h(),(empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(thisHead, thisTail), Cons(otherHead, otherTail)) =>
        Some((f(thisHead(), otherHead()),(thisTail(), otherTail())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = ???

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
