package Chapter3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(leftTree, rightTree) => size(leftTree) + size(rightTree) + 1
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(value) => value
      case Branch(leftTree, rightTree) => maximum(leftTree).max(maximum(rightTree))
    }

  def depth[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 0
      case Branch(leftTree, rightTree) => depth(leftTree).max(depth(rightTree)) + 1
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(leftTree, rightTree) => Branch(map(leftTree)(f), map(rightTree)(f))
    }

  private def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(leftTree, rightTree) => g(fold(leftTree)(f)(g), fold(rightTree)(f)(g))
  }

  def sizeFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((left, right) => left + right + 1)

  def maxFold(tree: Tree[Int]): Int =
    fold(tree)(a => a)((left, right) => left.max(right))

  def depthFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 0)((left, right) => left.max(right) + 1)

  def mapFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])((left, right) => Branch(left, right))
}