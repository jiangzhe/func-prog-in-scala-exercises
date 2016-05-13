package datastructures

/**
  * Tree
  * --
  * Desc:
  * Changes:
  * 2016/4/27 - Created by z_jiang.
  * --
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A,B](t: Tree[A])(leafF: A => B)(branchF: (Tree[A], Tree[A]) => B): B = t match {
    case Leaf(x) => leafF(x)
    case Branch(left, right) => branchF(left, right)
  }
}
