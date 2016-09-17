package monoid

/**
  * Foldable
  * Author: zhe.jiang
  * Desc:
  * Change log:
  * 2016/9/17 - created by zhe.jiang
  */
trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def toList[A](fa: F[A]): List[A] = foldRight(fa)(List.empty[A])(_ :: _)
}

object Impl {
  val listFoldable: Foldable[List] = new Foldable[List] {

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

    override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B = as.map(f).foldLeft(mb.zero)(mb.op)

    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  }

  val treeFoldable: Foldable[Tree] = new Foldable[Tree] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B =
      as match {
        case Leaf(value) => f(value, z)
        case Branch(left, right) =>
          val rightValue = foldRight(right)(z)(f)
          foldRight(left)(rightValue)(f)
      }

    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(value) => f(z, value)
        case Branch(left, right) =>
          val leftValue = foldLeft(left)(z)(f)
          foldLeft(right)(leftValue)(f)
      }

    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
  }

  val optionFoldable: Foldable[Option] = new Foldable[Option] {
    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
      case None => z
      case Some(a) => f(a, z)
    }

    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
      case None => z
      case Some(a) => f(z, a)
    }

    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B =
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
