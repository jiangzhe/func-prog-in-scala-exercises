package datastructures

/**
  * List
  * --
  * Desc:
  * Changes:
  * 2016/4/26 - Created by z_jiang.
  * --
  */
sealed trait List[+T]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x+y)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def len[T](ns: List[T]): Int = foldRight(ns, 0) {
    (elem, n) => n + 1
  }

  @annotation.tailrec
  def foldLeft[A,B](ns: List[A], z: B)(f: (A,B) => B): B = ns match {
    case Nil => z
    case Cons(hd, tl) => foldLeft(tl, f(hd, z))(f)
  }


  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def len3[A](ns: List[A]): Int = foldLeft(ns, 0) {
    (elem, n) => n + 1
  }

  def reverse[A](ns: List[A]): List[A] = foldLeft(ns, Nil: List[A])(Cons(_,_))

  def foldRight2[A,B](as: List[A], z: B)(f: (A,B) => B): B = foldLeft(reverse(as),z)(f)

  def append[A](ns: List[A], elem: A): List[A] = reverse(foldLeft(ns, Cons(elem, Nil))(Cons(_,_)))

  def concat[A](ns: List[A], ms: List[A]): List[A] = reverse(foldLeft(ms, reverse(ns)){ (elem, ls) => Cons(elem, ls) })

  def flatten[A](nss: List[List[A]]): List[A] = foldLeft(nss, Nil: List[A]) {
    (next, prev) => concat(prev, next)
  }

  def addOne(ns: List[Int]): List[Int] = foldRight2(ns, Nil: List[Int]) {
    (elem, ls) => Cons(elem+1, ls)
  }

  def convertString(ns: List[Double]): List[String] = foldRight2(ns, Nil: List[String]) {
    (elem, ls) => Cons(elem.toString, ls)
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight2(as, Nil: List[B]) {
    (elem, ls) => Cons(f(elem), ls)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight2(as, Nil: List[A]) {
    (elem, ls) => if (f(elem)) Cons(elem, ls) else ls
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldLeft(as, Nil: List[B]) {
    (la, lb) => concat(lb, f(la))
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as) {
    elem => if (f(elem)) List(elem) else Nil
  }

  def parallel(as: List[Int], bs: List[Int]): List[Int] = as match {
    case Nil => Nil
    case Cons(ahd, atl) =>
      bs match {
        case Nil => Nil
        case Cons(bhd, btl) => Cons(ahd + bhd, parallel(atl, btl))
      }
  }

  def zipWith[A,B](as: List[A], bs: List[A])(f: (A,A) => B): List[B] = as match {
    case Nil => Nil
    case Cons(ahd, atl) =>
      bs match {
        case Nil => Nil
        case Cons(bhd, btl) =>
          Cons(f(ahd, bhd), zipWith(atl, btl)(f))
      }
  }

  def exists[A](as: List[A])(f: A => Boolean): Boolean = as match {
    case Nil => false
    case Cons(hd, tl) => f(hd) || exists(tl)(f)
  }


  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startWithSequence(sup: List[A], sub: List[A]): Boolean = sub match {
      case Nil => true
      case Cons(hd, tl) =>
        sup match {
          case Cons(phd, ptl) => phd == hd && startWithSequence(ptl, tl)
          case _ => false
        }
    }

    sub match {
      case Nil => true
      case Cons(hd, tl) =>
        sup match {
          case Cons(phd, ptl) =>
            if (phd == hd) startWithSequence(ptl, tl) || hasSubsequence(ptl, sub)
            else hasSubsequence(ptl, sub)
          case _ => false
        }
    }
  }
}