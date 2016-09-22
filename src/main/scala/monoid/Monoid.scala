package monoid

import par.Par
import par.Par.Par
import par.Par.Par

/**
  * Monoid
  * Author: zhe.jiang
  * Desc:
  * Change log:
  * 2016/9/13 - created by zhe.jiang
  */
trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero: Boolean = true
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: (A) => A, a2: (A) => A): (A) => A = (a: A) => a2(a1(a))

    override def zero: (A) => A = a => a
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.map(f).foldRight(m.zero)(m.op)

  def foldLeft[B](as: List[B], z: B)(f: (B, B) => B): B = {
    val m: Monoid[B] = new Monoid[B] {
      override def op(a1: B, a2: B): B = f(a2, f(a1, z))
      override def zero: B = z
    }
    foldMap(as, m)(b => b)
  }

  def foldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.length == 1) f(v(0))
    else {
      val (p1, p2) = v.splitAt(v.length / 2)
      val b1 = foldMap(p1, m)(f)
      val b2 = foldMap(p2, m)(f)
      m.op(b1, b2)
    }
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    val parMonoid = par(m)
    foldMap(v.map(Par.unit), parMonoid)(pa => Par.map(pa)(f))
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1(a), a2(a))
    override def zero: (A) => B = a => B.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val M: Monoid[Map[A, Int]] = mapMergeMonoid(intAddition)
    foldMap(as, M)((a: A) => Map((a, 1)))
  }

  val m = productMonoid(intAddition, intAddition)

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = a1 match {
      case Stub(ch1) =>
        a2 match {
          case Stub(ch2) => Stub(ch1 + ch2)
          case Part(lStub, words, rStub) => Part(ch1 + lStub, words, rStub)
        }
      case Part(l1, w1, r1) =>
        a2 match {
          case Stub(ch2) => Part(l1, w1, r1 + ch2)
          case Part(l2, w2, r2) =>
            val intermediateWords = if ((r1 + l2).length > 0) 1 else 0
            Part(l1, w1 + w2 + intermediateWords, r2)
        }
    }
    override def zero: WC = Stub("")
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {

    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))

    override def zero: (A, B) = (A.zero, B.zero)
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {

    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
      (a1.keySet ++ a2.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
      }

    override def zero: Map[K, V] = Map[K, V]()
  }


  def wc(str: String, splitNum: Int): Int = {
    val step = str.length / splitNum
    if (step == 0) 0 else {
      val ss = 0 until str.length by step
      val es = ss.drop(1) ++ List(str.length)
      val wcs = ss.zip(es).map(pos => str2wc(str.substring(pos._1, pos._2)))
      val total: WC = foldMap(wcs, wcMonoid)(x => x)
      total match {
        case Stub(chars) => if (chars.length > 0) 1 else 0
        case Part(lStub, words, rStub) =>
          val ls = if (lStub.length > 0) 1 else 0
          val rs = if (rStub.length > 0) 1 else 0
          words + ls + rs
      }
    }
  }

  def str2wc(str: String): WC = {
    val words = str.split("\\W+", -1)
    if (words.length == 1) Stub(words(0))
    else Part(words.head, words.length - 2, words.last)
  }

}

object Run {
  def main(args: Array[String]): Unit = {
//    println(Monoid.foldLeft(List(1,2,3), 0)(_ + _))
    println(Monoid.wc("", 5))
    println(Monoid.wc("hello", 5))
    println(Monoid.wc("hello, world", 5))
    println(Monoid.wc("this is a very long sentence from john to test the word count monoid function", 5))
  }
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

