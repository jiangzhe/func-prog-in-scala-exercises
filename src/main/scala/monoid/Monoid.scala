package monoid

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

}

object Run {
  def main(args: Array[String]): Unit = {
    println(Monoid.foldLeft(List(1,2,3), 0)(_ + _))
  }
}