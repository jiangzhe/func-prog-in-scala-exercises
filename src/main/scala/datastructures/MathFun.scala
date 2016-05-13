package datastructures

/**
  * MathFun
  * --
  * Desc:
  * Changes:
  * 2016/4/28 - Created by z_jiang.
  * --
  */
object MathFun {
  def variance(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else {
      val mean = xs.sum / xs.length
      val v = xs.map(elem => Math.pow(elem - mean, 2)).sum
      Some(v)
    }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift(Math.abs)

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = 100.0

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = a match {
    case None => None
    case Some(xa) =>
      b match {
        case None => None
        case Some(xb) => Some(f(xa, xb))
      }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = List.foldRight2(a, Some(Nil): Option[List[A]]) {
    (optA, listOpt) =>
      optA match {
        case None => None
        case Some(xa) => listOpt.map(ls => Cons(xa, ls))
      }
  }

  def parseInts(a: List[String]): Option[List[Int]] = sequence(List.map(a)(i => Try(i.toInt)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = List.foldRight2(a, Some(Nil): Option[List[B]]) {
    (elemA, listOpt) =>
      f(elemA) match {
        case None => None
        case Some(elemB) => listOpt.map(ls => Cons(elemB, ls))
      }
  }


  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = List.foldRight2(es, Right(Nil): Either[E, List[A]]) {
    (elem, holder) =>
      elem match {
        case left @ Left(_) => left
        case Right(value) => holder.map(ls => Cons(value, ls))
      }
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = List.foldRight2(as, Right(Nil): Either[E, List[B]]) {
    (elem, holder) =>
      f(elem) match {
        case left @ Left(_) => left
        case Right(value) =>
          holder.map(ls => Cons(value, ls))
      }
  }

}
