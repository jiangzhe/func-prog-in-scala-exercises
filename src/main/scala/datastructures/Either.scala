package datastructures

/**
  * Either
  * --
  * Desc:
  * Changes:
  * 2016/4/28 - Created by z_jiang.
  * --
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case left @ Left(_) => left
    case Right(value) => Right(f(value))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case left @ Left(_) => left
    case Right(value) => f(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case right @ Right(_) => right
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case left @ Left(_) => left
    case Right(av) =>
      b match {
        case left @ Left(_) => left
        case Right(bv) => Right(f(av, bv))
      }
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
