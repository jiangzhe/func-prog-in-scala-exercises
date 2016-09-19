package monad

import combinators.MyParser.Parser
import combinators.MyParsers
import par.Par
import par.Par.Par
import par.Par.Par

/**
  * Mon
  * Author: zhe.jiang
  * Desc:
  * Change log:
  * 2016/9/19 - created by zhe.jiang
  */
trait Mon[F[_]] extends Functor[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def unit[A](a: A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))


}

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Monad {
  val parMonad: Mon[Par] = new Mon[Par] {

    override def flatMap[A, B](fa: Par[A])(f: (A) => Par[B]): Par[B] = Par.flatMap(fa)(f)

    override def unit[A](a: A): Par[A] = Par.unit(a)
  }

  val parserMonad: Mon[Parser] = new Mon[Parser] {

    override def flatMap[A, B](fa: Parser[A])(f: (A) => Parser[B]): Parser[B] =
      MyParsers.flatMap(fa)(f)

    override def unit[A](a: A): Parser[A] = MyParsers.succeed(a)
  }

  val optionMonad: Mon[Option] = new Mon[Option] {

    override def flatMap[A, B](fa: Option[A])(f: (A) => Option[B]): Option[B] = fa.flatMap(f)

    override def unit[A](a: A): Option[A] = Option(a)
  }

  val streamMonad: Mon[Stream] = new Mon[Stream] {

    override def flatMap[A, B](fa: Stream[A])(f: (A) => Stream[B]): Stream[B] =
      fa.flatMap(f)

    override def unit[A](a: A): Stream[A] = Stream(a)
  }

  val listMonad: Mon[List] = new Mon[List] {

    override def flatMap[A, B](fa: List[A])(f: (A) => List[B]): List[B] = fa.flatMap(f)

    override def unit[A](a: A): List[A] = List(a)
  }
}