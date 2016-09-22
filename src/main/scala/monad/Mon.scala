package monad

import combinators.MyParser.Parser
import combinators.MyParsers
import par.Par
import par.Par.Par
import state.State

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

  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List.empty[A]))((fa, b) => map2(fa, b)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(f))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    map(sequence(ms.map(a => map2(f(a), unit(a))((_, _)))))(ls => ls.filter(_._1).map(_._2))
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  def flatMapX[A, B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)(())

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(a => a)

  def composeX[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))

  def flatMapXX[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

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

  // monad提供了一个引入和绑定变量的上下文，同时执行了变量替换
  val idMonad: Mon[Id] = new Mon[Id] {

    override def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = f(fa.value)

    override def unit[A](a: A): Id[A] = Id(a)
  }

  // (A, S)
  type IntState[A] = State[Int, A]
  val intStateMonad: Mon[IntState] = new Mon[IntState] {
    override def flatMap[A, B](fa: IntState[A])(f: (A) => IntState[B]): IntState[B] =
      State({
        a =>
          val (a1, s1) = fa.run(a)
          f(a1).run(s1)
      })

    override def unit[A](a: A): IntState[A] = State((a, _))
  }

  object iStateMonad extends Mon[({type IntState[A] = State[Int, A]})#IntState] {
    override def unit[A](a: A): IntState[A] = State((a, _))
    override def flatMap[A, B](fa: IntState[A])(f: A => IntState[B]): IntState[B] = fa.flatMap(f)
  }

  def stateMonad[S] = new Mon[({type F[X] = State[S, X]})#F] {
    override def flatMap[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = fa.flatMap(f)

    override def unit[A](a: A): State[S, A] = State((a, _))

  }

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]()))((acc, a) => for {
      xs <- acc
      n <- acc.getState
      _ <- acc.setState(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse

}

case class Id[A](value: A)

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Mon[({type F[X] = Reader[R, X]})#F] {
    override def flatMap[A, B](fa: Reader[R, A])(f: (A) => Reader[R, B]): Reader[R, B] =
      Reader({
        r =>
          val a1 = fa.run(r)
          f(a1).run(r)
      })

    override def unit[A](a: A): Reader[R, A] = Reader(_ => a)
  }
}


object Run {
  def main(args: Array[String]): Unit = {
    val arr = Monad.zipWithIndex(List.fill(5)(1))
    println(arr)
  }
}