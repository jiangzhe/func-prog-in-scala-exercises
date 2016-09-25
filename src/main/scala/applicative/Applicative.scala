package applicative

import monad.Functor

/**
  * Applicative
  * Author: zhe.jiang
  * Desc:
  * Change log:
  * 2016/9/22 - created by zhe.jiang
  */
trait Applicative[F[_]] extends Functor[F] {
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.map(f).foldRight(unit(List.empty[B]))((fb, acc) => map2(fb, acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List.empty[A]))((fa, acc) => map2(fa, acc)(_ :: _))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    List.fill(n)(fa).foldRight(unit(List.empty[A]))((fa, acc) => map2(fa, acc)(_ :: _))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)((a2b, a) => a2b(a))

  def mapX[A, B](fa: F[A])(f: A => B): F[B] = apply(unit[A => B](f))(fa)

  def map2X[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
//    apply(apply(unit[A => (B => C)]((a: A) => { (b: B) => f(a,b) }))(fa))(fb)
    apply(apply(unit[A => B => C](f.curried))(fa))(fb)

  def map3X[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit[A => B => C => D](f.curried))(fa))(fb))(fc)

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map.empty[K,V]))((tuple, acc) =>
      map2(tuple._2, acc)((v, m) => m.updated(tuple._1, v))
    )
}

object Applicative {
  val streamApplicative = new Applicative[Stream] {

    override def map2[A, B, C](fa: Stream[A], fb: Stream[B])(f: (A, B) => C): Stream[C] =
      fa zip fb map f.tupled

    override def unit[A](a: => A): Stream[A] = Stream.continually(a)

//    override def sequence[A](a: List[Stream[A]]): Stream[List[A]] =
//      a.foldRight(Stream.empty[List[A]])((st, acc) =>
//        map2(st, acc)(_ :: _)
//      )
  }

  def validationApplicative[S] = new Applicative[({type F[X] = Validation[S, X]})#F] {
    def unit[A](a: => A): Validation[S, A] = Success(a)

    override def map2[A, B, C](fa: Validation[S, A], fb: Validation[S, B])(f: (A, B) => C): Validation[S, C] =
      fa match {
        case Success(a) =>
          fb match {
            case Success(b) => Success(f(a, b))
            case Failure(h1, t1)=> Failure(h1, t1)
          }
        case Failure(h1, t1) =>
          fb match {
            case Success(b) => Failure(h1, t1)
            case Failure(h2, t2) => Failure(h1, t1 ++ t2)
          }
      }
  }

  implicit class ApplicativeExt[F[_]](val F: Applicative[F]) {
    def product[G[_]](G: Applicative[G]): Applicative[({type H[X] = (F[X], G[X])})#H] =
      new Applicative[({type H[X] = (F[X], G[X])})#H] {
        override def map2[A, B, C](fa: (F[A], G[A]), fb: (F[B], G[B]))(f: (A, B) => C): (F[C], G[C]) = {
          (F.map2(fa._1, fb._1)(f), G.map2(fa._2, fb._2)(f))
        }

        override def unit[A](a: => A): (F[A], G[A]) = (F.unit(a), G.unit(a))
      }
  }

  implicit class ApplicativeExt2[F[_]](val F: Applicative[F]) {
    def compose[G[_]](G: Applicative[G]): Applicative[({type H[X] = F[G[X]]})#H] =
      new Applicative[({type H[X] = F[G[X]]})#H] {
        override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
          F.map2(fa, fb)((ga, gb) => G.map2(ga, gb)((a, b) => f(a,b)))

        override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
      }
  }
}



sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]


