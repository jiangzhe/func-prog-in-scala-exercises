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
    apply(apply(unit[A => (B => C)]((a: A) => { (b: B) => f(a,b) }))(fa))(fb)
}
