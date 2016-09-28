package iomonad

import monad.Mon

import scala.annotation.tailrec

/**
  * IO
  * Author: zhe.jiang
  * Desc:
  * Change log:
  * 2016/9/26 - created by zhe.jiang
  */
sealed trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
  def map[B](f: A => B): IO[B] = // flatMap(f andThen (Return(_)))
    flatMap(a => Return(f(a)))
}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO {
  @tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y.flatMap(a => g(a).flatMap(f)))
    }
  }
}