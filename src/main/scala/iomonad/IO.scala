package iomonad

import monad.Mon
import par.Par
import par.Par.Par
import par.Par.Par

import scala.annotation.tailrec

/**
  * IO
  * Author: zhe.jiang
  * Desc:
  * Change log:
  * 2016/9/26 - created by zhe.jiang
  */
sealed trait IO[A] {
  import IO._
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
  def map[B](f: A => B): IO[B] = // flatMap(f andThen (Return(_)))
    flatMap(a => Return(f(a)))
}


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
  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]
}

//
//sealed trait TailRec[A] {
//  import TailRec._
//  def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
//  def map[B](f: A => B): TailRec[B] = flatMap(f andThen (Return(_)))
//}
//
//object TailRec {
//  case class Return[A](a: A) extends TailRec[A]
//  case class Suspend[A](resume: () => A) extends TailRec[A]
//  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
//}
//
//sealed trait Async[A] {
//  import Async._
//  def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)
//  def map[B](f: A => B): Async[B] = flatMap(f andThen (Return(_)))
//}
//
//object Async {
//  case class Return[A](a: A) extends Async[A]
//  case class Suspend[A](resume: Par[A]) extends Async[A]
//  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]
//
//  @tailrec
//  def step[A](async: Async[A]): Async[A] = async match {
//    case FlatMap(FlatMap(x, f), g) => step(x flatMap (a => f(a) flatMap g))
//    case FlatMap(Return(x), f) => step(f(x))
//    case _ => async
//  }
//
//  def run[A](async: Async[A]): Par[A] = step(async) match {
//    case Return(a) => Par.unit(a)
//    case Suspend(r) => Par.flatMap(r)(a => run(a))
//    case FlatMap(x, f) => x match {
//      case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
//      case _ => sys.error("Impossible; step eliminates these cases")
//    }
//  }
//}
