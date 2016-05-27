package combinator

import scala.util.matching.Regex

/**
  * Parser
  * --
  * Desc:
  * Changes:
  * 2016/5/25 - Created by z_jiang.
  * --
  */
trait Parsers {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String] = ???


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def char2(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def succeed[A](a: A): Parser[A] = string("").map(_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2).map(t => f(t._1, t._2))

  def manyX[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _).or(succeed(List()))

  def listOfNX[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n == 0) succeed(List()) else map2(p, listOfNX(n-1, p))(_ :: _)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
}

trait ParseError

trait Parser[+A] {

}