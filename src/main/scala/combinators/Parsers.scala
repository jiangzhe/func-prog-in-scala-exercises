package combinators

import scala.util.matching.Regex

/**
  * Parsers
  * Author: zhe.jiang
  * Desc:
  * Change log:
  * 2016/9/8 - created by zhe.jiang
  */
trait Parsers[ParseError, Parser[+_]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n == 0) succeed(Nil) else map2(p, listOfN(n-1, p))((a, b) => a :: b)
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List.empty[A])
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(ar => succeed(f(ar)))
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  def slice[A](p: Parser[A]): Parser[String]
  def many1[A](p: Parser[A]): Parser[List[A]]
//  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]
//  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2).map(x => f(x._1, x._2))
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = flatMap(p)(a => map(p2)(b => (a, b)))
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = flatMap(p)(a => map(p2)(b => f(a, b)))

//  def manyE[A](p: Parser[A]): Parser[List[A]] = map2(p, manyE(p))(_ :: _) or succeed(List.empty[A])
//  def listOfNE[A](n: Int, p: Parser[A]): Parser[List[A]] =
//    if (n == 0) succeed(Nil) else map2(p, listOfNE(n-1, p))((a, b) => a :: b)

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]
  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
  }
}
