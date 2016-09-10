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
  // primitive
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  // primitive
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n == 0) succeed(Nil) else map2(p, listOfN(n-1, p))((a, b) => a :: b)
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List.empty[A])
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = flatMap(a)(ar => succeed(f(ar)))
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  // primitive
  def slice[A](p: Parser[A]): Parser[String]
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
  // primitive
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = flatMap(p)(a => map(p2)(b => (a, b)))
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = flatMap(p)(a => map(p2)(b => f(a, b)))
  // primitive
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  // primitive
  implicit def regex(r: Regex): Parser[String]
  def ignoreLeft[A, B](l: Parser[A], r: Parser[B]): Parser[B] = map2(l, r)((ll, rr) => rr)
  def ignoreRight[A, B](l: Parser[A], r: Parser[B]): Parser[A] = map2(l, r)((ll, rr) => ll)
  def opt[A](p: Parser[A]): Parser[Option[A]] = or(p.map(Option(_)), succeed(None))
  def rep[A, B](p: Parser[A], s: Parser[B]): Parser[List[A]] = or(rep1(p, s), succeed(Nil))
  def rep1[A, B](p: Parser[A], s: Parser[B]): Parser[List[A]] = map2(p, many(ignoreLeft(s, p)))(_ :: _)










  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def ~>[B](p2: Parser[B]): Parser[B] = ignoreLeft(p, p2)
    def <~[B](p2: Parser[B]): Parser[A] = ignoreRight(p, p2)
  }
}


trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

//  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
//    import P._
//
//    val spaces = char(' ').many.slice
//    val digit = regex("""(0|[1-9][0-9]*)(\.[0-9]+)?""".r)
//    val whitespaces = regex("""[ \t\n]+""".r)
//    val lq = string("{")
//    val rq = string("}")
//    val nullParse: Parser[JSON] =
//  }
}