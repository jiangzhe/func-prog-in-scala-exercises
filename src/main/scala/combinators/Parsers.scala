package combinators

import combinators.MyParser.{Failure, Success, Result, Parser}

import scala.util.matching.Regex

/**
  * Parsers
  * Author: zhe.jiang
  * Desc:
  * Change log:
  * 2016/9/8 - created by zhe.jiang
  */
trait Parsers[Parser[+_]] {
  self =>
  // primitive
  def run[A](p: Parser[A])(input: String): Result[A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  // primitive
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n == 0) succeed(Nil) else map2(p, listOfN(n-1, p))((a, b) => a :: b)
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, p.many)(_ :: _).attempt or succeed(List.empty[A])
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
  def ignoreLeft[A, B](l: Parser[A], r: => Parser[B]): Parser[B] = map2(l, r)((ll, rr) => rr)
  def ignoreRight[A, B](l: Parser[A], r: => Parser[B]): Parser[A] = map2(l, r)((ll, rr) => ll)
  def opt[A](p: Parser[A]): Parser[Option[A]] = p.map(Option(_)).attempt | succeed(None)
  def rep[A, B](p: => Parser[A], s: Parser[B]): Parser[List[A]] = rep1(p, s).attempt | succeed(Nil)
  def rep1[A, B](p: => Parser[A], s: Parser[B]): Parser[List[A]] = map2(p, ignoreLeft(s, p).many)(_ :: _)
  def kv[K, S, V](k: => Parser[K], s: Parser[S], v: => Parser[V]): Parser[(K, V)] = product(ignoreRight(k, s), v)

  // primitive
  def fail[A](p: Parser[A]): Parser[A]
  // primitive
  def attempt[A](p: Parser[A]): Parser[A]


  // error handling
  def label[A](msg: String)(p: Parser[A]): Parser[A]

//  def errorLocation(e: ParseError): Location
//  def errorMessage(e: ParseError): String

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def ~>[B](p2: => Parser[B]): Parser[B] = self.ignoreLeft(p, p2)
    def <~[B](p2: => Parser[B]): Parser[A] = self.ignoreRight(p, p2)
    def attempt: Parser[A] = self.attempt(p)
    def ||[B>:A](p2: Parser[B]): Parser[B] = self.or(self.attempt(p), p2)
  }
}

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] = latest map (_._1)

  def latest: Option[(Location, String)] = stack.lastOption
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def currStr: String = input.substring(offset)

  def toError(msg: String): ParseError = ParseError(List((this, msg)))

  def advanceBy(n: Int): Location = copy(offset = offset + n)
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._

    object PImpl {
      val digit: Parser[JNumber] = regex("""(0|[1-9][0-9]*)(\.[0-9]+)?""".r).map(s => JNumber(s.toDouble))
      val word = regex("""[a-zA-Z0-9_]+""".r)
      val str: Parser[JString] = char('"') ~> regex("""[^"]*""".r) <~ char('"') map JString
      val nul: Parser[JNull.type] = string("null").map(_ => JNull)
      val bol: Parser[JBool] = (attempt(string("true")) | string("false")).map {
        case "true" => JBool(true)
        case "false" => JBool(false)
      }
      val whitespaces = regex("""[ \t\n]*""".r)
      val colonSep = opt(whitespaces) ~> string(":") <~ opt(whitespaces)
      val commaSep = opt(whitespaces) ~> string(",") <~ opt(whitespaces)
      val lbc = opt(whitespaces) ~> char('{') <~ opt(whitespaces)
      val rbc = opt(whitespaces) ~> char('}') <~ opt(whitespaces)
      val lbk = opt(whitespaces) ~> char('[') <~ opt(whitespaces)
      val rbk = opt(whitespaces) ~> char(']') <~ opt(whitespaces)
      val sq = string("'")
      val dq = char('"')


      val key: Parser[String] = dq ~> word <~ dq
      def array: Parser[JArray] = lbk ~> rep(body, commaSep) <~ rbk map (ls => JArray(ls.toVector))
      def obj: Parser[JObject] = lbc ~> rep(
        nullKV || numberKV || stringKV || boolKV || arrayKV || objKV, commaSep) <~ rbc map (ls => JObject(ls.toMap))

      def nullKV: Parser[(String, JNull.type)] = kv(key, colonSep, nul)
      def numberKV: Parser[(String, JNumber)] = kv(key, colonSep, digit)
      def stringKV: Parser[(String, JString)] = kv(key, colonSep, str)
      def boolKV: Parser[(String, JBool)] = kv(key, colonSep, bol)
      def arrayKV: Parser[(String, JArray)] = kv(key, colonSep, array)
      def objKV: Parser[(String, JObject)] = kv(key, colonSep, obj)

      def body: Parser[JSON] = nul || str || digit || bol || array || obj
//      def body: Parser[JSON] = lbc ~> rep(numberKV || nullKV,commaSep) <~ rbc map (ls => JArray(ls.map(_._2).toVector))
//      def body: Parser[JSON] = attempt(array)
//      def body: Parser[JSON] = attempt(digit) | attempt(array)

//      def body: Parser[JSON] = rep1(digit, commaSep) map (ls => JArray(ls.toVector))

//      def body: Parser[JSON] = digit

//      def body: Parser[JSON] = char('a').many map (ls => JNumber(ls.size))

//      def bodyForArray: Parser[JSON] = attempt(nul) | attempt(str) | attempt(digit) | attempt(bol) | attempt(obj) | array
//      def bodyForArray: Parser[JSON] = attempt(digit)
    }

    PImpl.body

  }
}


object MyParser {
  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }
  }

  case class Success[+A](get: A, charConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]
}


object MyParsers extends Parsers[Parser] {
  def string(s: String): Parser[String] =
    (loc: Location) =>
      if (loc.currStr.startsWith(s))
        Success(s, s.length)
      else
        Failure(loc.toError("Expected: " + s), true)

  def regex(r: Regex): Parser[String] =
    (loc: Location) => {
      val found = r.findPrefixMatchOf(loc.currStr)
      if (found.isDefined)
        Success(found.get.group(0), found.get.group(0).length)
      else
        Failure(loc.toError("Regex match error: " + r), true)
    }

  override def succeed[A](a: A): Parser[A] = (loc: Location) => Success(a, 0)

  def slice[A](p: Parser[A]): Parser[String] =
    s => p(s) match {
      case Success(a, n) => Success(s.input.substring(s.offset, n), n)
      case e @ Failure(_, _) => e
    }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s, msg))

  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  def fail[A](p: Parser[A]): Parser[A] =
    s => Failure(ParseError(List((s, "direct failure"))), true)

  def attempt[A](p: Parser[A]): Parser[A] = s => p(s).uncommit

  def or[A](x: Parser[A], y: => Parser[A]): Parser[A] =
    s => x(s) match {
      case Failure(e, false) => y(s)
      case r => r
    }

  def flatMap[A, B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    s => f(s) match {
      case Success(a, n) => g(a)(s.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
      case e @ Failure(_, _) => e
    }

  def run[A](p: Parser[A])(input: String): Result[A] = p(Location(input, 0))
}


object Run {
  def main(args: Array[String]): Unit = {
    def parse(json: String): Result[JSON] = MyParsers.run(JSON.jsonParser(MyParsers))(json) match {
      case e @ Failure(_, _) => e
      case r @ Success(a, n) =>
        if (n == json.length) r else Failure(ParseError(List((Location(json, n), "Extra characters unparsed"))), true)
    }
//    def parse(json: String): Result[JSON] = MyParsers.run(JSON.jsonParser(MyParsers))(json)
    println(parse("null"))
    println(parse("100"))
    println(parse("123.45"))
    println(parse("\"hello\""))
    println(parse("true"))
    println(parse("false"))
    println(parse("[1,2,3]"))
    println(parse("""["a","b","c"]"""))
    println(parse("""[true,false]"""))
    println(parse("""[null,"a",123.0]"""))
    println(parse("""{}"""))
    println(parse("""{"a":1}"""))
    println(parse("""{"a":{},"b":false}"""))
    println(parse("{\"a\":null,\"b\":null}"))
    println(parse("{}aabbcc"))
  }
}
