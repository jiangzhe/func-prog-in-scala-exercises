package lazyeval

/**
  * Main
  * --
  * Desc:
  * Changes:
  * 2016/4/28 - Created by z_jiang.
  * --
  */
object Main {
  def main(args: Array[String]): Unit = {
    val s = Stream(1,2,3,4,5)

    val list = s.take(3).toList
    println(list)

    println(s.forAll(_ < 6))

    println(s.takeWhile2(_ <= 3).toList)

    println(s.headOption2)

    println(s.map(_ * 2).toList)

    println(s.filter(n => (n & 1) == 1).toList)

    println(s.append(10).toList)

    println(s.flatMap(gen).toList)
  }


  def gen(n: Int): Stream[Int] = Stream(n, n+1, n+2)

}
