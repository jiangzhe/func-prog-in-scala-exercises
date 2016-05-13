package datastructures

/**
  * Main
  * --
  * Desc:
  * Changes:
  * 2016/4/27 - Created by z_jiang.
  * --
  */
object Main {
  def main(args: Array[String]): Unit = {
    val x = List.foldRight(List(1,2,3,4), Nil: List[Int])(Cons(_,_))
    println(x)

    val y = List.foldLeft(List(1,2,3,4), Nil: List[Int])(Cons(_,_))
    println(y)

    val len = List.len(List(1,2,3,4))
    println(len)

    val list1 = List(1,5,6,8)
    val list2 = List(2,3,4,7)

    val list3: List[Int] = Nil
    val list4 = List(100)

    println(List.concat(list1, list2))

    println(List.flatten(List(list1,list2,list3,list4)))

    println(List.convertString(List(1,2,3,4)))

    println(List.map(List(1,2,3,4))(_ * 2))

    println(List.filter(List(1,2,3,4))((x: Int) => (x & 1) == 1))

    println(List.flatMap(List(1,2,3,4))((x: Int) => List(x, x/2)))

    println(List.filter2(List(1,2,3,4))((x: Int) => (x & 1) == 1))

    println(List.parallel(List(1,3,5), List(2,4,6)))

    println(List.zipWith(List(1,3,5), List(2,4,6))(_ * _))

    println(List.hasSubsequence(List(1,2,3,4,5), List(3,4)))

    println(MathFun.sequence(List(Some(1),Some(2),Some(3))))

  }
}
