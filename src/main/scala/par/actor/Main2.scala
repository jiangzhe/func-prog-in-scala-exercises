package par.actor

import java.util.concurrent.Executors

/**
  * Main2
  * --
  * Desc:
  * Changes:
  * 2016/5/13 - Created by z_jiang.
  * --
  */
object Main2 {
  def main(args: Array[String]): Unit = {
    val es = Executors.newFixedThreadPool(4)

    val f1 = Future.unit(1)
    println(Future.run(es)(f1))

    val f2 = Future.unit(2)

    val f3 = Future.map2(f1, f2)(_ + _)
    println(Future.run(es)(f3))
  }
}
