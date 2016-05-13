package par

/**
  * Main
  * --
  * Desc:
  * Changes:
  * 2016/5/6 - Created by z_jiang.
  * --
  */
object Main {
  private var cnt = 0
  def run1[A](a: => A): A = {
//    val af = () => a
    cnt += 1
    println("before run af " + cnt)
    val z = a
    println("after run af " + cnt)
    z
  }

  def run0[A](a: A): A = {
    cnt += 1
    println("run0 " + cnt)
    a
  }


  def main(args: Array[String]): Unit = {
    run1(
      run0({
        println("hello")
        100
      })
    )
  }




}
