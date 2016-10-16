package mutate

/**
  * ST
  * Author: zhe.jiang
  * Desc:
  * Change log:
  * 2016/10/10 - created by zhe.jiang
  */
sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run (s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }

}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }

  def doWhile[S, A](st: ST[S, A])(cond: A => ST[S, Boolean], f: A => A): ST[S, A] =
    for {
      a <- st
      ok <- cond(a)
      newA <- if (ok) doWhile(ST.apply[S, A](f(a)))(cond, f) else st
    } yield newA


  def runST[A](st: RunnableST[A]): A = st.apply[Unit].run(())._1
}


sealed trait STRef[S, A] {
  protected var cell: A
  def read: ST[S, A] = ST(cell)
  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A]{
    var cell = a
  })
}

sealed trait STIf[S, A] {
  protected def pass: Boolean

}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S, Int] = ST(value.size)
  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      value(i) = a
      ((), s)
    }
  }
  def read(i: Int): ST[S, A] = ST(value(i))
  def freeze: ST[S, List[A]] = ST(value.toList)
  def fill(xs: Map[Int, A]): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S) = {
      for ((k, v) <- xs) value(k) = v
      ((), s)
    }
  }
//    for {
//      (k, v) <- xs
//      _ <- write(k, v)
//    } yield ()

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(i, y)
    _ <- write(j, x)
  } yield ()

}

object STArray {
  def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] = {
    val arr: STArray[S, A] = new STArray[S, A] {
      lazy val value = Array.fill(sz)(v)
    }
    ST(arr)
  }

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] = {
    ST(new STArray[S, A] {
      lazy val value = xs.toArray
    })
  }
}

//object Algo {
//  def partition[S](arr: STArray[S, Int], n: Int, r: Int, pivot: Int): ST[S, Int] = {
//    for {
//      pivotVal <- arr.read(pivot)
//      _ <- arr.swap(pivot, r)
//      jRef <- STRef(n)
//      (x, _) <- doUntil(ST(n), r, i => {
//        for {
//          v <- arr.read(i)
//          j <- jRef.read
//        } yield (i, if (v < pivotVal) swapAndInc(arr, i, jRef) else arr)
//      }, ST[Nothing, (Int, STArray[S, Int])]((n, arr)))
//    } yield x
//  }
//
//  def swapAndInc[S](arr: STArray[S, Int], i: Int, jRef: STRef[S, Int]): ST[S, STArray[S, Int]] =
//    for {
//      j <- jRef.read
//      _ <- arr.swap(i, j)
//      _ <- jRef.write(j + 1)
//    } yield arr
//
//  def doUntil[S,B](idx: ST[S, Int], max: Int, f: Int => ST[S, B], dft: => ST[S, B]): ST[S, B] =
//    for {
//      i <- idx
//      result <- if (i < max) doUntil(inc(idx), max, f, f(i)) else dft
//    } yield result
//
//  def inc[S](idx: ST[S, Int]): ST[S, Int] = for {
//    i <- idx
//  } yield i + 1
//
//  def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] =
//    if (n < r) for {
//      pi <- partition(a, n, r, n + (r - n) / 2)
//      _ <- qs(a, n, pi - 1)
//      _ <- qs(a, pi + 1, r)
//    } yield () else ST(())
//
//  def quicksort(xs: List[Int]): List[Int] =
//    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
//      override def apply[S]: ST[S, List[Int]] = for {
//        arr <- STArray.fromList(xs)
//        size <- arr.size
//        _ <- qs(arr, 0, size - 1)
//        sorted <- arr.freeze
//      } yield sorted
//    })
//
//}

//object Run {
//  def main(args: Array[String]): Unit = {
//    println(Algo.quicksort(List(2,5,4,1,3)))
//  }
//}
