package par

import java.util.concurrent._

/**
  * Par
  * --
  * Desc:
  * Changes:
  * 2016/5/6 - Created by z_jiang.
  * --
  */
object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get))
  }

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

//  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List.empty[A]))(map2(_, _)((a,b) => a :: b))
//
//  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
//    val fbs: List[Par[B]] = ps.map(asyncF(f))
//    sequence(fbs)
//  }
//
//  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = ???

  def fork[A](a: => Par[A]): Par[A] = es => {
    es.submit(new Callable[A] {
      def call = a(es).get
    })
  }

  def run[A](es: ExecutorService)(f: Par[A]): A = f(es).get

  def map2timeout[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C)(timeout: Long, units: TimeUnit): Par[C] = (es: ExecutorService) => {
    val af = a(es)
    val bf = b(es)

    val t0 = System.currentTimeMillis()
    val av = af.get(timeout, units)
    val usedTime = System.currentTimeMillis() - t0
    val total = TimeUnit.MILLISECONDS.convert(timeout, units)
    if (total < usedTime) throw new TimeoutException()
    else {
      val bv = bf.get(total - usedTime, TimeUnit.MILLISECONDS)
      UnitFuture(f(av, bv))
    }
  }

  def asyncF[A,B](f: A => B): A => Par[B] = (a: A) => {
    fork(unit(f(a)))
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List.empty[A])) {
    (a, b) => map2(a, b)(_ :: _)
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as:List[A])(f: A => Boolean): Par[List[A]] = {
    val fs = as.map(asyncF(a => (f(a), a)))
    map(sequence(fs))(ls =>
      ls.foldRight(List.empty[A])((a, b) => if (a._1) a._2 :: b else b)
    )
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es =>
    if (run(es)(cond)) t(es) else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
    val idx = run(es)(n)
    choices(idx)(es)
  }

  def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es  => {
    val k = run(es)(key)
    choices(k)(es)
  }

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
    val a = run(es)(pa)
    choices(a)(es)
  }

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond)(if (_) t else f)

  def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(n)(choices)

  def join[A](a: Par[Par[A]]): Par[A] = es => {
    val pa = run(es)(a)
    pa(es)
  }

  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

}
