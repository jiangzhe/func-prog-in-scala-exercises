package state

/**
  * Rand
  * --
  * Desc:
  * Changes:
  * 2016/5/3 - Created by z_jiang.
  * --
  */
object Rand {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(Exercise.nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] = map(int)(i => Math.abs(i * 1.0) / Int.MaxValue)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = rng => {
    val (a, rngA) = ra(rng)
    val (b, rngB) = rb(rngA)
    (f(a, b), rngB)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((a, b) => (a, b))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List.empty[A]))(map2(_,_)((xa, xb) => xa :: xb))
  }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rngA) = f(rng)
      g(a)(rngA)
    }


  def nonNegativeInt: Rand[Int] = map(int)(Math.abs)

  def nonNegativeLessThan(n: Int): Rand[Int] = map(nonNegativeInt)(i => i % n)

  def nonNegativeLessThan2(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan2(n)(rng2)
  }

  def nonNegativeLessThan3(n: Int): Rand[Int] = flatMap(nonNegativeInt) {
    i =>
      val mod = i % n
      if (i + (n-1) - mod > 0) (mod, _)
      else nonNegativeLessThan3(n)
  }

  def mapF[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) {
    a => rng => (f(a), rng)
  }

  def mapF2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = flatMap(ra) {
    a =>
      flatMap(rb) {
        b =>
          rng => (f(a, b), rng)
      }
  }

//  def i2dr: Int => Rand[Double] = i => {
//    rng => (i * 1.0 / Int.MaxValue, rng)
//  }

//  def rollDie: Rand[Int] = nonNegativeLessThan(6)

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)


}
