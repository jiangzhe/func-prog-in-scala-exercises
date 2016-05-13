package state

/**
  * Exercise
  * --
  * Desc:
  * Changes:
  * 2016/5/3 - Created by z_jiang.
  * --
  */
object Exercise {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i == Int.MinValue) (0, r)
    else (Math.abs(i), r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = rng.nextInt
    (Math.abs(i) * 1.0 / Int.MaxValue, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (0 to count).foldRight((List.empty[Int], rng)) {
      case (i: Int, (ls: List[Int], r: RNG)) =>
        val (intElem, nextRNG) = r.nextInt
        (intElem :: ls, nextRNG)
    }
}
