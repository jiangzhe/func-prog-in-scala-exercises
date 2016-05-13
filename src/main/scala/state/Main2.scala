package state

/**
  * Main2
  * --
  * Desc:
  * Changes:
  * 2016/5/6 - Created by z_jiang.
  * --
  */
object Main2 {
  type Rand[A] = State[RNG, A]

  def unit[A](a: A): Rand[A] = State((a, _))

  val int: Rand[Int] = State(_.nextInt)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List.empty[A]))((ra, result) => ra.map2(result)(_ :: _))

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  val ns: Rand[List[Int]] = int.flatMap(x =>
    int.flatMap(y =>
      ints(x).map(xs =>
        xs.map(_ % y))))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s,s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def main(args: Array[String]): Unit = {
    for {
      x <- int
      y <- int
      xs <- ints(x)
    } yield xs.map(_ % y)
  }
}
