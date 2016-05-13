package state

/**
  * State
  * --
  * Desc:
  * Changes:
  * 2016/5/4 - Created by z_jiang.
  * --
  */
object State {
  def unit[S, A](a: A): State[S, A] = State((a, _))

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = State({
    s =>
      ss.foldRight((List.empty[A], s)) {
        (state, tuple) =>
          val (nextValue, nextState) = state.run(tuple._2)
          (nextValue :: tuple._1, nextState)
      }
  })

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State({
    s =>
      val (a, next) = run(s)
      (f(a), next)
  })

  def map2[B,C](rb: State[S, B])(f: (A, B) => C): State[S, C] = State({
    s =>
      val (a, sa) = run(s)
      val (b, sb) = rb.run(sa)
      (f(a, b), sb)
  })

  def flatMap[B](f: A => State[S, B]): State[S, B] = State({
    s =>
      val (a, sa) = run(s)
      f(a).run(sa)
  })

}