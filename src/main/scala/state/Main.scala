package state

import state.Rand.Rand

/**
  * Main
  * --
  * Desc:
  * Changes:
  * 2016/5/3 - Created by z_jiang.
  * --
  */
object Main {
  def main(args: Array[String]): Unit = {
    println(Exercise.ints(10)(new SimpleRNG(100L)))

    val intRand = Rand.ints(10)

    val simpleRNG = new SimpleRNG(100L)

    println(intRand(simpleRNG))

    println(Rand.nonNegativeLessThan3(100)(new SimpleRNG(100)))


    val intRnd: Rand[Int] = _.nextInt

    val doubleRnd: Rand[Double] = Rand.map(intRnd)(_ * 1.0/ Int.MaxValue)


    val d2Rnd = Rand.mapF(intRnd)(_ * 1.0 / Int.MaxValue)

    println(doubleRnd(simpleRNG))
    println(d2Rnd(simpleRNG))

    def i2dr: Int => Rand[Double] = {
      i => rng => {
        (i * 1.0 / Int.MaxValue, rng)
      }
    }

    val d3Rnd = Rand.flatMap(intRnd)(i2dr)
    println(d3Rnd(simpleRNG))


    val zero = Rand.rollDie(SimpleRNG(5))._1
    println(zero)


    val int1State = State({ i: Int => (1,1) })
    val int2State = State({ i: Int => (2,2) })

    val ls = List(int1State, int2State)
    println(ls.map(_.run(100)))

    val ls2 = State.sequence(ls).run(100)
    println(ls2)
  }

}
