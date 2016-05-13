package state

/**
  * Input
  * --
  * Desc:
  * Changes:
  * 2016/5/6 - Created by z_jiang.
  * --
  */

object Candy {
  sealed trait Input {

  }

  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)


  type CoinCandy = (Int, Int)

  def unit(init: CoinCandy): Input => State[Machine, CoinCandy]  = _ => State(x => (init, x))

  def change: Input => State[Machine, CoinCandy] = {
    case Coin => {
      State({
        case Machine(true, 0, n) =>
          ((n+1, 0), Machine(true, 0, n+1))
        case Machine(true, m, n) =>
          ((n+1, m), Machine(false, m, n+1))
        case Machine(false, m, n) =>
          ((n+1, m), Machine(false, m, n+1))
      })
    }
    case Turn => {
      State({
        case Machine(false, m, n) =>
          ((n, m-1), Machine(true, m-1, n))
        case machine @ Machine(true, m, n) =>
          ((n, m), machine)
      })
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State({
      initialMachine =>
        val finalMachine = inputs.foldLeft(initialMachine)((machine, input) => {
          val state = change(input).run(machine)
          state._2
        })
        val cc = (finalMachine.candies, finalMachine.coins)
        (cc, finalMachine)
    })
  }


  def main(args: Array[String]): Unit = {
    val initMachine = Machine(true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Coin, Turn, Turn, Turn)
    val finalState = simulateMachine(inputs).run(initMachine)
    println(finalState)
  }

}
