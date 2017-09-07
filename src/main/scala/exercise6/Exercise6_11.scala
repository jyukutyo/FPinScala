
package exercise6

object Exercise6_11 extends App {

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  object Candy {
    def update = (i: Input) => (s: Machine) =>
      (i, s) match {
        case (_, Machine(_, 0, _)) => s // キャンディが0ならMachineはそのままとなる
        case (Coin, Machine(false, _, _)) => s  // ロックを解除しているので、硬貨投入ではMachineはそのままとなる
        case (Turn, Machine(true, _, _)) => s // ロックしているので、ハンドルを回してもMachineはそのままとなる
        case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1) // ロックしていてキャンディあり、硬貨投入した場合、ロックを解除し硬貨を1増やす
        case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin) // ロックを解除していてキャンディと硬貨があり、ハンドルを回した場合、ロックをしてキャンディを1減らす
      }
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      for {
        // 合成 Inputを引数にupdateを呼び出し、Machine => Machineの関数を作り、この関数をmodifyの引数にする
        // sequenceで順に操作を実行し、最終状態のMachineを引数にgetする
        _ <- State.sequence(inputs.map((State.modify[Machine] _).compose(update)))
        s <- State.get
      } yield (s.coins, s.candies)
    }

  }
  case class State[S, +A](run: S => (A, S)) {

    def flatMap[B](g: A => State[S, B]): State[S, B] = {
      State(s => {
        val (a, s2) = this.run(s)
        g(a).run(s2)
      })
    }

    def map[B](f: A => B): State[S, B] = {
      flatMap(a => State.unit(f(a)))
    }

    def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = {
      flatMap(a => rb.map(b => f(a, b)))
    }
  }

  object State {
    def modify[S](f: S => S): State[S, Unit] = {
      for {
        s <- get
        _ <- set(f(s))
      } yield ()
    }

    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def unit[S, A](a: A): State[S, A] = {
      State(s => (a, s))
    }

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
      fs.foldRight(unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))
    }

  }

}