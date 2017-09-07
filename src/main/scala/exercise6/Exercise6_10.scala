package exercise6

object Exercise6_10 extends App {

  case class State[S, +A](run: S => (A, S)) {
//    def unit[S, A](a: A): State[S, A] = {
//      State(s => (a, s))
//    }

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
    def unit[S, A](a: A): State[S, A] = {
      State(s => (a, s))
    }

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
      fs.foldRight(unit[S, List[A]](List[A]()))((f, acc) => f.map2(acc)(_ :: _))
    }

  }

  type Rand[A] = State[RNG, A]

  trait RNG {
    def nextInt: (Int, RNG)
  }

}
