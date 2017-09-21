
package exercise8

object Exercise8_5 extends App {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))
    // Gen(State(RNG.int).map(n => n % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  case class Gen[A](sample: State[RNG, A]) {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
    }
  }
}
