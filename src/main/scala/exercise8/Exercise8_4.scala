
package exercise8

object Exercise8_4 extends App {
  case class Gen[A](sample: State[RNG, A]) {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))
    }
  }
}
