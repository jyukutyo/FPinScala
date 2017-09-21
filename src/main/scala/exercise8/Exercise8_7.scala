
package exercise8

object Exercise8_7 extends App {
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    // Gen(g1.sample.map2(g1.sample)((a, b) => a))
    boolean.flatMap(b => if (b) g1 else g2)
  }

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  case class Gen[A](sample: State[RNG, A]) {

    def listOfN(size: Gen[Int]): Gen[List[A]] = {
      size.flatMap(n => {
        Gen(State.sequence(List.fill(n)(this.sample)))
      })
    }

    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen(sample.flatMap(a => f(a).sample))
    }
  }
}
