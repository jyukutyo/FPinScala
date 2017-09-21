
package exercise8

object Exercise8_8 extends App {

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    Gen(State(RNG.double).flatMap(d => if (d <= g1._2 / (g1._2 + g2._2)) g1._1.sample else g2._1.sample))
  }

  case class Gen[A](sample: State[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen(sample.flatMap(a => f(a).sample))
    }
  }
}
