
package exercise8

object Exercise8_6 extends App {
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
