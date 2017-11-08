
package exercise8

object Exercise8_11 extends App {
  case class SGen[+A](forSize: Int => Gen[A]) {
    def flatMap[B](f: A => SGen[B]): SGen[B] = {
      SGen(n => forSize(n).flatMap(a => f(a).forSize(n)))
    }
  }

  case class Gen[A](sample: State[RNG, A]) {
    def unsized: SGen[A] = {
      SGen(_ => this)
    }
    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen(sample.flatMap(a => f(a).sample))
    }
  }
}
