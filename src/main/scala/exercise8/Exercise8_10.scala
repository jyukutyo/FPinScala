
package exercise8

object Exercise8_10 extends App {
  case class SGen[+A](forSize: Int => Gen[A])

  case class Gen[A](sample: State[RNG, A]) {
    def unsized: SGen[A] = {
      SGen(_ => this)
    }
  }
}
