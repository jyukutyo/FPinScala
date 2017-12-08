
package exercise8

object Exercise8_12 extends App {

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => listOfN(n, g))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  case class SGen[A](forSize: Int => Gen[A]) {
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
