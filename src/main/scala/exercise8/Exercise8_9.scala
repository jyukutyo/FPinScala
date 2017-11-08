
package exercise8

object Exercise8_9 extends App {

  case class Prop(run: (TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = {
      Prop((tc: TestCases, rng: RNG) => {
        val r = this.run(tc, rng)
        r match {
          case f: Falsified => f
          case Passed => p.run(tc, rng)
        }
      })
    }

    def ||(p: Prop): Prop = {
      Prop((tc: TestCases, rng: RNG) => {
        val r = this.run(tc, rng)
        r match {
          case Passed => Passed
          case _: Falsified => p.run(tc, rng)
        }
      })
    }
  }

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, successed: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }

  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int

  case class Gen[A](sample: State[RNG, A]) {

  }
}
