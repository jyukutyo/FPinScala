
package exercise8

object Exercise8_13 extends App {

  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => g.listOfN(n max 1))
  }

  val smallInt = Gen.choose(-10, 10)
  val maxProp1 = forAll[List[Int]](listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(n => g.listOfN(n))
  }

  case class SGen[A](forSize: Int => Gen[A]) {
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def &&(p: Prop) = Prop {
      (max, n, rng) =>
        run(max, n, rng) match {
          case Passed => p.run(max, n, rng)
          case x => x
        }
    }

    def ||(p: Prop) = Prop {
      (max, n, rng) =>
        run(max, n, rng) match {
          // In case of failure, run the other prop.
          case Falsified(msg, _) => p.tag(msg).run(max, n, rng)
          case x => x
        }
    }

    /* This is rather simplistic - in the event of failure, we simply prepend
     * the given message on a newline in front of the existing message.
     */
    def tag(msg: String) = Prop {
      (max, n, rng) =>
        run(max, n, rng) match {
          case Falsified(e, c) => Falsified(msg + "\n" + e, c)
          case x => x
        }
    }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g)(f)

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified afger $n passed tests:¥n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
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
  type MaxSize = Int

  case class Gen[A](sample: State[RNG, A]) {
    def listOfN(size: Int): Gen[List[A]] =
      Gen.listOfN(size, this)
  }

  object Gen {
    def unit[A](a: => A): Gen[A] =
      Gen(State.unit(a))

    val boolean: Gen[Boolean] =
      Gen(State(RNG.boolean))

    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))
  }


}
