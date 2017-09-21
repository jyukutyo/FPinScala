package exercise6

object Exercise6_7 extends App {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldLeft((List.empty[A], rng))((t, rand) => {
        val (v, halfRng) = rand(t._2)
        (t._1.::(v), halfRng)
      })
    }
    // We are using `foldRight`. If we used `foldLeft` then the values in the
    // resulting list would appear in reverse order. It would be arguably better
    // to use `foldLeft` followed by `reverse`. What do you think?
    // fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val l = List.fill(count)(int)
    val r: Rand[List[Int]] = sequence(l)
    r(rng)
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  println(ints(10)(SimpleRNG(199)))
}
