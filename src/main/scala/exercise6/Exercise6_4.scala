package exercise6

object Exercise6_4 extends App {

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(l: List[Int], n: Int, rng: RNG): (List[Int], RNG) = {
      if (n == 0) (l, rng)
      else {
        val (v, r) = rng.nextInt
        loop(l.::(v), n - 1, r)
      }
    }
    loop(List.empty, count, rng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

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

}
