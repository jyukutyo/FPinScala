package exercise6

object Exercise6_1 extends App {

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    def loop(rng: RNG): (Int, RNG) = {
      val (v, r) = rng.nextInt
      if (v < 0 || v == Int.MinValue) loop(r) else (v, r)
    }
    loop(rng)

//    val (i, r) = rng.nextInt
//    (if (i < 0) -(i + 1) else i, r)
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
