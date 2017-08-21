import scala.annotation.tailrec

object Exercise2_2 extends App {

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

    @tailrec
    def loop(i: Int): Boolean = {
      if (i >= as.length - 1) return true

      if (!ordered(as(i), as(i + 1))) return false

      loop(i + 1)
    }

    loop(0)
  }

}
