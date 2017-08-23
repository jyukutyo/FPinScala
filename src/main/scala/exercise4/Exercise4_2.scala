
package exercise4

object Exercise4_2 extends App {

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = {
      this match {
        case Some(g) => Some(f(g))
        case _ => None
      }
    }

    def getOrElse[B >: A](default: => B): B = {
      this match {
        case Some(g) => g
        case _ => default
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f).getOrElse(None)
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      map(v => Some(v)).getOrElse(ob)
    }

    def filter(f: A => Boolean): Option[A] = {
      flatMap(a => if (f(a)) Some(a) else None)
    }

  }

  /**
    * 平均を返す.
    */
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] ={
    val m = mean(xs)
    m.flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

}
