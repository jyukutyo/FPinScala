
package exercise4

object Exercise4_3 extends App {

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

  def lift[A, B](f: A => B): Option[A] => Option[B] = {
    (a) => a map f
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (a2 => b map (b2 => f(a2, b2)))
  }


}
