
package exercise4

object Exercise4_4 extends App {

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

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap(v => sequence(t) map (v :: _))
    }
  }

}
