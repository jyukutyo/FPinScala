
package exercise4

object Exercise4_7 extends App {

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  sealed trait Either[+E, +A] {

    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
      traverse(es)(x => x)
//      es match {
//        case Nil => Right(Nil)
//        case h :: t => h flatMap(v => sequence(t) map (v :: _))
//      }
    }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      as match {
        case Nil => Right(Nil)
//        case h :: t => f(h) flatMap (v => traverse(t)(f) map (v :: _))
        case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
      }
    }

    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Right(v) => Right(f(v))
        case Left(v) => Left(v)
      }
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Right(v) => f(v)
        case Left(v) => Left(v)
      }
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
      this match {
        case Right(v) => Right(v)
        case Left(_) => b
      }
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
      for { a <- this; b1 <- b } yield f(a,b1)
      // flatMap(a => b.map(v => f(a, v)))
    }
  }
}
