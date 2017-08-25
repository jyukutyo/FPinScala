
package exercise4

object Exercise4_6 extends App {

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  sealed trait Either[+E, +A] {
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
