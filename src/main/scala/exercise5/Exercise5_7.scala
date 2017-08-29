package exercise5

object Exercise5_7 extends App {

  trait Stream[+A] {

    def map[B](f: A => B): Stream[B] = {
      foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))
    }

    def filetr(f: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)
    }

    def append[B >: A](a: => Stream[B]): Stream[B] = {
      foldRight(Stream.empty[A])((a, b) => Stream.cons(a, b))
    }

    def flatMap[B](f: A => Option[B]): Stream[B] = {
      foldRight(Stream.empty[B])((a, b) => {
        val op = f(a)
        op match {
          case None => b
          case Some(v) => Stream.cons(v, b)
        }
      })
      // foldRight(Stream.empty[B])((h,t) => f(h) append t)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
  }
}
