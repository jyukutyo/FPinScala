package exercise5

object Exercise5_15 extends App {

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, b)) => Stream.cons(a, unfold(b)(f))
      case _ => Stream.empty
    }
  }

  trait Stream[+A] {

    def tails: Stream[Stream[A]] = {
      unfold(this) {
        case s@Cons(_, t) => Some(s, t())
        case _ => None
      }
//      unfold(this) {
//        case Empty => None
//        case s => Some((s, s drop 1))
//      } append Stream(Stream.empty)
    }

    def append[B >: A](a: => Stream[B]): Stream[B] = {
      foldRight(Stream.empty[A])((a, b) => Stream.cons(a, b))
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    def drop(n: Int): Stream[A] = {
      this match {
        case Cons(_, t) if n > 0 => t().drop(n - 1)
        case _ => this
      }
    }

    def toList: List[A] = {
      this match {
        case Empty => List.empty
        case Cons(h, t) => h() :: t().toList
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
