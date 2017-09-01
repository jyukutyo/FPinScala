package exercise5

object Exercise5_10 extends App {

  def fibs(): Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a + b, loop(b, a + b))
    }
    Stream.cons(0, Stream.cons(1, loop(0, 1)))

//    def go(f0: Int, f1: Int): Stream[Int] =
//      Stream.cons(f0, go(f1, f0+f1))
//    go(0, 1)
  }

  trait Stream[+A] {
    def take(n: Int): Stream[A] = {
      this match {
        case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
        case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
        case _ => Stream.empty
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
