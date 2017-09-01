package exercise5

object Exercise5_8 extends App {

  def constant[A](a: A): Stream[A] = {
    lazy val infinite: Stream[A] = Stream.cons(a, infinite)
    // lazy val infinite: Stream[A] = Cons(() => a, () => infinite)
    infinite
  }

  trait Stream[+A]
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
