package exercise5

object Exercise5_12 extends App {

  def fibs(): Stream[Int] = {
    unfold((0, 1))(t => Some(t._1, (t._2, t._1 + t._2)))
    // unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }
  }

  def from(n: Int): Stream[Int] = {
    unfold(n)(n => Some(n, n + 1))
  }

  def constant[A](a: A): Stream[A] = {
    unfold(a)(_ => Some((a, a)))
  }

  val ones: Stream[Int] = constant(1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, b)) => Stream.cons(a, unfold(b)(f))
      case _ => Stream.empty
    }
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
