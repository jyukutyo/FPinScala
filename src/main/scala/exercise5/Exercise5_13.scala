package exercise5

object Exercise5_13 extends App {

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, b)) => Stream.cons(a, unfold(b)(f))
      case _ => Stream.empty
    }
  }

  trait Stream[+A] {
    def map[B](f: A => B): Stream[B] = {
      unfold(this)(stream => stream match {
        case Cons(h, t) => Some(f(h()), t())
        case _ => None
      })
    }

    def take(n: Int): Stream[A] = {
      unfold((this, n)){
        tp => tp._1 match {
          case Cons(h, t) if n > 0 => Some(h(), (t(), tp._2 - 1))
          case _ => None
        }
//        case (Cons(h,t), 1) => Some((h(), (Stream.empty, 0)))
//        case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n-1)))
//        case _ => None

      }
    }

    def takeWhile(f: A => Boolean): Stream[A] = {
      unfold(this) {
        case Cons(h, t) if f(h()) => Some(h(), t())
        case _ => None
      }
    }

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case _ => None
      }
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
        case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
        case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
        case _ => None
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
