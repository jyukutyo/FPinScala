package exercise5

object Exercise5_14 extends App {

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, b)) => Stream.cons(a, unfold(b)(f))
      case _ => Stream.empty
    }
  }

  trait Stream[+A] {

    def startsWith[B >: A](s: Stream[B]): Boolean = {
      this.zipAll(s).takeWhile {
        case (Some(a), Some(b)) if a == b => true
        case _ => false
      }.toList.length == s.toList.length

//      zipAll(s).takeWhile(!_._2.isEmpty) forAll {
//        case (h, h2) => h == h2
//      }
    }

    def forAll(p: A => Boolean): Boolean = {
      foldRight(true)((a, b) => p(a) && b)
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
      zipWithAll(s2)((_,_))

    def takeWhile(f: A => Boolean): Stream[A] = {
      unfold(this) {
        case Cons(h, t) if f(h()) => Some(h(), t())
        case _ => None
      }
    }

    def startsWithFirstTry[B >: A](s: Stream[B]): Boolean = {
      val r = unfold((this, s)) {
        case (Cons(_, _), Empty) => Some(true, (Empty, Empty))
        case (Empty, Cons(_, _)) => Some(false, (Empty, Empty))
        case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => Some(false, (t1(), t2()))
        case (Cons(_, t1), Cons(_, _)) => Some(false, (t1(), s))
        case _ => None
      }
      r.exist(b => b)
    }

    def exist(p: A => Boolean): Boolean = {
      foldRight(false)((a, b) => p(a) || b)
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    def map[B](f: A => B): Stream[B] = {
      unfold(this)(stream => stream match {
        case Cons(h, t) => Some(f(h()), t())
        case _ => None
      })
    }

    def take(n: Int): Stream[A] = {
      unfold((this, n)) {
        case (Cons(h, t), 1) => Some((h(), (Stream.empty, 0)))
        case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
        case _ => None
      }
    }

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
      unfold((this, s2)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
        case _ => None
      }
    }

    def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
      unfold((this, s2)) {
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Stream.empty[B]))
        case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Stream.empty[A] -> t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
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
