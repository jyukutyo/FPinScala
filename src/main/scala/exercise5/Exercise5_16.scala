package exercise5

object Exercise5_16 extends App {

  trait Stream[+A] {

    /*
    The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.
    The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
    */
    def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = {
      foldRight((z, Stream(z)))((a, p0) => {
        // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
        lazy val p1 = p0
        val b2 = f(a, p1._1)
        (b2, Stream.cons(b2, p1._2))
      })._2
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    def toList: List[A] = {
      this match {
        case Empty => List.empty
        case Cons(h, t) => h() :: t().toList
      }
    }
  }

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

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  Stream(1, 2, 3,4).scanRight(0)(_ + _)

}
