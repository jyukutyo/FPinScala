package exercise5

object Exercise5_1 extends App {

  trait Stream[+A] {
    def toList: List[A] = {
      this match {
        case Empty => List.empty
        case Cons(h, t) => h() :: t().toList
      }
    }

//    def toList: List[A] = {
//      @annotation.tailrec
//      def go(s: Stream[A], acc: List[A]): List[A] = s match {
//        case Cons(h,t) => go(t(), h() :: acc)
//        case _ => acc
//      }
//      go(this, List()).reverse
//    }
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
