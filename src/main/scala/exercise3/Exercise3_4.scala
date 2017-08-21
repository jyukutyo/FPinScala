
package exercise3

import scala.annotation.tailrec

object Exercise3_4 extends App {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A] (head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A] (as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    @tailrec
    def drop[A] (l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
    }

  }

}
