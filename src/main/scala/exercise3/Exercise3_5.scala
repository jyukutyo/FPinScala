
package exercise3

import scala.annotation.tailrec

object Exercise3_5 extends App {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A] (head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A] (as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    @tailrec
    def dropWhile[A] (l: List[A], f: A => Boolean): List[A] = {
      l match {
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _ => l
      }
    }
  }

}
