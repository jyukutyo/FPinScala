
package exercise3

object Exercise3_2 extends App {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A] (head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A] (as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A] (l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(_, xs) => xs
      }
    }
  }
}
