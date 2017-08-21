package exercise3

import scala.annotation.tailrec

object Exercise3_11 extends App {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A] (head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A] (as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }
    }

    def sum(ints: List[Int]): Int = {
      foldLeft(ints, 0)(_ + _)
    }

    def product(ints: List[Double]): Double = {
      foldLeft(ints, 1.0)(_ * _)
    }

    def length[A](as: List[A]): Int = {
      foldLeft(as, 0)((accumulator, _) => accumulator + 1)
    }

  }

}
