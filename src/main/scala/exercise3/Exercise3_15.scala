package exercise3

import scala.annotation.tailrec

object Exercise3_15 extends App {

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

    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def append[A](l: List[A], a: List[A]): List[A] = {
      foldRight(l, a)((a, b) => Cons(a, b))
    }

    def add[A](lists: List[List[A]]): List[A] = {
      foldLeft(lists, Nil: List[A])((b, a) => append(b, a))
      // same as
      // foldLeft(lists, Nil: List[A])(append)
    }
  }

  println(List.add(List(List(1,2,3), List(4,5))))

}
