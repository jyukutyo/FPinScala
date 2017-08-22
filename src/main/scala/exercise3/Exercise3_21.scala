package exercise3

import scala.annotation.tailrec

object Exercise3_21 extends App {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A] (head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A] (as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

  }

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
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    add(foldRight(as, Nil: List[List[B]])((h, t) => Cons(f(h), t)))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  println(filter(List(1,2,3,4,5))(_ < 4))

}
