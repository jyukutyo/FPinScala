package exercise3

import scala.annotation.tailrec

object Exercise3_13 extends App {

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


    def reverse[A](l: List[A]): List[A] = {
      foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))
    }

    /*
    The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack overflows
    when implementing a strict `foldRight` function as we've done in this chapter. (We'll revisit this in a later chapter,
    when we discuss laziness).
    The other implementations build up a chain of functions which, when called, results in the operations being performed
    with the correct associativity. We are calling `foldRight` with the `B` type being instantiated to `B => B`, then
    calling the built up function with the `z` argument. Try expanding the definitions by substituting equals for equals
    using a simple example, like `foldLeft(List(1,2,3), 0)(_ + _)` if this isn't clear. Note these implementations are
    more of theoretical interest - they aren't stack-safe and won't work for large lists.
    */
    def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
      foldLeft(reverse(l), z)((b,a) => f(a,b))

    def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
      foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

    def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
      foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  }

}
