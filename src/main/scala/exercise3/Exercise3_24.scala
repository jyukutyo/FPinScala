package exercise3

import scala.annotation.tailrec

object Exercise3_24 extends App {

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

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = {
    (l,r) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  /*
  It's good to specify some properties about these functions.
  For example, do you expect these expressions to be true?
    (xs append ys) startsWith xs
    xs startsWith Nil
    (xs append ys append zs) hasSubsequence ys
    xs hasSubsequence Nil
  */
  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }
  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_,t) => hasSubsequence(t, sub)
  }

  println(hasSubsequence(List(1,2,3,4,5), List(3,4)))
  println(hasSubsequence(List(1,2,3,4,5), List(3,5)))

}
