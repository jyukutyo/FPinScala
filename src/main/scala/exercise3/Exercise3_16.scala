package exercise3

object Exercise3_16 extends App {

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A] (head: A, tail: List[A]) extends List[A]

  object List {
    def apply[A] (as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def addOne(l: List[Int]): List[Int] = {
    def loop(l: List[Int]): List[Int] = {
      l match {
        case Nil => l
        case Cons(x, xs) => Cons(x + 1, addOne(xs))
      }
    }
    loop(l)

    // foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))
  }

  println(addOne(List(1,2,3)))
}
