
package exercise3

object Exercise3_29 extends App {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((x, y) => 1 + x + y)
  }

  def depth[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)((x, y) => 1 + (x max y))
  }

  def maximum(t: Tree[Int]): Int = {
    fold(t)(v => v)((x, y) => x max y)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(v => Leaf(f(v)): Tree[B])((x, y) => Branch(x, y))
  }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = {
  // def fold[A, B, C](t: Tree[A], acc: C)(f: A => B): C = {
    t match {
      case Leaf(v) => l(v)
      case Branch(x, y) => b(fold(x)(l)(b), fold(y)(l)(b))
    }
  }

  println(size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Branch(Leaf("d"), Leaf("e"))))))
  println(maximum(Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(-10), Leaf(0))))))
  println(depth(Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(-10), Leaf(0))))))
  println(map(Branch(Branch(Leaf(7), Leaf(3)), Branch(Leaf(1), Leaf(2))))(_ * 5))
}
