
package exercise3

object Exercise3_25 extends App {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  println(size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))
  println(size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Branch(Leaf("d"), Leaf("e"))))))
}
