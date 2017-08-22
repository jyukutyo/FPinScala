
package exercise3

object Exercise3_26 extends App {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  println(maximum(Branch(Branch(Leaf(7), Leaf(3)), Branch(Leaf(1), Leaf(2)))))
  println(maximum(Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(-10), Leaf(0))))))

}
