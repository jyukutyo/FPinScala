
package exercise3

object Exercise3_27 extends App {

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
//    def loop(t: Tree[A], depth: Int): Int = {
//      t match {
//        case Leaf(_) => depth + 1
//        case Branch(l, r) => loop(l, depth + 1) max loop(r, depth + 1)
//      }
//    }
//    loop(t, 0)
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  println(depth(Branch(Branch(Leaf(7), Leaf(3)), Branch(Leaf(1), Leaf(2)))))
  println(depth(Branch(Branch(Leaf(-1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(-10), Leaf(0))))))

}
