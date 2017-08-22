package exercise2


object Exercise2_3 extends App {

  def curry[A,B,C] (f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

}
