package exercise2


object Exercise2_5 extends App {

  def compose[A, B, C] (f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

}
