package exercise2


object Exercise2_4 extends App {

  def uncurry[A, B, C] (f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

}
