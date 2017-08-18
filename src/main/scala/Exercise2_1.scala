object Exercise2_1 extends App {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, count: Int): Int = {
      if (count == 0) return a
      go(b, a + b, count - 1)
    }
    go(0, 1, n - 1)
  }
  println(fib(3)) // 1
  println(fib(13)) // 144

}
