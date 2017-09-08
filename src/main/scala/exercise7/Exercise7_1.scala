package exercise7

object Exercise7_1 extends App {

  case class Par[A](val a: A)

  object Par {

    /* def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] */
    def map2[A, B](a: Par[A], b: Par[A])(f: (A, A) => B): Par[B] = {
      Par(f(a, b))
    }

    def unit[A](a: => A): Par[A] = {
      Par(a)
    }

    def get[A](a: Par[A]): A = {
      a.a
    }
  }
}