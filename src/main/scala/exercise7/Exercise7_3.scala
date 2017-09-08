package exercise7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import scala.concurrent.duration.TimeUnit

object Exercise7_3 extends App {

  type Par[A] = ExecutorService => Future[A]

  object Par {

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    /*
    Note: this implementation will not prevent repeated evaluation if multiple threads call `get` in parallel. We could prevent this using synchronization, but it isn't needed for our purposes here (also, repeated evaluation of pure values won't affect results).
    */
    case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                                 f: (A,B) => C) extends Future[C] {
      @volatile var cache: Option[C] = None
      def isDone = cache.isDefined
      def isCancelled = a.isCancelled || b.isCancelled
      def cancel(evenIfRunning: Boolean) =
        a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
      def get = compute(Long.MaxValue)
      def get(timeout: Long, units: TimeUnit): C =
        compute(TimeUnit.NANOSECONDS.convert(timeout, units))

      private def compute(timeoutInNanos: Long): C = cache match {
        case Some(c) => c
        case None =>
          val start = System.nanoTime
          val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
          val stop = System.nanoTime;val aTime = stop-start
          val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
      }
    }

    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
      es => {
        val (af, bf) = (a(es), b(es))
        Map2Future(af, bf, f)
      }

//    private case class TimeoutFuture[A](f: Future[A]) extends Future[A] {
//
//      override def cancel(mayInterruptIfRunning: Boolean): Boolean = f.cancel(mayInterruptIfRunning)
//
//      override def isCancelled: Boolean = f.isCancelled
//
//      override def isDone: Boolean = f.isDone
//
//      override def get(timeout: Long, unit: TimeUnit): A = {
//        val start = System.nanoTime()
//      }
//    }
//
//    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
//      (es: ExecutorService) => {
//        val af = a(es)
//        val bf = b(es)
//        val taf = TimeoutFuture(af)
//        val afValue = taf.get
//        UnitFuture(f(afValue, bf.get))
//      }

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        override def call(): A = a(es).get
      })
  }
}