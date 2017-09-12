package exercise7

import java.util.concurrent._

import scala.concurrent.duration.TimeUnit

object Exercise7_8 extends App {

  type Par[A] = ExecutorService => Future[A]

  object Par {

    def equals[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def parMap[A, B] (ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
      val filtered: List[Par[A]] = as.filter(f).map(asyncF(x => x))
      sequence(filtered)
    }

    // We define `sequenceBalanced` using `IndexedSeq`, which provides an
    // efficient function for splitting the sequence in half.
    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Vector())
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l,r) = as.splitAt(as.length/2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] =
      map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def asyncF[A, B](f: A => B): A => Par[B] = {
      a => {
        lazyUnit(f(a))
      }
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

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

    def fork[A](a: => Par[A]): Par[A] =
      es => es.submit(new Callable[A] {
        override def call(): A = a(es).get
      })
  }

  val a = Exercise7_8.Par.lazyUnit(42 + 1)
  val S = Executors.newFixedThreadPool(1)
  println(Par.equals(S)(a, Exercise7_8.Par.fork(a)))

}





