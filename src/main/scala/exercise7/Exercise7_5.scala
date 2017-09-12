package exercise7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import scala.concurrent.duration.TimeUnit

object Exercise7_5 extends App {

  type Par[A] = ExecutorService => Future[A]

  object Par {

    def traverse[A, B](a: List[A])(f: A => Par[B]): Par[List[B]] = {
      a match {
        case Nil => unit(Nil)
        case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
      }
    }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      traverse(ps)(x => x)
    }

//    def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
//      l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))
//
//    // This implementation forks the recursive step off to a new logical thread,
//    // making it effectively tail-recursive. However, we are constructing
//    // a right-nested parallel program, and we can get better performance by
//    // dividing the list in half, and running both halves in parallel.
//    // See `sequenceBalanced` below.
//    def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
//    as match {
//      case Nil => unit(Nil)
//      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
//    }
//
//    // We define `sequenceBalanced` using `IndexedSeq`, which provides an
//    // efficient function for splitting the sequence in half.
//    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
//      if (as.isEmpty) unit(Vector())
//      else if (as.length == 1) map(as.head)(a => Vector(a))
//      else {
//        val (l,r) = as.splitAt(as.length/2)
//        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
//      }
//    }
//
//    def sequence[A](as: List[Par[A]]): Par[List[A]] =
//      map(sequenceBalanced(as.toIndexedSeq))(_.toList)
//
//    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
//      map2(pa, unit(()))((a, _) => f(a))

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
}