package breeze.util

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

/**
 * Stores various implicits, also available by importing breeze.util._
 */
object Implicits extends DoubleImplicits with IteratorImplicits {
  implicit class scEnrichColl[Coll <: Traversable[(_, _)]](val __this: Coll) extends AnyVal {
    def toMultiMap[Result, A, B](
        implicit view: Coll <:< Traversable[(A, B)],
        cbf: CanBuildFrom[Coll, B, Result]): Map[A, Result] = {
      var result = collection.mutable.Map[A, mutable.Builder[B, Result]]()
      result = result.withDefault { a =>
        val r = cbf(__this); result.update(a, r); r
      }

      for ((a, b) <- view(__this)) {
        result(a) += b
      }

      result.mapValues(_.result()).toMap
    }
  }

  implicit class scEnrichArray[A, B](val __this: Array[(A, B)]) extends AnyVal {
    def toMultiMap[Result](implicit cbf: CanBuildFrom[Array[(A, B)], B, Result]): Map[A, Result] = {
      var result = collection.mutable.Map[A, mutable.Builder[B, Result]]()
      result = result.withDefault { a =>
        val r = cbf(__this); result.update(a, r); r
      }

      for ((a, b) <- __this) {
        result(a) += b
      }

      Map.empty ++ result.mapValues(_.result())
    }

  }

}

trait DoubleImplicits {
  implicit class RichDouble(x: Double) {
    def closeTo(y: Double, tol: Double = 1E-5) = {
      (math.abs(x - y) / (math.abs(x) + math.abs(y) + 1e-10) < tol);
    }
    def isDangerous = x.isNaN || x.isInfinite
  }

  implicit class RichFloat(x: Float) {
    def closeTo(y: Float, tol: Double = 1E-5) = {
      (math.abs(x - y) / (math.abs(x) + math.abs(y) + 1e-10) < tol);
    }
    def isDangerous: Boolean = x.isNaN || x.isInfinite
  }
}

trait IteratorImplicits {
  class RichIterator[T](iter: Iterator[T]) {
    def tee(f: T => Unit): Iterator[T] = new Iterator[T] {
      def next(): T = {
        val n = iter.next;
        f(n);
        n
      }

      def hasNext: Boolean = {
        iter.hasNext;
      }
    }

    def takeUpToWhere(f: T => Boolean): Iterator[T] = new Iterator[T] {
      var done = false
      def next(): T = {
        if (done) throw new NoSuchElementException()
        val n = iter.next;
        done = f(n)
        n
      }

      def hasNext: Boolean = {
        !done && iter.hasNext;
      }
    }

    def last: T = {
      var x = iter.next()
      while (iter.hasNext) {
        x = iter.next()
      }
      x
    }
  }

  implicit def scEnrichIterator[T](iter: Iterator[T]): RichIterator[T] = new RichIterator[T](iter)
}
