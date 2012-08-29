package breeze.util

/**
 * Stores various implicits, also available by importing breeze.util._
 */
object Implicits extends DoubleImplicits with IteratorImplicits {

}

trait DoubleImplicits {
  class RichDouble(x: Double) {
    def closeTo(y: Double, tol: Double=1E-5) = {
      (math.abs(x - y) / (math.abs(x) + math.abs(y) + 1e-10) < tol);
    }
    def isDangerous = x.isNaN || x.isInfinite
  }

  implicit def scEnrichDouble(x: Double) = new RichDouble(x);
}


trait IteratorImplicits {
  class RichIterator[T](iter: Iterator[T]) {
    def tee(f: T=>Unit):Iterator[T] = new Iterator[T] {
      def next = {
        val n = iter.next;
        f(n);
        n
      }

      def hasNext = {
        iter.hasNext;
      }
    }

    def takeUpToWhere(f: T=>Boolean):Iterator[T] = new Iterator[T] {
      var done = false
      def next = {
        if(done) throw new NoSuchElementException()
        val n = iter.next;
        done = f(n)
        n
      }

      def hasNext = {
        !done && iter.hasNext;
      }
    }
  }

  implicit def scEnrichIterator[T](iter: Iterator[T]) = new RichIterator[T](iter);
}