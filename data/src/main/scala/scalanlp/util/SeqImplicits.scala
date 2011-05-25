package scalanlp
package util

/**
 * 
 * @author dlwh
 */
trait SeqImplicits {
  class SeqExtras[T](s: Seq[T]) {
    def argmax(implicit ordering: Ordering[T]) = {
      s.zipWithIndex.reduceLeft( (a,b) => if(ordering.gt(a._1,b._1)) a else b)._2;
    }
    def argmin(implicit ordering: Ordering[T]) = {
      s.zipWithIndex.reduceLeft( (a,b) => if(ordering.lt(a._1,b._1)) a else b)._2;
    }

    def asMap = new Map[Int,T] {
      def -(key: Int) = Map() ++ this - key;

      def +[B1 >: T](kv: (Int, B1)) = Map() ++ this + kv

      def get(key: Int) = {
          if(key >= 0 && key < s.length) Some(s(key))
          else None
        }

      def iterator = 0 until s.length zip s iterator;

    }
  }

  implicit def seqExtras[T](s: Seq[T]) = new SeqExtras(s);

  implicit def arraySeqExtras[T](s: Array[T]) = new SeqExtras(s);
}