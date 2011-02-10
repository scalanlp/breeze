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
  }

  implicit def seqExtras[T](s: Seq[T]) = new SeqExtras(s);

  implicit def seqExtras[T](s: Array[T]) = new SeqExtras(s);
}