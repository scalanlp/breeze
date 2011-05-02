package scalanlp.data.process;

import scalanlp.data._;
import scalala.tensor.mutable.Counter;

/**
* Filter that removes rare word that occur in fewer than threshold documents
* Syntax: new RemoveRareWords(10) apply (data);
*
* @author dlwh
*/
class RemoveRareWords(threshold:Int) {
  def apply[T,Obs<:Observation[Seq[T]]](data: Seq[Obs]) = {
    val c = Counter[T,Int]();
    for(d <- data;
        w <- d.features.distinct) {
      c(w) += 1;
    }

    for(d <- data) 
      yield for(seq <- d)
      yield for(w <- seq if c(w) >= threshold) yield w;
  }
}
