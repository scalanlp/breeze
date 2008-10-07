package scalanlp.util;

/**
* Utilities and implicits for iterators. Nothing major.
* @author dlwh
*/
object Iterators {
  /**
   * Return the total count and the number of doubles.
   */
  def accumulateAndCount(it : Iterator[Double]) = it.foldLeft( (0.0,0) ) { (tup,d) =>
    (tup._1 + d, tup._2 + 1);
  }
}
