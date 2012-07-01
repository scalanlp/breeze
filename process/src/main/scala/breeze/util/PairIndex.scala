package breeze.util

/**
 * An index over pairs from a pair of indexes
 * @author dlwh
 */
class PairIndex[T,U](tIndex: Index[T], uIndex: Index[U]) extends Index[(T,U)] {
  def iterator = for( t <- tIndex iterator; u <- uIndex iterator) yield (t,u);

  def pairs = iterator.zipWithIndex;

  def unapply(i: Int) = if(i >= 0) {
    val ti =  i / uIndex.size;
    val ui = i % uIndex.size;
    for(t <- tIndex.unapply(ti); u <- uIndex.unapply(ui)) yield t -> u;
  } else {
    None
  }

  def apply(t: (T, U)) = {
    val i = tIndex(t._1) * uIndex.size + uIndex(t._2)
    if(i < 0) -1
    else i
  };

  override def size: Int = tIndex.size * uIndex.size
}