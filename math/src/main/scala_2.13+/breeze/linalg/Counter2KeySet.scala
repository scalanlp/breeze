package breeze.linalg

private class Counter2KeySet[K1, K2, V](data: scala.collection.mutable.Map[K1, Counter[K2, V]]) extends Set[(K1, K2)] {
  def contains(elem: (K1, K2)): Boolean = data.contains(elem._1) && data(elem._1).contains(elem._2)

  def incl(elem: (K1, K2)): Set[(K1, K2)] = Set() ++ iterator + elem
  def excl(elem: (K1, K2)): Set[(K1, K2)] = Set() ++ iterator - elem

  def iterator: Iterator[(K1, K2)] = for ((k1, m) <- data.iterator; k2 <- m.keysIterator) yield (k1, k2)
}
