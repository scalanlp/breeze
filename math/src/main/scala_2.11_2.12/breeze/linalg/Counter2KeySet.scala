package breeze.linalg

import scala.collection.{Set, immutable}

private class Counter2KeySet[K1, K2, V](data: scala.collection.mutable.Map[K1, Counter[K2, V]]) extends Set[(K1, K2)] {
  def contains(k: (K1, K2)): Boolean = data.contains(k._1) && data(k._1).contains(k._2)

  def +(elem: (K1, K2)): Set[(K1, K2)] = immutable.Set.empty ++ iterator + elem
  def -(elem: (K1, K2)): Set[(K1, K2)] = immutable.Set.empty ++ iterator - elem

  def iterator: Iterator[(K1, K2)] = for ((k1, m) <- data.iterator; k2 <- m.keysIterator) yield (k1, k2)
}
