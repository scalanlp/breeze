package breeze.linalg

private class Counter2ProjectionMap[K1, K2, V](counter: Counter2[K1, K2, V], col: K2) extends scala.collection.mutable.Map[K1, V] {
  def addOne(elem: (K1, V)) = {
    counter.data(elem._1)(col) = (elem._2)
    this
  }
  override def iterator = for ((k1, map) <- counter.data.iterator; v <- map.get(col)) yield (k1, v)
  override def get(k1: K1) = counter.data.get(k1).map(_(col))
  override def apply(k1: K1) = counter(k1, col)
  override def update(k1: K1, v: V) = counter(k1, col) = v
  override def subtractOne(k1: K1) = {
    counter.data(k1)(col) = counter.default
    this
  }
}