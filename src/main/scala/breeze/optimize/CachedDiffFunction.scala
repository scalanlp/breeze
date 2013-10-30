package breeze.optimize

import breeze.linalg.support.CanCopy
import breeze.linalg.copy
import breeze.concurrent.ThreadLocal


/**
 *
 * @author dlwh
 */
class CachedDiffFunction[T:CanCopy](obj: DiffFunction[T]) extends DiffFunction[T] {
  /** calculates the gradient at a point */
  override def gradientAt(x: T): T = calculate(x)._2
  /** calculates the value at a point */
  override def valueAt(x:T): Double = calculate(x)._1

  private var lastData:(T, Double, T) = null

  /** Calculates both the value and the gradient at a point */
  def calculate(x:T):(Double,T) = {
    this.synchronized {
      if (lastData == null || x != lastData._1) {
        val newData = obj.calculate(x)
        lastData = (copy(x), newData._1, newData._2)
      }
    }

    val (_, v, g) = lastData
    v -> g
  }
}

/**
 * @author dlwh
 */
class CachedBatchDiffFunction[T:CanCopy](obj: BatchDiffFunction[T]) extends BatchDiffFunction[T] {
  /** calculates the gradient at a point */
  override def gradientAt(x: T, range: IndexedSeq[Int]): T = calculate(x,range)._2
  /** calculates the value at a point */
  override def valueAt(x:T, range: IndexedSeq[Int]): Double = calculate(x,range)._1

  private var lastData: (T, Double, T, IndexedSeq[Int])  = null

  def fullRange = obj.fullRange

  /** Calculates both the value and the gradient at a point */
  override def calculate(x:T, range: IndexedSeq[Int]):(Double,T) = {
    this.synchronized {
      if (lastData == null || range != lastData._4 || x != lastData._1) {
        val newData = obj.calculate(x, range)
        lastData = (copy(x), newData._1, newData._2, range)
      }
    }

    val (_, v, g, _) = lastData
    v -> g
  }
}