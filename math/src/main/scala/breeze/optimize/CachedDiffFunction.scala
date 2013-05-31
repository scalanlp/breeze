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

  private val lastData = new ThreadLocal[(T, Double, T)](null)

  /** Calculates both the value and the gradient at a point */
  def calculate(x:T):(Double,T) = {
    val last = lastData()
    if(last == null || x != last._1) {
      val newData = obj.calculate(x)
      lastData.set ( (copy(x), newData._1, newData._2))
    }

    val (_, v, g) = lastData.get()
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

  private val lastData = new ThreadLocal[(T, Double, T, IndexedSeq[Int])](null)

  def fullRange = obj.fullRange

  /** Calculates both the value and the gradient at a point */
  override def calculate(x:T, range: IndexedSeq[Int]):(Double,T) = {
    val last = lastData()
    if(last == null || range != last._4 || x != last._1) {
      val newData = obj.calculate(x, range)
      lastData.set ( (copy(x), newData._1, newData._2, range))
    }

    val (_, v, g, _) = lastData.get()
    v -> g
  }
}