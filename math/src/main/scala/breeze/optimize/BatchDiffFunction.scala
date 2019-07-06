package breeze.optimize

import breeze.stats.distributions.Rand
import breeze.util.Isomorphism

import scala.collection.immutable
import breeze.linalg.support.CanCopy

import scala.collection.compat.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

/**
 * A diff function that supports subsets of the data. By default it evaluates on all the data
 */
trait BatchDiffFunction[T] extends DiffFunction[T] with ((T, IndexedSeq[Int]) => Double) { outer =>

  /**
   * Calculates the gradient of the function on a subset of the data
   */
  def gradientAt(x: T, batch: IndexedSeq[Int]): T = calculate(x, batch)._2

  /**
   * Calculates the value of the function on a subset of the data
   */
  def valueAt(x: T, batch: IndexedSeq[Int]): Double = calculate(x, batch)._1

  /**
   * Calculates the value and gradient of the function on a subset of the data
   */
  def calculate(x: T, batch: IndexedSeq[Int]): (Double, T)

  override def calculate(x: T): (Double, T) = calculate(x, fullRange)
  override def valueAt(x: T): Double = valueAt(x, fullRange)
  override def gradientAt(x: T): T = gradientAt(x, fullRange)

  def apply(x: T, batch: IndexedSeq[Int]) = valueAt(x, batch)

  override def cached(implicit copy: CanCopy[T]) = {
    if (this.isInstanceOf[CachedBatchDiffFunction[_]]) {
      this
    } else {
      new CachedBatchDiffFunction[T](this)
    }
  }

  /**
   * The full size of the data
   */
  def fullRange: IndexedSeq[Int]

  def withRandomBatches(size: Int): StochasticDiffFunction[T] = new StochasticDiffFunction[T] {
    val rand = Rand.subsetsOfSize(fullRange, size)
    def calculate(x: T) = outer.calculate(x, rand.get)
  }

  def withScanningBatches(size: Int): StochasticDiffFunction[T] = new StochasticDiffFunction[T] {
    var lastStop = 0
    def nextBatch: ArraySeq[Int] = synchronized {
      val start = lastStop
      lastStop += size
      lastStop %= fullRange.size
      ArraySeq.unsafeWrapArray(Array.tabulate(size)(i => fullRange((i + start) % fullRange.size)))
    }

    def calculate(x: T) = outer.calculate(x, nextBatch)
  }

  def groupItems(groupSize: Int): BatchDiffFunction[T] = new BatchDiffFunction[T] {
    val numGroups = (outer.fullRange.size + groupSize - 1) / groupSize
    val groups: Array[immutable.IndexedSeq[Int]] =
      Array.tabulate(numGroups)(i => (i until outer.fullRange.length by numGroups).map(outer.fullRange))

    /**
     * Calculates the value and gradient of the function on a subset of the data
     */
    def calculate(x: T, batch: IndexedSeq[Int]): (Double, T) = outer.calculate(x, batch.flatMap(groups))

    override def gradientAt(x: T, batch: IndexedSeq[Int]): T = outer.gradientAt(x, batch.flatMap(groups))

    override def valueAt(x: T, batch: IndexedSeq[Int]): Double = outer.valueAt(x, batch.flatMap(groups))

    /**
     * The full size of the data
     */
    def fullRange: IndexedSeq[Int] = (0 until groups.length)
  }

  override def throughLens[U](implicit l: Isomorphism[T, U]): BatchDiffFunction[U] = new BatchDiffFunction[U] {

    /**
     * Calculates the value and gradient of the function on a subset of the data
     */
    override def calculate(u: U, batch: IndexedSeq[Int]): (Double, U) = {
      val t = l.backward(u)
      val (obj, gu) = outer.calculate(t, batch)
      (obj, l.forward(gu))
    }

    /**
     * The full size of the data
     */
    override def fullRange: IndexedSeq[Int] = outer.fullRange

    override def calculate(u: U) = {
      val t = l.backward(u)
      val (obj, gu) = outer.calculate(t)
      (obj, l.forward(gu))
    }
  }

}

object BatchDiffFunction {
  def wrap[T](f: DiffFunction[T]): BatchDiffFunction[T] = new BatchDiffFunction[T] {
    def calculate(x: T, batch: IndexedSeq[Int]): (Double, T) = f.calculate(x)

    override def gradientAt(x: T, batch: IndexedSeq[Int]): T = f.gradientAt(x)

    override def valueAt(x: T, batch: IndexedSeq[Int]): Double = f.valueAt(x)

    def fullRange: IndexedSeq[Int] = IndexedSeq(0)
  }
}
