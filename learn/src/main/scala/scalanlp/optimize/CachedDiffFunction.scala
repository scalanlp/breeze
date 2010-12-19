package scalanlp.optimize

import scalala.generic.collection.CanCopy
;

/**
 * 
 * @author dlwh
 */
class CachedDiffFunction[T:CanCopy](obj: DiffFunction[T]) extends DiffFunction[T] {
  /** calculates the gradient at a point */
  override def gradientAt(x: T): T = calculate(x)._2;
  /** calculates the value at a point */
  override def valueAt(x:T): Double = calculate(x)._1;

  private var lastX : T = _;
  private var lastGradVal : (Double,T) = _;

  /** Calculates both the value and the gradient at a point */
  def calculate(x:T):(Double,T) = {
    if(x != lastX) {
      lastGradVal = obj.calculate(x);
      lastX = implicitly[CanCopy[T]] apply (x);
    }

    lastGradVal;
  }
}

/**
 *
 * @author dlwh
 */
class CachedBatchDiffFunction[K,T:CanCopy](obj: BatchDiffFunction[T]) extends BatchDiffFunction[T] {
  /** calculates the gradient at a point */
  override def gradientAt(x: T, range: IndexedSeq[Int]): T = calculate(x,range)._2;
  /** calculates the value at a point */
  override def valueAt(x:T, range: IndexedSeq[Int]): Double = calculate(x,range)._1;

  private var lastX : T = _;
  private var lastRange : IndexedSeq[Int] = _;
  private var lastGradVal : (Double,T) = _;

  def fullRange = obj.fullRange;

  /** Calculates both the value and the gradient at a point */
  override def calculate(x:T, range: IndexedSeq[Int]):(Double,T) = {
    if(x != lastX || range != lastRange) {
      lastGradVal = obj.calculate(x, range);
      lastX = implicitly[CanCopy[T]] apply (x);
      lastRange = range;
    }

    lastGradVal;
  }
}