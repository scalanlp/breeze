package scalanlp.optimize

import scalala.tensor.Tensor1
import scalala.tensor.operators.TensorSelfOp
import scalala.tensor.operators.TensorShapes._;

/**
 * 
 * @author dlwh
 */
class CachedDiffFunction[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]](obj: DiffFunction[K,T]) extends DiffFunction[K,T] {
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
      lastX = x.copy;
    }

    lastGradVal;
  }
}

/**
 *
 * @author dlwh
 */
class CachedBatchDiffFunction[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]](obj: BatchDiffFunction[K,T]) extends BatchDiffFunction[K,T] {
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
      lastX = x.copy;
      lastRange = range;
    }

    lastGradVal;
  }
}