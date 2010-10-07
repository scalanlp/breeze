package scalanlp.optimize

import scalala.tensor.Tensor1

/**
 * 
 * @author dlwh
 */
class CachedDiffFunction[K,T<:Tensor1[K]](obj: DiffFunction[K,T]) extends DiffFunction[K,T] {
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
      lastX = x;
    }

    lastGradVal;
  }
}