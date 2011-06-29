package scalanlp.optimize

import scalanlp.util.Lens

/**
 * A differentiable function whose output is not guaranteed to be the same
 * across consecutive invocations.
 * @author dlwh
 */
trait StochasticDiffFunction[T] extends (T=>Double) { outer =>
   /** calculates the gradient at a point */
  def gradientAt(x: T): T = calculate(x)._2;
  /** calculates the value at a point */
  def valueAt(x:T): Double = calculate(x)._1;

  def apply(x:T) = valueAt(x);

  /** Calculates both the value and the gradient at a point */
  def calculate(x:T):(Double,T);

  /**
   * Lenses provide a way of mapping between two types, which we typically
   * use to convert something to a DenseVector or other Tensor for optimization purposes.
   */
  def throughLens[U](implicit l: Lens[T,U]) = new DiffFunction[U] {
    def calculate(u: U) = {
      val t = l.backward(u);
      val (obj,gu) = outer.calculate(t);
      (obj,l.forward(gu));
    }
  }
}