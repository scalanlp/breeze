package breeze.optimize

import breeze.math.InnerProductSpace

/**
 * A line search optimizes a function of one variable without
 * analytic gradient information. Differs only in whether or not it tries to find an exact minimizer
 * @author dlwh
 */
trait LineSearch extends ApproximateLineSearch

/**
 * A line search optimizes a function of one variable without
 * analytic gradient information. It's often used approximately (e.g. in
 * backtracking line search), where there is no intrinsic termination criterion, only extrinsic
 * @author dlwh
 */
trait ApproximateLineSearch {
  final case class State(alpha: Double, value: Double, deriv: Double)
  def iterations(f: DiffFunction[Double], init: Double = 1.0):Iterator[State]
  def minimize(f: DiffFunction[Double], init: Double = 1.0):Double = iterations(f, init).reduceLeft( (a,b) => b).alpha
}

object LineSearch {
  def functionFromSearchDirection[T](f: DiffFunction[T], x: T, direction: T)(implicit prod: InnerProductSpace[T, Double]):DiffFunction[Double] = new DiffFunction[Double] {
    import prod._

    /** calculates the value at a point */
    override def valueAt(alpha: Double): Double = f.valueAt(x + direction * alpha)

    /** calculates the gradient at a point */
    override def gradientAt(alpha: Double): Double = f.gradientAt(x + direction * alpha) dot direction

    /** Calculates both the value and the gradient at a point */
    def calculate(alpha: Double): (Double, Double) = {
      val (ff, grad) = f.calculate(x + direction * alpha)
      ff -> (grad dot direction)
    }
  }
}