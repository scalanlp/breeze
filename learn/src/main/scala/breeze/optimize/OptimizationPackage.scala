package breeze.optimize

import breeze.math.MutableCoordinateSpace
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.linalg.DenseVector
import breeze.linalg.operators.{OpMulMatrix, BinaryOp}

/**
 *
 * @author dlwh
 */
trait OptimizationPackage[Function, Vector, OptionType] {
  def minimize(fn: Function, init: Vector, options: OptionType*):Vector
}

object OptimizationPackage {
  class FirstOrderOptimizationPackage[Vector]()(implicit coord: MutableCoordinateSpace[Vector, Double]) extends OptimizationPackage[DiffFunction[Vector], Vector, FirstOrderOption] {
    def minimize(fn: DiffFunction[Vector], init: Vector, options: FirstOrderOption*):Vector = {
      options.foldLeft(OptParams())( (a,b) => b apply a).minimize(fn, init)
    }
  }

  implicit def firstOrderPackage[Vector](implicit coord: MutableCoordinateSpace[Vector, Double]) = new FirstOrderOptimizationPackage[Vector]()
  minimize(new DiffFunction[DenseVector[Double]] {
    /** Calculates both the value and the gradient at a point */
    def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = null
  }, DenseVector.zeros[Double](10))

  /*
  class SecondOrderOptimizationPackage[Vector, Hessian]()(implicit coord: MutableCoordinateSpace[Vector, Double],
                                                          mult: BinaryOp[Hessian, Vector, OpMulMatrix, Vector]) extends OptimizationPackage[SecondOrderFunction[Vector, Hessian], Vector, OptParams] {
    def minimize(fn: SecondOrderFunction[Vector, Hessian], init: Vector, options: (OptParams=>OptParams)*):Vector = {
      options.foldLeft(OptParams())( (a,b) => b apply a).minimize(fn, init)
    }
  }

  implicit def secondOrderPackage[Vector, Hessian](implicit coord: MutableCoordinateSpace[Vector, Double],
                                                   mult: BinaryOp[Hessian, Vector, OpMulMatrix, Vector]) = new SecondOrderOptimizationPackage[Vector, Hessian]()
                                                   */
}
