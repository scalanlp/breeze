package breeze.optimize

import breeze.math.MutableCoordinateSpace
import breeze.util.Implicits._
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.linalg.DenseVector
import breeze.linalg.operators.{OpMulMatrix, BinaryOp}
import breeze.linalg.support.CanCopy

/**
 *
 * @author dlwh
 */
trait OptimizationPackage[Function, Vector] {
  def minimize(fn: Function, init: Vector, options: OptimizationOption*):Vector
}

object OptimizationPackage {
  class FirstOrderOptimizationPackage[DF, Vector]()(implicit coord: MutableCoordinateSpace[Vector, Double], df: DF <:< DiffFunction[Vector]) extends OptimizationPackage[DF, Vector] {
    def minimize(fn: DF, init: Vector, options: OptimizationOption*):Vector = {
      options.foldLeft(OptParams())( (a,b) => b apply a).minimize(new CachedDiffFunction(fn)(coord.copy), init)
    }
  }

  implicit def firstOrderPackage[DF, Vector](implicit coord: MutableCoordinateSpace[Vector, Double], df: DF <:< DiffFunction[Vector]) = new FirstOrderOptimizationPackage[DF, Vector]()

  class SecondOrderOptimizationPackage[Vector, Hessian]()(implicit coord: MutableCoordinateSpace[Vector, Double],
                                                          mult: BinaryOp[Hessian, Vector, OpMulMatrix, Vector]) extends OptimizationPackage[SecondOrderFunction[Vector, Hessian], Vector] {
    def minimize(fn: SecondOrderFunction[Vector, Hessian], init: Vector, options: OptimizationOption*):Vector = {
      val params = options.foldLeft(OptParams())( (a,b) => b apply a)
      if(params.useL1) throw new UnsupportedOperationException("Can't use L1 with second order optimizer right now")
      val minimizer = new TruncatedNewtonMinimizer[Vector,Hessian](params.maxIterations, params.tolerance, params.regularization)
      minimizer.minimize(fn, init)
    }
  }

  implicit def secondOrderPackage[Vector, Hessian](implicit coord: MutableCoordinateSpace[Vector, Double],
                                                   mult: BinaryOp[Hessian, Vector, OpMulMatrix, Vector]) = new SecondOrderOptimizationPackage[Vector, Hessian]()

  class FirstOrderStochasticOptimizationPackage[Vector]()(implicit coord: MutableCoordinateSpace[Vector, Double]) extends OptimizationPackage[StochasticDiffFunction[Vector], Vector] {
    def minimize(fn: StochasticDiffFunction[Vector], init: Vector, options: OptimizationOption*):Vector = {
      options.foldLeft(OptParams())( (a,b) => b apply a).iterations(fn, init).last.x
    }
  }

  implicit def firstOrderStochasticPackage[Vector](implicit coord: MutableCoordinateSpace[Vector, Double]) = new FirstOrderStochasticOptimizationPackage[Vector]()

  class FirstOrderBatchOptimizationPackage[Vector]()(implicit coord: MutableCoordinateSpace[Vector, Double]) extends OptimizationPackage[BatchDiffFunction[Vector], Vector] {
    def minimize(fn: BatchDiffFunction[Vector], init: Vector, options: OptimizationOption*):Vector = {
      options.foldLeft(OptParams())( (a,b) => b apply a).iterations(new CachedBatchDiffFunction(fn)(coord.copy), init).last.x
    }
  }

  implicit def firstOrderBatchPackage[Vector](implicit coord: MutableCoordinateSpace[Vector, Double]) = new FirstOrderBatchOptimizationPackage[Vector]()
}
