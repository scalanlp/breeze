package breeze.optimize

import breeze.math.{MutablizingAdaptor, CoordinateSpace, MutableCoordinateSpace}
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
  class FirstOrderOptimizationPackage[DF, Vector]()(implicit coord: MutableCoordinateSpace[Vector, Double],
                                                    df: DF <:< DiffFunction[Vector]) extends OptimizationPackage[DF, Vector] {
    def minimize(fn: DF, init: Vector, options: OptimizationOption*):Vector = {
      options.foldLeft(OptParams())( (a,b) => b apply a).minimize(new CachedDiffFunction(fn)(coord.copy), init)
    }
  }

  implicit def firstOrderPackage[DF, Vector](implicit coord: MutableCoordinateSpace[Vector, Double], df: DF <:< DiffFunction[Vector]) = new FirstOrderOptimizationPackage[DF, Vector]()

  class SecondOrderOptimizationPackage[Vector, Hessian]()(implicit coord: MutableCoordinateSpace[Vector, Double],
                                                          mult: OpMulMatrix.Impl2[Hessian, Vector, Vector]) extends OptimizationPackage[SecondOrderFunction[Vector, Hessian], Vector] {
    def minimize(fn: SecondOrderFunction[Vector, Hessian], init: Vector, options: OptimizationOption*):Vector = {
      val params = options.foldLeft(OptParams())( (a,b) => b apply a)
      if(params.useL1) throw new UnsupportedOperationException("Can't use L1 with second order optimizer right now")
      val minimizer = new TruncatedNewtonMinimizer[Vector,Hessian](params.maxIterations, params.tolerance, params.regularization)
      minimizer.minimize(fn, init)
    }
  }

  implicit def secondOrderPackage[Vector, Hessian](implicit coord: MutableCoordinateSpace[Vector, Double],
                                                   mult: OpMulMatrix.Impl2[Hessian, Vector, Vector]) = new SecondOrderOptimizationPackage[Vector, Hessian]()

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


trait OptimizationPackageLowPriority {
  class ImmutableFirstOrderOptimizationPackage[DF, Vector]()(implicit coord: CoordinateSpace[Vector, Double],
                                                    df: DF <:< DiffFunction[Vector]) extends OptimizationPackage[DF, Vector] {
    def minimize(fn: DF, init: Vector, options: OptimizationOption*):Vector = {
      val mut = MutablizingAdaptor.ensureMutable(coord)
      import mut._

      val wrapped = fn.throughLens[Wrapper]

      val res = options.foldLeft(OptParams())( (a,b) => b apply a).minimize(new CachedDiffFunction(wrapped)(mutaVspace.copy), wrap(init))
      unwrap(res)
    }
  }

  implicit def imFirstOrderPackage[DF, Vector](implicit coord: CoordinateSpace[Vector, Double], df: DF <:< DiffFunction[Vector])  = new ImmutableFirstOrderOptimizationPackage[DF, Vector]()
}