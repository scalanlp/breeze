package breeze.optimize

import breeze.linalg.operators.OpMulMatrix
import breeze.linalg.support.{CanMapValues, CanTraverseValues, CanZipMapValues}
import breeze.math._
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.util.Implicits._

/**
 *
 * @author dlwh
 */
trait OptimizationPackage[Function, Vector] {
  def minimize(fn: Function, init: Vector, options: OptimizationOption*):Vector
}

object OptimizationPackage {
  class FirstOrderOptimizationPackage[DF, Vector]()(implicit space: MutableFiniteCoordinateField[Vector, _, Double],
                                                    df: DF <:< DiffFunction[Vector]) extends OptimizationPackage[DF, Vector] {
    def minimize(fn: DF, init: Vector, options: OptimizationOption*):Vector = {
      options.foldLeft(OptParams())( (a,b) => b apply a).minimize(new CachedDiffFunction(fn)(space.copy), init)
    }
  }

  implicit def firstOrderPackage[DF, Vector](implicit space: MutableFiniteCoordinateField[Vector, _, Double], df: DF <:< DiffFunction[Vector]) = new FirstOrderOptimizationPackage[DF, Vector]()

  class SecondOrderOptimizationPackage[Vector, Hessian]()(implicit space: MutableFiniteCoordinateField[Vector, _, Double],
                                                          mult: OpMulMatrix.Impl2[Hessian, Vector, Vector]) extends OptimizationPackage[SecondOrderFunction[Vector, Hessian], Vector] {
    def minimize(fn: SecondOrderFunction[Vector, Hessian], init: Vector, options: OptimizationOption*):Vector = {
      val params = options.foldLeft(OptParams())( (a,b) => b apply a)
      if(params.useL1) throw new UnsupportedOperationException("Can't use L1 with second order optimizer right now")
      val minimizer = new TruncatedNewtonMinimizer[Vector,Hessian](params.maxIterations, params.tolerance, params.regularization)
      minimizer.minimize(fn, init)
    }
  }

  implicit def secondOrderPackage[Vector, Hessian](implicit space: MutableFiniteCoordinateField[Vector, _, Double],
                                                   mult: OpMulMatrix.Impl2[Hessian, Vector, Vector]) = new SecondOrderOptimizationPackage[Vector, Hessian]()

  class FirstOrderStochasticOptimizationPackage[Vector]()(implicit space: MutableFiniteCoordinateField[Vector, _, Double]) extends OptimizationPackage[StochasticDiffFunction[Vector], Vector] {
    def minimize(fn: StochasticDiffFunction[Vector], init: Vector, options: OptimizationOption*):Vector = {
      options.foldLeft(OptParams())( (a,b) => b apply a).iterations(fn, init).last.x
    }
  }

  implicit def firstOrderStochasticPackage[Vector](implicit space: MutableFiniteCoordinateField[Vector, _, Double]) = new FirstOrderStochasticOptimizationPackage[Vector]()

  class FirstOrderBatchOptimizationPackage[Vector]()(implicit space: MutableFiniteCoordinateField[Vector, _, Double]) extends OptimizationPackage[BatchDiffFunction[Vector], Vector] {
    def minimize(fn: BatchDiffFunction[Vector], init: Vector, options: OptimizationOption*):Vector = {
      options.foldLeft(OptParams())( (a,b) => b apply a).iterations(new CachedBatchDiffFunction(fn)(space.copy), init).last.x
    }
  }

  implicit def firstOrderBatchPackage[Vector](implicit space: MutableFiniteCoordinateField[Vector, _, Double]) = new FirstOrderBatchOptimizationPackage[Vector]()
}


trait OptimizationPackageLowPriority {

  class ImmutableFirstOrderOptimizationPackage[DF, Vector]()(implicit space: CoordinateField[Vector, Double],
                                                             canIterate: CanTraverseValues[Vector, Double],
                                                             canMap: CanMapValues[Vector, Double, Double, Vector],
                                                             canZipMap: CanZipMapValues[Vector, Double, Double, Vector],
                                                             df: DF <:< DiffFunction[Vector]) extends OptimizationPackage[DF, Vector] {
    def minimize(fn: DF, init: Vector, options: OptimizationOption*): Vector = {
      val mut = MutablizingAdaptor.ensureMutable(space)
      import mut._

      val wrapped = fn.throughLens[Wrapper]

      val res = options.foldLeft(OptParams())( (a,b) => b apply a).minimize(new CachedDiffFunction(wrapped)(mutaVspace.copy), wrap(init))
      unwrap(res)
    }
  }

  implicit def imFirstOrderPackage[DF, Vector](implicit space: CoordinateField[Vector, Double],
                                               canIterate: CanTraverseValues[Vector, Double],
                                               canMap: CanMapValues[Vector, Double, Double, Vector],
                                               canZipMap: CanZipMapValues[Vector, Double, Double, Vector],df: DF <:< DiffFunction[Vector])  = new ImmutableFirstOrderOptimizationPackage[DF, Vector]()
}