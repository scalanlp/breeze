package breeze.optimize

import breeze.linalg.operators.OpMulMatrix
import breeze.math.{VectorField, MutableVectorField, MutablizingAdaptor}
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
  class FirstOrderOptimizationPackage[DF, K, Vector]()(implicit space: MutableVectorField[Vector, K, Double],
                                                    df: DF <:< DiffFunction[Vector]) extends OptimizationPackage[DF, Vector] {
    def minimize(fn: DF, init: Vector, options: OptimizationOption*):Vector = {
      options.foldLeft(OptParams())( (a,b) => b apply a).minimize(new CachedDiffFunction(fn)(space.copy), init)
    }
  }

  implicit def firstOrderPackage[DF, K, Vector](implicit space: MutableVectorField[Vector, K, Double], df: DF <:< DiffFunction[Vector]) = new FirstOrderOptimizationPackage[DF, K, Vector]()

  class SecondOrderOptimizationPackage[K, Vector, Hessian]()(implicit space: MutableVectorField[Vector, K, Double],
                                                          mult: OpMulMatrix.Impl2[Hessian, Vector, Vector]) extends OptimizationPackage[SecondOrderFunction[Vector, Hessian], Vector] {
    def minimize(fn: SecondOrderFunction[Vector, Hessian], init: Vector, options: OptimizationOption*):Vector = {
      val params = options.foldLeft(OptParams())( (a,b) => b apply a)
      if(params.useL1) throw new UnsupportedOperationException("Can't use L1 with second order optimizer right now")
      val minimizer = new TruncatedNewtonMinimizer[K,Vector,Hessian](params.maxIterations, params.tolerance, params.regularization)
      minimizer.minimize(fn, init)
    }
  }

  implicit def secondOrderPackage[K, Vector, Hessian](implicit space: MutableVectorField[Vector, K, Double],
                                                   mult: OpMulMatrix.Impl2[Hessian, Vector, Vector]) = new SecondOrderOptimizationPackage[K, Vector, Hessian]()

  class FirstOrderStochasticOptimizationPackage[K, Vector]()(implicit space: MutableVectorField[Vector, K, Double]) extends OptimizationPackage[StochasticDiffFunction[Vector], Vector] {
    def minimize(fn: StochasticDiffFunction[Vector], init: Vector, options: OptimizationOption*):Vector = {
      options.foldLeft(OptParams())( (a,b) => b apply a).iterations(fn, init).last.x
    }
  }

  implicit def firstOrderStochasticPackage[K, Vector](implicit space: MutableVectorField[Vector, K, Double]) = new FirstOrderStochasticOptimizationPackage[K,Vector]()

  class FirstOrderBatchOptimizationPackage[K, Vector]()(implicit space: MutableVectorField[Vector, K, Double]) extends OptimizationPackage[BatchDiffFunction[Vector], Vector] {
    def minimize(fn: BatchDiffFunction[Vector], init: Vector, options: OptimizationOption*):Vector = {
      options.foldLeft(OptParams())( (a,b) => b apply a).iterations(new CachedBatchDiffFunction(fn)(space.copy), init).last.x
    }
  }

  implicit def firstOrderBatchPackage[K, Vector](implicit space: MutableVectorField[Vector, K, Double]) = new FirstOrderBatchOptimizationPackage[K, Vector]()
}


trait OptimizationPackageLowPriority {
  class ImmutableFirstOrderOptimizationPackage[DF, K, Vector]()(implicit space: VectorField[Vector, K, Double],
                                                    df: DF <:< DiffFunction[Vector]) extends OptimizationPackage[DF, Vector] {
    def minimize(fn: DF, init: Vector, options: OptimizationOption*):Vector = {
      val mut = MutablizingAdaptor.ensureMutable(space)
      import mut._

      val wrapped = fn.throughLens[Wrapper]

      val res = options.foldLeft(OptParams())( (a,b) => b apply a).minimize(new CachedDiffFunction(wrapped)(mutaVspace.copy), wrap(init))
      unwrap(res)
    }
  }

  implicit def imFirstOrderPackage[DF, K, Vector](implicit space: MutableVectorField[Vector, K, Double], df: DF <:< DiffFunction[Vector])  = new ImmutableFirstOrderOptimizationPackage[DF, K, Vector]()
}