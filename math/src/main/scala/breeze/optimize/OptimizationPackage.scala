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
  def minimize(fn: Function, init: Vector, options: OptimizationOption*): Vector
}

trait IterableOptimizationPackage[Function, Vector, State] extends OptimizationPackage[Function, Vector] {
  def iterations(fn: Function, init: Vector, options: OptimizationOption*): Iterator[State]
}

object OptimizationPackage extends OptimizationPackageLowPriority {

  class SecondOrderOptimizationPackage[Vector, Hessian]()(implicit space: MutableFiniteCoordinateField[Vector, _, Double],
                                                          mult: OpMulMatrix.Impl2[Hessian, Vector, Vector]) extends IterableOptimizationPackage[SecondOrderFunction[Vector, Hessian], Vector, TruncatedNewtonMinimizer[Vector, Hessian]#State] {
    def minimize(fn: SecondOrderFunction[Vector, Hessian], init: Vector, options: OptimizationOption*):Vector = {
      iterations(fn, init, options:_*).last.x
    }

    override def iterations(fn: SecondOrderFunction[Vector, Hessian], init: Vector, options: OptimizationOption*): Iterator[TruncatedNewtonMinimizer[Vector, Hessian]#State] = {
      val params = options.foldLeft(OptParams())( (a,b) => b apply a)
      if(params.useL1) throw new UnsupportedOperationException("Can't use L1 with second order optimizer right now")
      val minimizer = new TruncatedNewtonMinimizer[Vector,Hessian](params.maxIterations, params.tolerance, params.regularization)
      minimizer.iterations(fn, init)
    }
  }

  implicit def secondOrderPackage[Vector, Hessian](implicit space: MutableFiniteCoordinateField[Vector, _, Double],
                                                   mult: OpMulMatrix.Impl2[Hessian, Vector, Vector]) = new SecondOrderOptimizationPackage[Vector, Hessian]()

  class FirstOrderStochasticOptimizationPackage[Vector]()(implicit space: MutableFiniteCoordinateField[Vector, _, Double]) extends IterableOptimizationPackage[StochasticDiffFunction[Vector], Vector, FirstOrderMinimizer[Vector, StochasticDiffFunction[Vector]]#State] {
    def minimize(fn: StochasticDiffFunction[Vector], init: Vector, options: OptimizationOption*):Vector = {
      iterations(fn, init, options:_*).last.x
    }

    override def iterations(fn: StochasticDiffFunction[Vector], init: Vector, options: OptimizationOption*): Iterator[FirstOrderMinimizer[Vector, StochasticDiffFunction[Vector]]#State] = {
      options.foldLeft(OptParams())( (a,b) => b apply a).iterations(fn, init)
    }
  }

  implicit def firstOrderStochasticPackage[Vector](implicit space: MutableFiniteCoordinateField[Vector, _, Double]): FirstOrderStochasticOptimizationPackage[Vector] = {
    new FirstOrderStochasticOptimizationPackage[Vector]()
  }

  class FirstOrderBatchOptimizationPackage[Vector]()(implicit space: MutableFiniteCoordinateField[Vector, _, Double]) extends IterableOptimizationPackage[BatchDiffFunction[Vector], Vector, FirstOrderMinimizer[Vector, BatchDiffFunction[Vector]]#State] {
    def minimize(fn: BatchDiffFunction[Vector], init: Vector, options: OptimizationOption*):Vector = {
      iterations(fn, init, options:_*).last.x
    }

    override def iterations(fn: BatchDiffFunction[Vector], init: Vector, options: OptimizationOption*): Iterator[FirstOrderMinimizer[Vector, BatchDiffFunction[Vector]]#State] = {
      options.foldLeft(OptParams())( (a,b) => b apply a).iterations(new CachedBatchDiffFunction(fn)(space.copy), init)
    }
  }

  implicit def firstOrderBatchPackage[Vector](implicit space: MutableFiniteCoordinateField[Vector, _, Double]): FirstOrderBatchOptimizationPackage[Vector] = {
    new FirstOrderBatchOptimizationPackage[Vector]()
  }
}

sealed trait OptimizationPackageLowPriority2 {


  class ImmutableFirstOrderOptimizationPackage[DF, Vector]()(implicit space: CoordinateField[Vector, Double],
                                                             canIterate: CanTraverseValues[Vector, Double],
                                                             canMap: CanMapValues[Vector, Double, Double, Vector],
                                                             canZipMap: CanZipMapValues[Vector, Double, Double, Vector],
                                                             df: DF <:< DiffFunction[Vector]) extends OptimizationPackage[DF, Vector] {
    def minimize(fn: DF, init: Vector, options: OptimizationOption*): Vector = {
      val mut = MutablizingAdaptor.ensureMutable(space)
      import mut._

      val wrapped = fn.throughLens[Wrapper]

      val params: OptParams = options.foldLeft(OptParams())((a, b) => b apply a)
      require(!params.useL1, "Sorry, we can't use L1 with immutable objects right now...")
      val lbfgs: LBFGS[Wrapper] = new LBFGS[Wrapper](tolerance = params.tolerance, maxIter = params.maxIterations)
      val res = lbfgs.minimize(DiffFunction.withL2Regularization(wrapped, params.regularization), wrap(init))
      unwrap(res)
    }
  }

  implicit def imFirstOrderPackage[DF, Vector](implicit space: CoordinateField[Vector, Double],
                                               canIterate: CanTraverseValues[Vector, Double],
                                               canMap: CanMapValues[Vector, Double, Double, Vector],
                                               canZipMap: CanZipMapValues[Vector, Double, Double, Vector],
                                               df: DF <:< DiffFunction[Vector]): ImmutableFirstOrderOptimizationPackage[DF, Vector] = {
    new ImmutableFirstOrderOptimizationPackage[DF, Vector]()
  }

}

sealed trait OptimizationPackageLowPriority extends OptimizationPackageLowPriority2 {

  class LBFGSMinimizationPackage[DF, Vector]()(implicit space: MutableEnumeratedCoordinateField[Vector, _, Double],
                                               df: DF <:< DiffFunction[Vector]) extends IterableOptimizationPackage[DF, Vector, LBFGS[Vector]#State] {
    def minimize(fn: DF, init: Vector, options: OptimizationOption*):Vector = {
      iterations(fn, init, options:_*).last.x
    }

    override def iterations(fn: DF, init: Vector, options: OptimizationOption*): Iterator[LBFGS[Vector]#State] = {
      options.foldLeft(OptParams())( (a,b) => b apply a).iterations(new CachedDiffFunction(fn)(space.copy), init)
    }
  }

  implicit def lbfgsMinimizationPackage[DF, Vector](implicit space: MutableFiniteCoordinateField[Vector, _, Double], df: DF <:< DiffFunction[Vector]): LBFGSMinimizationPackage[DF, Vector] = {
    new LBFGSMinimizationPackage[DF, Vector]()
  }
}
