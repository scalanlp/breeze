package breeze.optimize.linear

import breeze.linalg.operators.OpMulMatrix
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.MutableInnerProductModule
import breeze.numerics._
import breeze.optimize.proximal.QuadraticMinimizer
import breeze.util.Implicits._

/**
 * Inverse Power Method specialized on DenseVector[Double], DenseMatrix[Double]
 * For usage see breeze.optimize.proximal.QuadraticMinimizer rho computation
 * @author debasish83
 */
class InversePowerMethod(maxIterations: Int = 10, tolerance: Double = 1E-5)
                        (implicit space: MutableInnerProductModule[DenseVector[Double], Double],
                         mult: OpMulMatrix.Impl2[DenseMatrix[Double], DenseVector[Double], DenseVector[Double]])
  extends PowerMethod[DenseVector[Double], DenseMatrix[Double]] {

  override def initialState(y: DenseVector[Double], A: DenseMatrix[Double]): State = {
    val ynorm = normalize(y)
    val ay = QuadraticMinimizer.solveTriangular(A, ynorm)
    val lambda = nextEigen(ynorm, ay)
    State(lambda, ynorm, 0, false)
  }

  override def iterations(y: DenseVector[Double],
                          A: DenseMatrix[Double]): Iterator[State] = Iterator.iterate(initialState(y, A)) { state =>
    import state._
    val ay = QuadraticMinimizer.solveTriangular(A, eigenVector)
    val lambda = nextEigen(eigenVector, ay)
    val val_dif = abs(lambda - eigenValue)
    if (val_dif <= tolerance || iter > maxIterations) State(lambda, eigenVector, iter + 1, true)
    else State(lambda, eigenVector, iter + 1, false)
  }.takeUpToWhere(_.converged)
}
