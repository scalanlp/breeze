package breeze.optimize.linear

import breeze.linalg.operators.OpMulMatrix
import breeze.math.MutableInnerProductModule
import breeze.numerics.abs
import breeze.util.SerializableLogging
import breeze.linalg.norm
import breeze.util.Implicits._
/**
 * Power Method to compute maximum eigen value
 * @author debasish83
 */
class PowerMethod[T, M](maxIterations: Int = 10,tolerance: Double = 1E-5)
                       (implicit space: MutableInnerProductModule[T, Double],
                        mult: OpMulMatrix.Impl2[M, T, T]) extends SerializableLogging {

  import space._

  case class State(eigenValue: Double, eigenVector: T, iter: Int, converged: Boolean)

  //memory allocation for the eigen vector result
  def normalize(y: T) : T = {
    val normInit = norm(y)
    val init = copy(y)
    init *= 1.0/normInit
  }

  //in-place modification of eigen vector
  def nextEigen(eigenVector: T, ay: T) = {
    val lambda = eigenVector dot ay
    eigenVector := ay
    val norm1 = norm(ay)
    eigenVector *= 1.0/norm1
    if (lambda < 0.0) eigenVector *= -1.0
    lambda
  }

  def initialState(y: T, A: M): State = {
    val ynorm = normalize(y)
    val ay = mult(A, ynorm)
    val lambda = nextEigen(ynorm, ay)
    State(lambda, ynorm, 0, false)
  }

  def iterations(y: T,
                 A: M): Iterator[State] = Iterator.iterate(initialState(y, A)) { state =>
    import state._
    val ay = mult(A, eigenVector)
    val lambda = nextEigen(eigenVector, ay)
    val val_dif = abs(lambda - eigenValue)
    if (val_dif <= tolerance || iter > maxIterations) State(lambda, eigenVector, iter + 1, true)
    else State(lambda, eigenVector, iter + 1, false)
  }.takeUpToWhere(_.converged)

  def iterateAndReturnState(y: T, A: M): State = {
    iterations(y, A).last
  }

  def eigen(y: T, A: M): Double = {
    iterateAndReturnState(y, A).eigenValue
  }
}
