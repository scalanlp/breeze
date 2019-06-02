package breeze.optimize

/*
 Copyright 2015 David Hall, Debasish Das

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

import breeze.linalg._
import breeze.collection.mutable.RingBuffer
import breeze.math.MutableInnerProductModule
import breeze.optimize.FirstOrderMinimizer.{ConvergenceCheck, ConvergenceReason}
import breeze.util.SerializableLogging

// Compact representation of an n x n Hessian, maintained via L-BFGS updates
class CompactHessian(
    M: DenseMatrix[Double],
    Y: RingBuffer[DenseVector[Double]],
    S: RingBuffer[DenseVector[Double]],
    sigma: Double,
    m: Int)
    extends NumericOps[CompactHessian] {
  def this(m: Int) = this(null, new RingBuffer(m), new RingBuffer(m), 1.0, m)
  def repr: CompactHessian = this
  implicit def collectionOfVectorsToMatrix(coll: scala.collection.Seq[DenseVector[Double]]) =
    DenseMatrix.tabulate(coll.size, coll.headOption.map(_.size).getOrElse(0)) { case (i, j) => coll(i)(j) }
  def updated(y: DenseVector[Double], s: DenseVector[Double]): CompactHessian = {
    // Compute scaling factor for initial Hessian, which we choose as
    val yTs = y.dot(s)

    if (yTs < 1e-10) // ensure B remains strictly positive definite
      return this

    val S = this.S :+ s
    val Y = this.Y :+ y
    val sigma = y.dot(y) / yTs

    val k = Y.size

    // D_k is the k x k diagonal matrix D_k = diag [s_0^Ty_0, ...,s_{k-1}^Ty_{k-1}].
    // L_k is the k x k matrix with (L_k)_{i,j} = if( i > j ) s_i^T y_j else 0
    // (this is a lower triangular matrix with the diagonal set to all zeroes)
    val D = diag(DenseVector.tabulate[Double](k) { i =>
      S(i).dot(Y(i))
    })
    val L = DenseMatrix.tabulate[Double](k, k) { (i, j) =>
      if (i > j) {
        S(i).dot(Y(j))
      } else {
        0.0
      }
    }
    val SM = collectionOfVectorsToMatrix(S)
    // S_k^T S_k is the symmetric k x k matrix with element (i,j) given by <s_i, s_j>
    val STS = (SM * SM.t) * sigma

    // M is the 2k x 2k matrix given by: M = [ \sigma * S_k^T S_k    L_k ]
    //                                       [         L_k^T        -D_k ]
    val M = DenseMatrix.vertcat(DenseMatrix.horzcat(STS, L), DenseMatrix.horzcat(L.t, -D))

    val newB = new CompactHessian(M, Y, S, sigma, m)
    newB
  }

  def *(v: DenseVector[Double]): DenseVector[Double] = {
    if (Y.size == 0) {
      v
    } else {
      val nTv = N.t * v.toDenseMatrix.t
      val u = (N * (M \ nTv)).toDenseVector
      v * sigma - u
    }
  }
  lazy val N = DenseMatrix.horzcat(collectionOfVectorsToMatrix(S).t * sigma, collectionOfVectorsToMatrix(Y).t)
}

class ProjectedQuasiNewton(
    convergenceCheck: ConvergenceCheck[DenseVector[Double]],
    val innerOptimizer: SpectralProjectedGradient[DenseVector[Double]],
    val m: Int,
    val initFeas: Boolean,
    val testOpt: Boolean,
    val maxSrchIt: Int,
    val gamma: Double,
    val projection: DenseVector[Double] => DenseVector[Double])(
    implicit space: MutableInnerProductModule[DenseVector[Double], Double])
    extends FirstOrderMinimizer[DenseVector[Double], DiffFunction[DenseVector[Double]]](convergenceCheck)
    with Projecting[DenseVector[Double]]
    with SerializableLogging {
  type BDV = DenseVector[Double]
  def this(
      tolerance: Double = 1e-6,
      m: Int = 10,
      initFeas: Boolean = false,
      testOpt: Boolean = true,
      maxIter: Int = -1,
      maxSrchIt: Int = 50,
      gamma: Double = 1e-4,
      projection: DenseVector[Double] => DenseVector[Double] = identity,
      relativeTolerance: Boolean = true)(implicit space: MutableInnerProductModule[DenseVector[Double], Double]) = this(
    convergenceCheck =
      FirstOrderMinimizer.defaultConvergenceCheck[DenseVector[Double]](maxIter, tolerance, relativeTolerance),
    m = m,
    initFeas = initFeas,
    testOpt = testOpt,
    maxSrchIt = maxSrchIt,
    gamma = gamma,
    projection = projection,
    innerOptimizer = new SpectralProjectedGradient[DenseVector[Double]](
      tolerance = tolerance,
      maxIter = 50,
      bbMemory = 5,
      initFeas = true,
      fvalMemory = 10,
      projection = projection
    )
  )

  type History = CompactHessian

  protected def initialHistory(f: DiffFunction[DenseVector[Double]], init: DenseVector[Double]): History = {
    new CompactHessian(m)
  }

  override protected def adjust(
      newX: DenseVector[Double],
      newGrad: DenseVector[Double],
      newVal: Double): (Double, DenseVector[Double]) = (newVal, projectedVector(newX, -newGrad))

  private def computeGradient(x: DenseVector[Double], g: DenseVector[Double]): DenseVector[Double] =
    projectedVector(x, -g)

  protected def chooseDescentDirection(state: State, fn: DiffFunction[DenseVector[Double]]): DenseVector[Double] = {
    import state._
    if (iter == 0) {
      computeGradient(x, grad)
    } else {
      // Update the limited-memory BFGS approximation to the Hessian
      //B.update(y, s)
      // Solve subproblem; we use the current iterate x as a guess
      val subprob = new ProjectedQuasiNewton.QuadraticSubproblem(state.adjustedValue, x, grad, history)
      val spgResult = innerOptimizer.minimizeAndReturnState(new CachedDiffFunction(subprob), x)
      logger.info(f"ProjectedQuasiNewton: outerIter ${state.iter} innerIters ${spgResult.iter}")
      spgResult.x - x
      //	time += subprob.time
    }
  }

  /**
   * Given a direction, perform a Strong Wolfe Line Search
   *
   * TO DO: Compare performance with Cubic Interpolation based line search from Mark's PQN paper
   *
   * @param state the current state
   * @param f The objective
   * @param dir The step direction
   * @return stepSize
   */
  protected def determineStepSize(state: State, f: DiffFunction[DenseVector[Double]], dir: DenseVector[Double]) = {
    val x = state.x
    val grad = state.grad

    val ff = LineSearch.functionFromSearchDirection(f, x, dir)
    val search =
      new BacktrackingLineSearch(state.value, maxIterations = maxSrchIt, shrinkStep = if (state.iter < 1) 0.1 else 0.5)
    var alpha = if (state.iter == 0.0) min(1.0, 1.0 / norm(dir)) else 1.0
    alpha = search.minimize(ff, alpha)

    if (alpha * norm(grad) < 1E-10) throw new StepSizeUnderflow

    alpha
  }

  protected def takeStep(state: State, dir: DenseVector[Double], stepSize: Double): DenseVector[Double] = {
    projection(state.x + dir * stepSize)
  }

  protected def updateHistory(
      newX: DenseVector[Double],
      newGrad: DenseVector[Double],
      newVal: Double,
      f: DiffFunction[DenseVector[Double]],
      oldState: State): History = {
    import oldState._
    val s = newX - x
    val y = newGrad - grad
    oldState.history.updated(y, s)
  }
}

object ProjectedQuasiNewton extends SerializableLogging {
  // Forms a quadratic model around fun, the argmin of which is then a feasible
  // quasi-Newton descent direction
  class QuadraticSubproblem(fk: Double, xk: DenseVector[Double], gk: DenseVector[Double], B: CompactHessian)
      extends DiffFunction[DenseVector[Double]] {

    /**
     * Return value and gradient of the quadratic model at the current iterate:
     *  q_k(p)        = f_k + (p-x_k)^T g_k + 1/2 (p-x_k)^T B_k(p-x_k)
     *  \nabla q_k(p) = g_k + B_k(p-x_k)
     */
    override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
      val d = x - xk
      val Bd = B * d
      val f = fk + d.dot(gk) + (0.5 * d.dot(Bd))
      val g = gk + Bd
      (f, g)
    }
  }
}
