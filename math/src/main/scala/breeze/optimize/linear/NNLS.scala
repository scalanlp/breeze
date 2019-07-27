package breeze.optimize.linear

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.distributions.Rand
import breeze.util.SerializableLogging
import spire.syntax.cfor._
import breeze.optimize.proximal.QpGenerator
import breeze.optimize.proximal.QuadraticMinimizer.gemv
import breeze.linalg.axpy

/**
 * NNLS solves nonnegative least squares problems using a modified
 * projected gradient method.
 * @param maxIters user defined maximum iterations
 * @author tmyklebu debasish83
 */
class NNLS(val maxIters: Int = -1) extends SerializableLogging {
  type BDM = DenseMatrix[Double]
  type BDV = DenseVector[Double]

  case class State private[NNLS] (
      x: BDV,
      grad: BDV,
      dir: BDV,
      lastDir: BDV,
      res: BDV,
      tmp: BDV,
      lastNorm: Double,
      lastWall: Int,
      iter: Int,
      converged: Boolean)

  // find the optimal unconstrained step
  private def steplen(ata: BDM, dir: BDV, res: BDV, tmp: BDV): Double = {
    val top = dir.dot(res)
    gemv(1.0, ata, dir, 0.0, tmp)
    // Push the denominator upward very slightly to avoid infinities and silliness
    top / (tmp.dot(dir) + 1e-20)
  }

  // stopping condition
  private def stop(step: Double, ndir: Double, nx: Double): Boolean = {
    ((step.isNaN) // NaN
    || (step < 1e-7) // too small or negative
    || (step > 1e40) // too small; almost certainly numerical problems
    || (ndir < 1e-12 * nx) // gradient relatively too small
    || (ndir < 1e-32) // gradient absolutely too small; numerical issues may lurk
    )
  }

  /**
   * Solve a least squares problem, possibly with nonnegativity constraints, by a modified
   * projected gradient method.  That is, find x minimising ||Ax - b||_2 given A^T A and A^T b.
   *
   * We solve the problem
   * min_x      1/2 x' ata x' - x'atb
   * subject to x >= 0
   *
   * The method used is similar to one described by Polyak (B. T. Polyak, The conjugate gradient
   * method in extremal problems, Zh. Vychisl. Mat. Mat. Fiz. 9(4)(1969), pp. 94-112) for bound-
   * constrained nonlinear programming.  Polyak unconditionally uses a conjugate gradient
   * direction, however, while this method only uses a conjugate gradient direction if the last
   * iteration did not cause a previously-inactive constraint to become active.
   */
  def initialize(n: Int): State = {
    val grad = DenseVector.zeros[Double](n)
    val x = DenseVector.zeros[Double](n)
    val dir = DenseVector.zeros[Double](n)
    val lastDir = DenseVector.zeros[Double](n)
    val res = DenseVector.zeros[Double](n)
    val tmp = DenseVector.zeros[Double](n)
    val lastNorm = 0.0
    val lastWall = 0
    State(x, grad, dir, lastDir, res, tmp, lastNorm, lastWall, 0, false)
  }

  def reset(ata: DenseMatrix[Double], atb: DenseVector[Double], state: State) = {
    import state._
    require(ata.cols == ata.rows, s"NNLS:iterations gram matrix must be symmetric")
    require(ata.rows == state.x.length, s"NNLS:iterations gram and linear dimension mismatch")
    x := 0.0
    grad := 0.0
    dir := 0.0
    lastDir := 0.0
    res := 0.0
    tmp := 0.0
    State(x, grad, dir, lastDir, res, tmp, 0.0, 0, 0, false)
  }

  /**
   * minimizeAndReturnState allows users to hot start the solver using initialState. If a initialState is provided and
   * resetState is set to false, the optimizer will hot start using the previous state. By default resetState is
   * true and every time reset will be called on the incoming state
   *
   * @param ata gram matrix
   * @param atb linear term
   * @param initialState initial state for calling the solver from inner loops
   * @param resetState reset the state based on the flag
   * @return converged state
   */
  def minimizeAndReturnState(
      ata: DenseMatrix[Double],
      atb: DenseVector[Double],
      initialState: State,
      resetState: Boolean = true): State = {
    val startState = if (resetState) reset(ata, atb, initialState) else initialState
    import startState._
    val n = atb.length
    val iterMax = if (maxIters < 0) Math.max(400, 20 * n) else maxIters

    var nextNorm = lastNorm
    var nextWall = lastWall
    var nextIter = 0

    while (nextIter <= iterMax) {
      // find the residual
      gemv(1.0, ata, x, 0.0, res)
      axpy(-1.0, atb, res)
      grad := res

      // project the gradient
      cforRange(0 until n) { i =>
        if (grad(i) > 0.0 && x(i) == 0.0) {
          grad(i) = 0.0
        }
      }

      val ngrad = grad.dot(grad)
      dir := grad

      // use a CG direction under certain conditions
      var step = steplen(ata, grad, res, tmp)
      var ndir = 0.0
      val nx = x.dot(x)

      if (nextIter > nextWall + 1) {
        val alpha = ngrad / nextNorm
        axpy(alpha, lastDir, dir)
        val dstep = steplen(ata, dir, res, tmp)
        ndir = dir.dot(dir)
        if (stop(dstep, ndir, nx)) {
          // reject the CG step if it could lead to premature termination
          dir := grad
          ndir = dir.dot(dir)
        } else {
          step = dstep
        }
      } else {
        ndir = dir.dot(dir)
      }

      // terminate?
      if (stop(step, ndir, nx)) {
        return State(x, grad, dir, lastDir, res, tmp, nextNorm, nextWall, nextIter, true)
      } else {
        // don't run through the walls
        cforRange(0 until n) { i =>
          if (step * dir(i) > x(i)) {
            step = x(i) / dir(i)
          }
        }
        // take the step
        cforRange(0 until n) { i =>
          if (step * dir(i) > x(i) * (1 - 1e-14)) {
            x(i) = 0
            nextWall = nextIter
          } else {
            x(i) -= step * dir(i)
          }
        }
        lastDir := dir
        nextNorm = ngrad
      }
      nextIter += 1
    }
    State(x, grad, dir, lastDir, res, tmp, nextNorm, nextWall, nextIter, false)
  }

  def minimizeAndReturnState(ata: DenseMatrix[Double], atb: DenseVector[Double]): State = {
    val initialState = initialize(atb.length)
    minimizeAndReturnState(ata, atb, initialState)
  }

  def minimize(ata: DenseMatrix[Double], atb: DenseVector[Double]): DenseVector[Double] = {
    minimizeAndReturnState(ata, atb).x
  }

  def minimize(ata: DenseMatrix[Double], atb: DenseVector[Double], init: State): DenseVector[Double] = {
    minimizeAndReturnState(ata, atb, init).x
  }
}

object NNLS {

  /** Compute the objective value */
  def computeObjectiveValue(ata: DenseMatrix[Double], atb: DenseVector[Double], x: DenseVector[Double]): Double = {
    val res = (x.t * ata * x) * 0.5 - atb.dot(x)
    res
  }

  def apply(iters: Int) = new NNLS(iters)

  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("Usage: NNLS n s")
      println("Test NNLS with quadratic function of dimension n for s consecutive solves")
      sys.exit(1)
    }

    val problemSize = args(0).toInt
    val numSolves = args(1).toInt
    val nnls = new NNLS()

    var i = 0
    var nnlsTime = 0L
    while (i < numSolves) {
      val ata = QpGenerator.getGram(problemSize)
      val atb = DenseVector.rand[Double](problemSize, Rand.gaussian(0, 1))
      val startTime = System.nanoTime()
      nnls.minimize(ata, atb)
      nnlsTime = nnlsTime + (System.nanoTime() - startTime)
      i = i + 1
    }
    println(s"NNLS problemSize $problemSize solves $numSolves ${nnlsTime / 1e6} ms")
  }
}
