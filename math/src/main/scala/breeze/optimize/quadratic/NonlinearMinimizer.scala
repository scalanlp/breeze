package breeze.optimize.quadratic

import breeze.linalg.{norm, pinv, DenseVector, DenseMatrix}
import breeze.optimize.{LBFGS, DiffFunction}
import breeze.optimize.quadratic.Constraint._
import breeze.stats.distributions.Rand
import scala.math._

/**
 * Created by debasish83 on 12/11/14.
 *
 * Proximal operators and ADMM based primal-dual Nonlinear Solver
 *
 * It solves the problem that has the following structure
 * minimize f(x) + g(x)
 * s.t ax = b
 *
 *
 * We are not yet decided on the exact strategy to handle ax = b. Needs further experimentation
 *
 * g(x) represents the following constraints
 *
 * 1. x >= 0
 * 2. lb <= x <= ub
 * 3. L1(x)
 * 4. Generic regularization on x
 *
 * f(x) can be either a convex or a non-linear function.
 *
 * For convex functions we will use B matrix with 7/10 columns
 *
 * For non-linear functions we will experiment with B matrix with 1 column to mimic a CG solver
 */

class NonlinearMinimizer(ndim: Int,
                         lb: Option[DenseVector[Double]] = None, ub: Option[DenseVector[Double]],
                         Aeq: Option[DenseMatrix[Double]] = None, beq: Option[DenseVector[Double]],
                         maxIter: Int = -1, m: Int = 10, tolerance: Double=1E-9) {
  val alpha: Double = 1.0
  val rho: Double = 1.0

  var solveTime: Long = 0
  var iterations: Long = 0

  val ABSTOL = 1e-8
  val RELTOL = 1e-4
  val EPS = 1e-4

  val z = DenseVector.zeros[Double](ndim)
  val u = DenseVector.zeros[Double](ndim)

  val xHat = DenseVector.zeros[Double](ndim)
  val zOld = DenseVector.zeros[Double](ndim)

  val residual = DenseVector.zeros[Double](ndim)
  val s = DenseVector.zeros[Double](ndim)

  var constraint: Constraint = SMOOTH

  /* If Aeq exists and rows > 1, cache the pseudo-inverse to be used later */
  val invAeq = if (Aeq != None && Aeq.get.rows > 1) Some(pinv(Aeq.get)) else None

  //TO DO : This can take a proximal function as input
  //TO DO : alpha needs to be scaled based on Nesterov's acceleration
  def setProximal(constraint: Constraint): NonlinearMinimizer = {
    this.constraint = constraint
    this
  }

  var lambda: Double = 1.0

  /*Regularization for Elastic Net */
  def setLambda(lambda: Double): NonlinearMinimizer = {
    this.lambda = lambda
    this
  }

  val innerIters = 10

  def solve(primal: DiffFunction[DenseVector[Double]]) : (DenseVector[Double], Boolean) = {
    val init = DenseVector.rand[Double](ndim, Rand.gaussian(0, 1))

    val iters = if (constraint == SMOOTH) maxIter else innerIters

    val lbfgs = new LBFGS[DenseVector[Double]](iters, m, tolerance)

    /* Primal modification
    ADMM-Objective = f(x) + u'(x-z) + rho/2*||x - z||^{2}
     */
    val modifiedPrimal = new DiffFunction[DenseVector[Double]]  {
          def calculate(x: DenseVector[Double]) = {
            val (f, g) = primal.calculate(x)
            val modifiedObj = f // + f-blah
            val modifiedGrad = g // + g-blah
            (modifiedObj, modifiedGrad)
      }
    }

    z := 0.0
    u := 0.0

    residual := 0.0
    s := 0.0

    var k = 0

    if (constraint == SMOOTH) return (lbfgs.minimize(primal, init), true)

    while(k < maxIter) {
      //Try lbfgs.minimize or lbfgsb.minimize
      //Currently breeze does not have support for lbfgsb, for positive/bounds constraint try ProjectedQuasiNewton solver
      val result = lbfgs.minimize(modifiedPrimal, init)

      //z-update with relaxation

      //zold = (1-alpha)*z
      //x_hat = alpha*x + zold
      zOld := z
      zOld *= 1 - alpha

      xHat := result
      xHat *= alpha
      xHat += zOld

      //zold = z
      zOld := z

      //z = xHat + u
      z := xHat
      z += u

      //Apply proximal operator

      //Pick the correct proximal operator based on options
      //We will test the following

      //1. projectPos
      //2. projectBounds
      //3. projectEquality/projectHyperPlane based on the structure of Aeq
      //4. proxL1
      //Other options not tried yet
      //5. proxHuber
      constraint match {
        case POSITIVE => Proximal.projectPos(z.data)
        case BOUNDS => {
          if (lb == None && ub == None)
            throw new IllegalArgumentException("NonlinearMinimizer proximal operator on box needs lower and upper bounds")
          Proximal.projectBox(z.data, lb.get.data, ub.get.data)
        }
        case EQUALITY => {
          if (Aeq == None) throw new IllegalArgumentException("NonlinearMinimizer proximal operator on equality needs Aeq")
          if (beq == None) throw new IllegalArgumentException("NonlinearMinimizer proximal operator on equality needs beq")
          if (Aeq.get.rows > 1) Proximal.projectEquality(z, Aeq.get, invAeq.get, beq.get)
          else Proximal.projectHyperPlane(z, Aeq.get.toDenseVector, beq.get.data(0))
        }
        case SPARSE => Proximal.shrinkage(z.data, lambda / rho)
      }

      //z has proximal(x_hat)

      //Dual (u) update
      xHat -= z
      u += xHat

      //Convergence checks
      //history.r_norm(k)  = norm(x - z)
      residual := result
      residual -= z
      val residualNorm = norm(residual, 2)

      //history.s_norm(k)  = norm(-rho*(z - zold))
      s := z
      s -= zOld
      s *= -rho
      val sNorm = norm(s, 2)

      //TO DO : Make sure z.muli(-1) is actually needed in norm calculation
      residual := z
      residual *= -1.0

      //s = rho*u
      s := u
      s *= rho

      val epsPrimal = sqrt(ndim) * ABSTOL + RELTOL * max(norm(result, 2), norm(residual, 2))
      val epsDual = sqrt(ndim) * ABSTOL + RELTOL * norm(s, 2)

      if (residualNorm < epsPrimal && sNorm < epsDual) {
        iterations += k
        return (result, true)
      }
      k += 1
      init := result
    }
    (init, false)
  }
}