/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"), you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package breeze.optimize.quadratic

import breeze.linalg.{norm, DenseVector}
import breeze.optimize.quadratic.Constraint._
import breeze.optimize.{LBFGS, DiffFunction}
import breeze.stats.distributions.Rand
import scala.math._

/**
 * Created by debasish83 on 12/11/14.
 *
 * Proximal operators and ADMM based primal-dual Nonlinear Solver
 *
 * It solves the problem that has the following structure
 * minimize f(x) + g(x)
 *
 *
 * g(x) represents the following constraints
 *
 * 1. x >= 0
 * 2. lb <= x <= ub
 * 3. L1(x)
 * 4. Aeq*x = beq
 * 5. aeq'x = beq
 * 6. 1'x = 1, x >= 0 which is called ProbabilitySimplex from the following reference
 *    Proximal Algorithms by Boyd et al.
 *
 * f(x) can be either a convex or a non-linear function.
 *
 * For convex functions we will use B matrix with 7/10 columns
 *
 * For non-linear functions we will experiment with B matrix with 1 column to mimic a CG solver
 */

class NonlinearMinimizer(ndim: Int, maxIters: Int = -1, m: Int = 10, tolerance: Double=1E-9) {
  type BDV = DenseVector[Double]

  case class State(x: BDV, u: BDV, z: BDV, iterations: Int, converged: Boolean)

  /*
    Proximal modifications to Primal algorithm
    AdmmObj(x, u, z) = f(x) + u'(x-z) + rho/2*||x - z||^{2}
    dAdmmObj/dx = df/dx + u' + rho(x - z)
  */
  case class ProximalPrimal(primal: DiffFunction[DenseVector[Double]],
                            u: BDV, z: BDV,
                            rho: Double) extends DiffFunction[DenseVector[Double]] {
    override def calculate(x: DenseVector[Double]) = {
      val (f, g) = primal.calculate(x)
      val proxObj = f + u.dot(x - z) + 0.5 * rho * norm(x - z, 2)
      val proxGrad = g + u + (x - z):*rho
      (proxObj, proxGrad)
    }
  }

  val alpha: Double = 1.0
  val rho: Double = 1.0

  val ABSTOL = 1e-8
  val RELTOL = 1e-4

  var proximal: Proximal = null

  //TO DO : This can take a proximal function as input
  //TO DO : alpha needs to be scaled based on Nesterov's acceleration
  def setProximal(proximal: Proximal): NonlinearMinimizer = {
    this.proximal = proximal
    this
  }

  var lambda: Double = 1.0

  /*Regularization for Elastic Net */
  def setLambda(lambda: Double): NonlinearMinimizer = {
    this.lambda = lambda
    this
  }

  val innerIters = 10

  def iterations(primal: DiffFunction[DenseVector[Double]]) : State = {

    val iters = if (proximal == null) maxIters else innerIters
    val lbfgs = new LBFGS[DenseVector[Double]](iters, m, tolerance)
    val init = DenseVector.rand[Double](ndim, Rand.gaussian(0, 1))

    val z = DenseVector.zeros[Double](ndim)
    val u = DenseVector.zeros[Double](ndim)

    if (proximal == null) return State(lbfgs.minimize(primal, init), u, z, 0, true)

    val proxPrimal = ProximalPrimal(primal, u, z, rho)

    val xHat = DenseVector.zeros[Double](ndim)
    val zOld = DenseVector.zeros[Double](ndim)

    val residual = DenseVector.zeros[Double](ndim)
    val s = DenseVector.zeros[Double](ndim)

    var k = 0

    while(k < maxIters) {
      val result = lbfgs.minimize(proxPrimal, init)
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
      proximal.prox(z, lambda/rho)

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
        State(result, u, z, k, true)
      }
      k += 1
      init := result
    }
    State(init, u, z, maxIters, false)
  }

  def minimize(primal: DiffFunction[DenseVector[Double]]) : DenseVector[Double] = {
    iterations(primal).x
  }
}

object NonlinearMinimizer {
  def apply(ndim: Int, constraint: Constraint, lambda: Double): NonlinearMinimizer = {
    val minimizer = new NonlinearMinimizer(ndim)
    constraint match {
      case POSITIVE => minimizer.setProximal(ProjectPos())
      case BOUNDS => {
        val lb = DenseVector.zeros[Double](ndim)
        val ub = DenseVector.ones[Double](ndim)
        minimizer.setProximal(ProjectBox(lb, ub))
      }
      case EQUALITY => {
        val aeq = DenseVector.ones[Double](ndim)
        val beq = 1.0
        minimizer.setProximal(ProjectHyperPlane(aeq, beq))
      }
      case SPARSE => minimizer.setProximal(ProximalL1())
      //TO DO: ProximalSimplex : for PLSA
    }
  }

  def main(args: Array[String]) {
    ???
  }
}