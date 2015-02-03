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

package breeze.optimize.proximal

import breeze.linalg.{DenseMatrix, DenseVector, norm}
import breeze.numerics._
import breeze.optimize.{DiffFunction, LBFGS}
import breeze.stats.distributions.Rand
import breeze.optimize.proximal.Constraint._
import scala.math._
import scala.math.pow
import scala.math.sqrt

/**
 * Created by debasish83 on 12/11/14.
 *
 * Proximal operators and ADMM based primal-dual Nonlinear Solver
 *
 * It solves the problem that has the following structure
 * minimize f(x) + g(x)
 *
 * g(x) represents the following constraints
 *
 * 1. x >= 0
 * 2. lb <= x <= ub
 * 3. L1(x)
 * 4. Aeq*x = beq
 * 5. aeq'x = beq
 * 6. 1'x = 1, x >= 0 ProbabilitySimplex from the reference Proximal Algorithms by Boyd et al.
 *
 * f(x) can be either a convex or a non-linear function.
 *
 * For convex functions we will use B matrix with 7/10 columns
 *
 * TO DO : For non-linear functions we will experiment with TRON-like Primal solver
 */

class NonlinearMinimizer(ndim: Int,
                         proximal: Proximal = null,
                         maxIters: Int = -1, m: Int = 10, tolerance: Double=1E-4) {
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
      val proxObj = f + u.dot(x - z) + 0.5 * rho * pow(norm(x - z), 2)
      val proxGrad = g + u + (x - z):*rho
      (proxObj, proxGrad)
    }
  }

  //TO DO : alpha needs to be scaled based on Nesterov's acceleration
  val alpha: Double = 1.0

  val ABSTOL = 1e-8
  val RELTOL = 1e-4
  val innerIters = 10

  def iterations(primal: DiffFunction[DenseVector[Double]],
                 rho: Double = 1.0) : State = {
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
      proximal.prox(z, rho)

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
    constraint match {
      case SMOOTH => new NonlinearMinimizer(ndim)
      case POSITIVE => new NonlinearMinimizer(ndim, ProjectPos())
      case BOX => {
        val lb = DenseVector.zeros[Double](ndim)
        val ub = DenseVector.ones[Double](ndim)
        new NonlinearMinimizer(ndim, ProjectBox(lb, ub))
      }
      case EQUALITY => {
        val aeq = DenseVector.ones[Double](ndim)
        new NonlinearMinimizer(ndim, ProjectHyperPlane(aeq, 1.0))
      }
      case SPARSE => new NonlinearMinimizer(ndim, ProximalL1().setLambda(lambda))
      //TO DO: ProximalSimplex : for PLSA
    }
  }

  def main(args: Array[String]) {
    if (args.length < 4) {
      println("Usage: NonlinearMinimizer n m lambda beta")
      println("Test NonlinearMinimizer with a quadratic function of dimenion n and m equalities with lambda beta for elasticNet")
      sys.exit(1)
    }

    val problemSize = args(0).toInt
    val nequalities = args(1).toInt

    val lambda = args(2).toDouble
    val beta = args(3).toDouble

    println(s"Generating randomized QPs with rank ${problemSize} equalities ${nequalities}")
    val (aeq, b, bl, bu, q, h) = QpGenerator(problemSize, nequalities)

    val qpSolver = new QuadraticMinimizer(problemSize)
    val qpStart = System.nanoTime()
    val qpResult = qpSolver.minimize(h, q)
    val qpTime = System.nanoTime() - qpStart

    val nlStart = System.nanoTime()
    val nlResult = NonlinearMinimizer(problemSize, SMOOTH, 0.0).minimize(QuadraticMinimizer.Cost(h, q))
    val nlTime = System.nanoTime() - nlStart

    println(s"||qp - nl|| norm ${norm(qpResult - nlResult, 2)} max-norm ${norm(qpResult - nlResult, inf)}")

    val qpObj = QuadraticMinimizer.computeObjective(h, q, qpResult)
    val nlObj = QuadraticMinimizer.computeObjective(h, q, nlResult)
    println(s"Objective qp $qpObj nl $nlObj")

    println(s"dim ${problemSize} qp ${qpTime/1e6} ms nl ${nlTime/1e6} ms")

    val lambdaL1 = lambda * beta
    val lambdaL2 = lambda * (1 - beta)

    val regularizedGram = h + (DenseMatrix.eye[Double](h.rows) :* lambdaL2)

    val nlSparseStart = System.nanoTime()
    val nlSparseResult = NonlinearMinimizer(problemSize, SPARSE, lambdaL1).iterations(QuadraticMinimizer.Cost(regularizedGram,q))
    val nlSparseTime = System.nanoTime() - nlSparseStart

    val owlqnStart = System.nanoTime()
    val owlqnResult = QuadraticMinimizer.optimizeWithOWLQN(DenseVector.rand[Double](problemSize), regularizedGram, q, lambdaL1)
    val owlqnTime = System.nanoTime() - owlqnStart

    println(s"||owlqn - sparseqp|| norm ${norm(owlqnResult.x - nlSparseResult.x, 2)} inf-norm ${norm(owlqnResult.x - nlSparseResult.x, inf)}")
    println(s"nlSparse ${nlSparseTime/1e6} ms iters ${nlSparseResult.iterations} owlqn ${owlqnTime/1e6} ms iters ${owlqnResult.iter}")
  }
}