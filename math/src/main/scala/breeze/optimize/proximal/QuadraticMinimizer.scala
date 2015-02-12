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

import breeze.linalg._
import breeze.util.SerializableLogging
import scala.math.sqrt
import breeze.optimize.{LBFGS, OWLQN, DiffFunction}
import org.netlib.util.intW
import breeze.optimize.proximal.Constraint._
import scala.math.abs
import breeze.numerics._
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}
import breeze.optimize.linear.{PowerMethod, NNLS, ConjugateGradient}
import breeze.stats.distributions.Rand
import breeze.util.Implicits._

/*
 * Proximal operators and ADMM based Primal-Dual QP Solver 
 * 
 * Reference: http://www.stanford.edu/~boyd/papers/admm/quadprog/quadprog.html
 * 
 * 
 * It solves problem that has the following structure
 * 
 * 1/2 x*'Hx + f*'x + g(x) 
 * s.t ax = b
 *
 * g(x) represents the following constraints which covers matrix factorization use-cases
 * 
 * 1. x >= 0
 * 2. lb <= x <= ub
 * 3. L1(x)
 * 4. L2(x)
 * 5. Generic regularization on x
 */

class QuadraticMinimizer(nGram: Int,
                         proximal: Proximal = null,
                         Aeq: DenseMatrix[Double] = null,
                         beq: DenseVector[Double] = null,
                         maxIters: Int = -1) extends SerializableLogging {
  type BDM = DenseMatrix[Double]
  type BDV = DenseVector[Double]

  case class State(x: BDV, u: BDV, z: BDV, R: BDM, pivot: Array[Int], xHat: BDV, zOld: BDV, residual: BDV, s: BDV, iter: Int, converged: Boolean)

  val linearEquality = if (Aeq != null) Aeq.rows else 0

  if(linearEquality > 0)
    require(beq.length == linearEquality, s"QuadraticMinimizer linear equalities should match beq vector")

  val n = nGram + linearEquality

  //TO DO: Tune alpha based on Nesterov's acceleration
  val alpha: Double = 1.0

  val wsH = if (linearEquality > 0) {
    /**
     * Aeq is l x rank
     * H is rank x rank
     * wsH is a quasi-definite matrix
     * [ P + rho*I, A' ]
	   * [ A        , 0  ]
	  */
    val ws = DenseMatrix.zeros[Double](n, n)
    val transAeq = Aeq.t

    for (row <- 0 until Aeq.rows)
      for (column <- 0 until Aeq.cols)
        ws(row + nGram, column) = Aeq.valueAt(row, column)

    for (row <- 0 until transAeq.rows)
      for (column <- 0 until transAeq.cols)
        ws(row, column + nGram) = transAeq.valueAt(row, column)
    ws
  } else {
    DenseMatrix.zeros[Double](n, n)
  }

  val MAX_ITERS = if (maxIters < 0) Math.max(400, 20 * n) else maxIters

  val ABSTOL = 1e-8
  val RELTOL = 1e-4

  def updateGram(row: Int, col: Int, value: Double) {
    if (row < 0 || row >= n)
      throw new IllegalArgumentException("QuadraticMinimizer row out of bounds for gram matrix update")
    if (col < 0 || col >= n)
      throw new IllegalArgumentException("QuadraticMinimizer column out of bounds for gram matrix update")
    wsH.update(row, col, value)
  }

  //u is the langrange multiplier
  //z is for the proximal operator application

  def initialState(nGram: Int) = {
    var R: DenseMatrix[Double] = null
    var pivot: Array[Int] = null

    //Dense cholesky factorization if the gram matrix is well defined
    if (linearEquality > 0) {
      val lu = LU(wsH)
      R = lu._1
      pivot = lu._2
    } else {
      R = cholesky(wsH).t
    }

    val x = DenseVector.zeros[Double](nGram)

    val z = DenseVector.zeros[Double](nGram)
    val u = DenseVector.zeros[Double](nGram)

    val xHat = DenseVector.zeros[Double](nGram)
    val zOld = DenseVector.zeros[Double](nGram)

    val residual = DenseVector.zeros[Double](nGram)
    val s = DenseVector.zeros[Double](nGram)

    State(x, u, z, R, pivot, xHat, zOld, residual, s, 0, false)
  }

  def iterations(q: DenseVector[Double],
                 rho: Double): Iterator[State] = Iterator.iterate(initialState(nGram)) { state =>
    import state._

    //scale will hold q + linearEqualities
    val scale = DenseVector.zeros[Double](n)

    //scale = rho*(z - u) - q
    for (i <- 0 until z.length) {
      val entryScale = rho * (z(i) - u(i)) - q(i)
      scale.update(i, entryScale)
    }

    if (linearEquality > 0) {
      for (i <- 0 until beq.data.length) scale.update(nGram + i, beq.data(i))
    }

    //TO DO : Use LDL' decomposition for efficiency if the Gram matrix is sparse
    //TO DO : Do we need a full newton step or we should take a damped newton step
    val xlambda = if (linearEquality > 0) {
      // If the Gram matrix is positive definite then use Cholesky else use LU Decomposition
      // x = U \ (L \ q)
      QuadraticMinimizer.solveTriangularLU(R, pivot, scale)
    } else {
      // x = R \ (R' \ scale)
      //Step 1 : R' * y = scale
      //Step 2 : R * x = y
      QuadraticMinimizer.solveTriangular(R, scale)
    }
    for (i <- 0 until x.length) x.update(i, xlambda(i))

    //Unconstrained Quadratic Minimization does need any proximal step
    if (proximal == null) {
      State(x, u, z, R, pivot, xHat, zOld, residual, s, iter + 1, true)
    }
    else {
      //z-update with relaxation

      //zold = (1-alpha)*z
      //x_hat = alpha*x + zold
      zOld := z
      zOld *= 1 - alpha

      xHat := x
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
      residual := x
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

      val epsPrimal = sqrt(n) * ABSTOL + RELTOL * max(norm(x, 2), norm(residual, 2))
      val epsDual = sqrt(n) * ABSTOL + RELTOL * norm(s, 2)

      val converged = residualNorm < epsPrimal && sNorm < epsDual || iter > MAX_ITERS

      State(x, u, z, R, pivot, xHat, zOld, residual, s, iter + 1, converged)
    }
  }.takeUpToWhere(_.converged)

  private def computeRho(H: DenseMatrix[Double]): Double = {
    proximal match {
      case null => 0.0
      case ProximalL1() => {
        val eigenMax = QuadraticMinimizer.normColumn(H)
        val eigenMin = QuadraticMinimizer.approximateMinEigen(H)
        sqrt(eigenMin * eigenMax)
      }
      case _ => sqrt(QuadraticMinimizer.normColumn(H))
    }
  }

  def minimize(H: DenseMatrix[Double], q: DenseVector[Double]): DenseVector[Double] = {
    minimizeAndReturnState(H, q).x
  }

  def minimizeAndReturnState(H: DenseMatrix[Double], q: DenseVector[Double]): State = {
    iterations(H, q).last
  }

  def iterations(H: DenseMatrix[Double], q: DenseVector[Double]): Iterator[State] = {
    for (i <- 0 until H.rows)
      for (j <- 0 until H.cols) {
        wsH.update(i, j, H(i, j))
      }
    val rho = computeRho(wsH)
    for (i <- 0 until H.rows) wsH.update(i, i, wsH(i, i) + rho)

    val result = iterations(q, rho)
    result
  }
}

/* 
 * PDCO dense quadratic program generator
 *  
 * Reference
 * 
 * Generates random instances of Quadratic Programming Problems
 * 0.5x'Px + q'x
 * s.t Ax = b
 *  lb <= x <= ub  
 
 * @param A is the equality constraint
 * @param b is the equality parameters
 * @param lb is vector of lower bounds
 * @param ub is vector of upper bounds
 * @param q is linear representation of the function
 * @param H is the quadratic representation of the function 
 * 
 */
object QpGenerator {
  def getGram(nGram: Int) = {
    val hrand = DenseMatrix.rand[Double](nGram, nGram, Rand.gaussian(0, 1))
    val hrandt = hrand.t
    val hposdef = hrandt * hrand
    val H = hposdef.t + hposdef
    H
  }
  
  def apply(nHessian: Int, nEqualities: Int) = {
    val en = DenseVector.ones[Double](nHessian)
    val zn = DenseVector.zeros[Double](nHessian)

    val A = DenseMatrix.rand[Double](nEqualities, nHessian)
    val x = en

    val b = A * x
    val q = DenseVector.rand[Double](nHessian)

    val lb = zn.copy
    val ub = en :* 10.0
    
    val H = getGram(nHessian)
    
    (A, b, lb, ub, q, H)
  }
}

object QuadraticMinimizer {
  //upper bound on max eigen value
  def normColumn(H: DenseMatrix[Double]): Double = {
    var absColSum = 0.0
    var maxColSum = 0.0
    for (c <- 0 until H.cols) {
      for (r <- 0 until H.rows) {
        absColSum += abs(H(r, c))
      }
      if (absColSum > maxColSum) maxColSum = absColSum
      absColSum = 0.0
    }
    maxColSum
  }

  //approximate max eigen using inverse power method
  def approximateMaxEigen(H: DenseMatrix[Double]) : Double = {
    val pm = new PowerMethod[DenseVector[Double], DenseMatrix[Double]]()
    val init = DenseVector.rand[Double](H.rows, Rand.gaussian(0, 1))
    pm.eigen(init, H)
  }

  //approximate min eigen using inverse power method
  def approximateMinEigen(H: DenseMatrix[Double]) : Double = {
    val R = cholesky(H).t
    val pmInv = new PowerMethod[DenseVector[Double], DenseMatrix[Double]](10, 1e-5, true)
    val init = DenseVector.rand[Double](H.rows, Rand.gaussian(0, 1))
    1.0/pmInv.eigen(init, R)
  }

  /* 
   * Triangular LU solve for A*X = B 
   * TO DO : Add appropriate exception from LAPACK
   */
  def solveTriangularLU(A: DenseMatrix[Double], pivot: Array[Int], B: DenseVector[Double]) : DenseVector[Double] = {
    require(A.rows == A.cols)
    
    val X = new DenseMatrix(B.length, 1, B.data.clone)
    
    val n = A.rows
    val nrhs = X.cols
    var info: intW = new intW(0)

    lapack.dgetrs("No transpose", n, nrhs, A.data, 0, A.rows, pivot, 0, X.data, 0, X.rows, info)

    if (info.`val` > 0) throw new LapackException("DGETRS: LU solve unsuccessful")

    DenseVector(X.data)
  }

  /*Triangular cholesky solve for A*X = B */
  def solveTriangular(A: DenseMatrix[Double], B: DenseVector[Double]) : DenseVector[Double] = {
    require(A.rows == A.cols)
    
    val X = new DenseMatrix(B.length, 1, B.data.clone)
    
    val n = A.rows
    val nrhs = X.cols
    var info: intW = new intW(0)

    lapack.dpotrs("L", n, nrhs, A.data, 0, A.rows, X.data, 0, X.rows, info)

    if (info.`val` > 0) throw new LapackException("DPOTRS : Leading minor of order i of A is not positive definite.")
    
    DenseVector(X.data)
  }

  def apply(rank: Int,
            constraint: Constraint,
            lambda: Double): QuadraticMinimizer = {
    constraint match {
      case SMOOTH => new QuadraticMinimizer(rank)
      case POSITIVE => new QuadraticMinimizer(rank, ProjectPos())
      case BOX => {
        //Direct QP with bounds
        val lb = DenseVector.zeros[Double](rank)
        val ub = DenseVector.ones[Double](rank)
        new QuadraticMinimizer(rank, ProjectBox(lb, ub))
      }
      case EQUALITY => {
        //Direct QP with equality and positivity constraint
        val Aeq = DenseMatrix.ones[Double](1, rank)
        val beq = DenseVector.ones[Double](1)
        new QuadraticMinimizer(rank, ProjectPos(), Aeq, beq)
      }
      case SPARSE => new QuadraticMinimizer(rank, ProximalL1().setLambda(lambda))
    }
  }

  def computeObjective(h: DenseMatrix[Double], q: DenseVector[Double], x: DenseVector[Double]): Double = {
    val res = (x.t*h*x)*0.5 + q.dot(x)
    res
  }

  case class Cost(H: DenseMatrix[Double],
                  q: DenseVector[Double]) extends DiffFunction[DenseVector[Double]] {
    def calculate(x: DenseVector[Double]) = {
      (computeObjective(H, q, x), H * x + q)
    }
  }

  def optimizeWithLBFGS(init: DenseVector[Double],
                         H: DenseMatrix[Double],
                         q: DenseVector[Double]) = {
    val lbfgs = new LBFGS[DenseVector[Double]](-1, 7)
    val state = lbfgs.minimizeAndReturnState(Cost(H, q), init)
    val approxMinEigen = lbfgs.minEigen(state, init)
    val approxMaxEigen = lbfgs.maxEigen(state, init)
    val eigs = eigSym(H).eigenvalues

    val minEigen = min(eigs)
    val maxEigen = max(eigs)
    println(s"minEigen $minEigen approx $approxMinEigen maxEigen $maxEigen approx $approxMaxEigen")

    state.x
  }

  def optimizeWithOWLQN(init: DenseVector[Double],
                        regularizedGram: DenseMatrix[Double],
                        q: DenseVector[Double],
                        lambdaL1: Double) = {
    val owlqn = new OWLQN[Int, DenseVector[Double]](-1, 7, lambdaL1, 1e-6)
    owlqn.minimizeAndReturnState(Cost(regularizedGram, q), init)
  }

  def main(args: Array[String]) {
    if (args.length < 4) {
      println("Usage: QpSolver n m lambda beta")
      println("Test QpSolver with a simple quadratic function of dimension n and m equalities lambda beta for elasticNet")
      sys.exit(1)
    }

    val problemSize = args(0).toInt
    val nequalities = args(1).toInt

    val lambda = args(2).toDouble
    val beta = args(3).toDouble
    
    println(s"Generating randomized QPs with rank ${problemSize} equalities ${nequalities}")
    val (aeq, b, bl, bu, q, h) = QpGenerator(problemSize, nequalities)
    
    println(s"Test QuadraticMinimizer, CG , BFGS and OWLQN with $problemSize variables and $nequalities equality constraints")
    
    val luStart = System.nanoTime()
    val luResult = h \ q:*(-1.0)
    val luTime = System.nanoTime() - luStart

    val cg = new ConjugateGradient[DenseVector[Double], DenseMatrix[Double]]()
    
    val startCg = System.nanoTime()
    val cgResult = cg.minimize(q:*(-1.0), h)
    val cgTime = System.nanoTime() - startCg
    
    val qpSolver = new QuadraticMinimizer(problemSize)
    val qpStart = System.nanoTime()
    val result = qpSolver.minimize(h, q)
    val qpTime = System.nanoTime() - qpStart

    val startBFGS = System.nanoTime()
    val bfgsResult = optimizeWithLBFGS(DenseVector.rand[Double](problemSize), h, q)
    val bfgsTime = System.nanoTime() - startBFGS

    println(s"||qp - lu|| norm ${norm(result - luResult, 2)} max-norm ${norm(result - luResult, inf)}")
    println(s"||cg - lu|| norm ${norm(cgResult - luResult,2)} max-norm ${norm(cgResult - luResult, inf)}")
    println(s"||bfgs - lu|| norm ${norm(bfgsResult - luResult, 2)} max-norm ${norm(bfgsResult - luResult, inf)}")

    val luObj = computeObjective(h, q, luResult)
    val bfgsObj = computeObjective(h, q, bfgsResult)
    val qpObj = computeObjective(h, q, result)

    println(s"Objective lu $luObj bfgs $bfgsObj qp $qpObj")

    println(s"dim ${problemSize} lu ${luTime/1e6} ms qp ${qpTime/1e6} ms cg ${cgTime/1e6} ms bfgs ${bfgsTime/1e6} ms")
    
    val lambdaL1 = lambda * beta
    val lambdaL2 = lambda * (1 - beta)

    val regularizedGram = h + (DenseMatrix.eye[Double](h.rows) :* lambdaL2)
    
    val sparseQp = QuadraticMinimizer(h.rows, SPARSE, lambdaL1)
    val sparseQpStart = System.nanoTime()
    val sparseQpResult = sparseQp.minimizeAndReturnState(regularizedGram, q)
    val sparseQpTime = System.nanoTime() - sparseQpStart

    val startOWLQN = System.nanoTime()
    val owlqnResult = optimizeWithOWLQN(DenseVector.rand[Double](problemSize), regularizedGram, q, lambdaL1)
    val owlqnTime = System.nanoTime() - startOWLQN
    
    println(s"||owlqn - sparseqp|| norm ${norm(owlqnResult.x - sparseQpResult.x, 2)} inf-norm ${norm(owlqnResult.x - sparseQpResult.x, inf)}")
    println(s"sparseQp ${sparseQpTime/1e6} ms iters ${sparseQpResult.iter} owlqn ${owlqnTime/1e6} ms iters ${owlqnResult.iter}")

    val posQp = QuadraticMinimizer(h.rows, POSITIVE, 0.0)
    val posQpStart = System.nanoTime()
    val posQpResult = posQp.minimizeAndReturnState(h, q)
    val posQpTime = System.nanoTime() - posQpStart

    val nnls = new NNLS()
    val nnlsStart = System.nanoTime()
    val nnlsResult = nnls.minimizeAndReturnState(h, q)
    val nnlsTime = System.nanoTime() - nnlsStart

    println(s"posQp ${posQpTime/1e6} ms iters ${posQpResult.iter} nnls ${nnlsTime/1e6} ms iters ${nnlsResult.iter}")
    
    val boundsQp = new QuadraticMinimizer(h.rows, ProjectBox(bl, bu))
    val boundsQpStart = System.nanoTime()
    val boundsQpResult = boundsQp.minimizeAndReturnState(h, q)
    val boundsQpTime = System.nanoTime() - boundsQpStart

    println(s"boundsQp ${boundsQpTime/1e6} ms iters ${boundsQpResult.iter} converged ${boundsQpResult.converged}")

    val qpEquality = new QuadraticMinimizer(h.rows, ProjectPos(), aeq, b)
    val qpEqualityStart = System.nanoTime()
    val qpEqualityResult = qpEquality.minimizeAndReturnState(h, q)
    val qpEqualityTime = System.nanoTime() - qpEqualityStart

    println(s"Qp Equality ${qpEqualityTime/1e6} ms iters ${qpEqualityResult.iter} converged ${qpEqualityResult.converged}")
  }
}