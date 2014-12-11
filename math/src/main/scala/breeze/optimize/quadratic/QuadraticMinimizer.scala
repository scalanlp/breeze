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

import breeze.linalg.pinv
import breeze.linalg.cholesky
import breeze.linalg.LU
import scala.math.min
import scala.math.max
import scala.math.sqrt
import breeze.optimize.LBFGS
import breeze.optimize.OWLQN
import breeze.linalg.DenseVector
import breeze.optimize.DiffFunction
import org.netlib.util.intW
import breeze.optimize.quadratic.Constraint._
import scala.math.abs
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.numerics._
import breeze.linalg.LapackException
import breeze.linalg.norm
import com.github.fommil.netlib.BLAS.{getInstance=>blas}
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}
import breeze.optimize.linear.ConjugateGradient

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
  lb: Option[DenseVector[Double]] = None, ub: Option[DenseVector[Double]] = None,
  Aeq: Option[DenseMatrix[Double]] = None, beq: Option[DenseVector[Double]] = None,
  addEqualityToGram: Boolean = false) {

  //if addEquality is true, add the contribution of the equality constraint to gram matrix workspace
  val linearEquality = if (Aeq != None) Aeq.get.rows else 0

  val n = nGram + linearEquality

  var alpha: Double = 1.0
  var rho: Double = 0.0

  var solveTime: Long = 0
  var iterations: Long = 0

  val wsH = if (addEqualityToGram && linearEquality > 0) {
    //Aeq is l x rank
    //H is rank x rank
    /* wsH is a quasi-definite matrix */
    /* [ P + rho*I, A' ]
	 * [ A        , 0  ]
	 */
    val matAeq = Aeq.get
    val ws = DenseMatrix.zeros[Double](n, n)
    val transAeq = matAeq.t

    for (row <- 0 until matAeq.rows)
      for (column <- 0 until matAeq.cols)
        ws(row + nGram, column) = matAeq.valueAt(row, column)

    for (row <- 0 until transAeq.rows)
      for (column <- 0 until transAeq.cols)
        ws(row, column + nGram) = transAeq.valueAt(row, column)
    ws
  } else {
    DenseMatrix.zeros[Double](n, n)
  }

  val MAX_ITER = Math.max(400, 20 * n)
  
  val ABSTOL = 1e-8
  val RELTOL = 1e-4
  val EPS = 1e-4

  val z = DenseVector.zeros[Double](nGram)
  val u = DenseVector.zeros[Double](nGram)

  val xHat = DenseVector.zeros[Double](nGram)
  val zOld = DenseVector.zeros[Double](nGram)

  //scale will hold q + linearEqualities
  val scale = DenseVector.zeros[Double](n)

  val residual = DenseVector.zeros[Double](nGram)
  val s = DenseVector.zeros[Double](nGram)

  /* L1 regularization */
  var lambda: Double = 1.0
  var constraint: Constraint = SMOOTH

  var R: DenseMatrix[Double] = null
  var pivot: Array[Int] = null

  /* If Aeq exists and rows > 1, cache the pseudo-inverse to be used later */
  /* If Aeq exist and rows = 1, cache the transpose */
  val invAeq = if (!addEqualityToGram && Aeq != None && Aeq.get.rows > 1)
    Some(pinv(Aeq.get))
  else
    None

  /*Regularization for Elastic Net */
  def setLambda(lambda: Double): QuadraticMinimizer = {
    this.lambda = lambda
    this
  }

  //TO DO : This can take a proximal function as input
  //TO DO : alpha needs to be scaled based on Nesterov's acceleration
  def setProximal(constraint: Constraint): QuadraticMinimizer = {
    this.constraint = constraint
    this
  }

  def updateGram(row: Int, col: Int, value: Double) {
    if (row < 0 || row >= n) {
      throw new IllegalArgumentException("QuadraticMinimizer row out of bounds for gram matrix update")
    }
    if (col < 0 || col >= n) {
      throw new IllegalArgumentException("QuadraticMinimizer column out of bounds for gram matrix update")
    }
    wsH.update(row, col, value)
  }
  
  def solve(q: DenseVector[Double]): (DenseVector[Double], Boolean) = {
    //Dense cholesky factorization if the gram matrix is well defined
    if (!addEqualityToGram) {
      R = cholesky(wsH).t
    } else {
      val lu = LU(wsH)
      R = lu._1
      pivot = lu._2
    }
    
    val x = DenseVector.zeros[Double](nGram)

    z := 0.0
    u := 0.0

    residual := 0.0
    s := 0.0

    var k = 0

    //u is the langrange multiplier
    //z is for the proximal operator application

    /* Check if linearEqualities and beq matches */
    if (linearEquality > 0 && beq.get.length != linearEquality) {
      throw new IllegalArgumentException("QuadraticMinimizer beq vector should match the number of linear equalities")
    }

    //Memory for x and tempR are allocated by Solve.solve calls
    //TO DO : See how this is implemented in breeze, why a workspace can't be used  
    while (k < MAX_ITER) {
      //scale = rho*(z - u) - q
      for (i <- 0 until z.length) {
        val entryScale = rho * (z(i) - u(i)) - q(i)
        scale.update(i, entryScale)
      }

      if (linearEquality > 0) {
        for (i <- 0 until beq.get.data.length) scale.update(nGram + i, beq.get.data(i))
      }

      //TO DO : Use LDL' decomposition for efficiency if the Gram matrix is sparse
      //TO DO : Do we need a full newton step or we should take a damped newton step
      val xlambda = if (addEqualityToGram) {
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
      if (constraint == SMOOTH) return (x, true)
      
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
            throw new IllegalArgumentException("QuadraticMinimizer proximal operator on box needs lower and upper bounds")
          Proximal.projectBox(z.data, lb.get.data, ub.get.data)
        }
        case EQUALITY => {
          if (Aeq == None) throw new IllegalArgumentException("QuadraticMinimizer proximal operator on equality needs Aeq")
          if (beq == None) throw new IllegalArgumentException("QuadraticMinimizer proximal operator on equality needs beq")
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
      
      if (residualNorm < epsPrimal && sNorm < epsDual) {
        iterations += k
        return (z, true)
      }
      k += 1
    }
    iterations += MAX_ITER
    (z, false)
  }

  private def normColumn(H: DenseMatrix[Double]): Double = {
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

  //TO DO : Replace eigenMin using inverse power law for 5-10 iterations
  private def computeRho(H: DenseMatrix[Double]): Double = {
    constraint match {
      case SMOOTH => 0.0
      case SPARSE => {
        val eigenMax = normColumn(H)
        //TO DO: Make sure it calls Dposv.dposv
        val inverse = H \ DenseMatrix.eye[Double](H.rows)
        val eigenMin = 1 / normColumn(inverse)
        sqrt(eigenMin * eigenMax)
      }
      case _ => sqrt(normColumn(H))
    }
  }

  def solve(H: DenseMatrix[Double], q: DenseVector[Double]): (DenseVector[Double], Boolean) = {
    for (i <- 0 until H.rows)
      for (j <- 0 until H.cols) {
        wsH.update(i, j, H(i, j))
      }
    rho = computeRho(wsH)
    for (i <- 0 until H.rows) wsH.update(i, i, wsH(i, i) + rho)

    val solveStart = System.nanoTime()
    val result = solve(q)
    solveTime += System.nanoTime() - solveStart
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
    val hrand = DenseMatrix.rand[Double](nGram, nGram)
    val hrandt = hrand.t
    val hposdef = hrandt * hrand
    val H = hposdef.t + hposdef
    H
  }
  
  def apply(nHessian: Int, nEqualities: Int) = {
    val em = DenseVector.ones[Double](nEqualities)
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
  /* 
   * Triangular LU solve for A*X = B 
   * TO DO : Add appropriate exception from LAPACK
   */
  def solveTriangularLU(A: DenseMatrix[Double], pivot: Array[Int], B: DenseVector[Double]) = {
    require(A.rows == A.cols)
    
    var X = new DenseMatrix(B.length, 1, B.data.clone)
    
    val n = A.rows
    val nrhs = X.cols
    var info: intW = new intW(0)

    lapack.dgetrs("No transpose", n, nrhs, A.data, 0, A.rows, pivot, 0, X.data, 0, X.rows, info)

    if (info.`val` > 0) throw new LapackException("DGETRS: LU solve unsuccessful")

    DenseVector(X.data)
  }

  /*Triangular cholesky solve for A*X = B */
  def solveTriangular(A: DenseMatrix[Double], B: DenseVector[Double]) = {
    require(A.rows == A.cols)
    
    var X = new DenseMatrix(B.length, 1, B.data.clone)
    
    val n = A.rows
    val nrhs = X.cols
    var info: intW = new intW(0)

    lapack.dpotrs("L", n, nrhs, A.data, 0, A.rows, X.data, 0, X.rows, info)

    if (info.`val` > 0) throw new LapackException("DPOTRS : Leading minor of order i of A is not positive definite.")
    
    DenseVector(X.data)
  }
  
  def apply(rank: Int, constraint: Constraint, lambda: Double): QuadraticMinimizer = {
    constraint match {
      case SMOOTH => new QuadraticMinimizer(rank)
      case POSITIVE => new QuadraticMinimizer(rank).setProximal(POSITIVE)
      case BOUNDS => {
        //Direct QP with bounds
        val lb = DenseVector.zeros[Double](rank)
        val ub = DenseVector.ones[Double](rank)
        new QuadraticMinimizer(rank, Some(lb), Some(ub)).setProximal(BOUNDS)
      }
      case EQUALITY => {
        //Direct QP with equality and positivity constraint
        val Aeq = DenseMatrix.ones[Double](1, rank)
        val beq = DenseVector.ones[Double](1)
        val qm = new QuadraticMinimizer(rank, None, None, Some(Aeq), Some(beq), true).setProximal(POSITIVE)
        qm
      }
      case SPARSE => {
        val qm = new QuadraticMinimizer(rank).setProximal(SPARSE)
        qm.setLambda(lambda)
        qm
      }
    }
  }

  def computeObjective(h: DenseMatrix[Double], q: DenseVector[Double], x: DenseVector[Double]): Double = {
    val res = (x.t*h*x)*0.5 + q.dot(x)
    res
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
    
    println(s"Test QuadraticMinimizer, CG and OWLQN with n $problemSize m $nequalities")
    
    val luStart = System.nanoTime()
    val luResult = h \ q:*(-1.0)
    val luTime = System.nanoTime() - luStart

    val cg = new ConjugateGradient[DenseVector[Double], DenseMatrix[Double]]()
    
    val startCg = System.nanoTime()
    val cgResult = cg.minimize(q:*(-1.0), h)
    val cgTime = System.nanoTime() - startCg
    
    val qpSolver = new QuadraticMinimizer(problemSize)
    val qpStart = System.nanoTime()
    val (result, converged) = qpSolver.solve(h, q)
    val qpTime = System.nanoTime() - qpStart
    
    println(s"||qp - lu|| norm ${norm(result - luResult, 2)} max-norm ${norm(result - luResult, inf)}")
    println(s"||cg - lu|| norm ${norm(cgResult - luResult,2)} max-norm ${norm(cgResult - luResult, inf)}")
    println(s"dim ${problemSize} lu ${luTime/1e6} qp ${qpTime/1e6} cg ${cgTime/1e6}")
    
    val lambdaL1 = lambda * beta
    val lambdaL2 = lambda * (1 - beta)
    val regularizedGram = h + DenseMatrix.eye[Double](h.rows) :* lambdaL2
    
    val owlqn = new OWLQN[Int, DenseVector[Double]](-1, 7, lambdaL1)
    
    def optimizeWithOWLQN(init: DenseVector[Double]) = {
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (computeObjective(regularizedGram, q, x), regularizedGram*x + q)
        }
      }
      owlqn.minimize(f, init)
    }
    
    val sparseQp = QuadraticMinimizer(h.rows, SPARSE, lambdaL1)
    val (sparseQpResult, sparseQpConverged) = sparseQp.solve(regularizedGram, q)
    
    val startOWLQN = System.nanoTime()
    val owlqnResult = optimizeWithOWLQN(DenseVector.rand[Double](problemSize))
    val owlqnTime = System.nanoTime() - startOWLQN
    
    println(s"||owlqn - sparseqp|| norm ${norm(owlqnResult - sparseQpResult, 2)} inf-norm ${norm(owlqnResult - sparseQpResult, inf)}")
    println(s"sparseQp ${sparseQp.solveTime/1e6} ms iters ${sparseQp.iterations} owlqn ${owlqnTime/1e6} ms")

    val posQp = new QuadraticMinimizer(h.rows).setProximal(POSITIVE)
    val (posQpResult, posQpConverged) = posQp.solve(h, q)

    val nnls = NNLS(h.rows)
    val nnlsResult = nnls.solve(h, q)
    
    println(s"posQp ${posQp.solveTime/1e6} ms iters ${posQp.iterations} nnls ${nnls.solveTime/1e6} ms iters ${nnls.iterations}")
    
    val boundsQp = new QuadraticMinimizer(h.rows, Some(bl), Some(bu)).setProximal(BOUNDS)
    val (boundsQpResult, boundsQpConverged) = boundsQp.solve(h, q)

    println(s"boundsQp ${boundsQp.solveTime/1e6} ms iters ${boundsQp.iterations} converged $boundsQpConverged")

    val qpEquality = new QuadraticMinimizer(h.rows, None, None, Some(aeq), Some(b), true).setProximal(POSITIVE)
    val (qpEqualityResult, qpEqualityConverged) = qpEquality.solve(h, q)

    println(s"Qp Equality ${qpEquality.solveTime/1e6} ms iters ${qpEquality.iterations} converged $qpEqualityConverged")
  }
}