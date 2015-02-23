package breeze.optimize.proximal

import breeze.linalg.{DenseMatrix, DenseVector, norm}
import breeze.optimize.{ProjectedQuasiNewton, OWLQN, DiffFunction, LBFGS}
import breeze.stats.distributions.Rand
import breeze.optimize.proximal.Constraint._
import scala.math._
import scala.math.pow
import scala.math.sqrt
import scala.math.abs

/**
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
 *
 * @author debasish83
 */

class NonlinearMinimizer(ndim: Int,
                         proximal: Proximal = null,
                         maxIters: Int = -1, m: Int = 7) {
  type BDV = DenseVector[Double]

  case class State(x: BDV, u: BDV, z: BDV, iterations: Int, converged: Boolean)

  /*
    Proximal modifications to Primal algorithm
    AdmmObj(x, u, z) = f(x) + u'(x-z) + rho/2*||x - z||^{2}
    dAdmmObj/dx = df/dx + u + rho(x - z)
  */
  case class ProximalPrimal(primal: DiffFunction[DenseVector[Double]],
                            u: BDV, z: BDV,
                            rho: Double) extends DiffFunction[DenseVector[Double]] {
    override def calculate(x: DenseVector[Double]) = {
      val (f, g) = primal.calculate(x)
      val proxObj = f + u.dot(x - z) + 0.5 * rho * pow(norm(x - z), 2)
      val proxGrad = g + u + (x - z) :* rho
      (proxObj, proxGrad)
    }
  }

  //TO DO : alpha needs to be scaled based on Nesterov's acceleration
  val alpha: Double = 1.0

  val ABSTOL = 1e-4
  val RELTOL = 1e-4

  val admmIters = math.max(1000, 40 * ndim)

  val quadraticIters = 10

  def getProximal = proximal

  def iterationsADMM(primal: DiffFunction[DenseVector[Double]],
                     rho: Double = 1.0): State = {
    val innerIters = 10

    val iters = if (proximal == null) maxIters else innerIters
    val lbfgs = new LBFGS[DenseVector[Double]](iters, m)
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

    while (k < admmIters) {
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
        return State(result, u, z, k, true)
      }
      k = k + 1
      init := result
    }
    State(init, u, z, admmIters, false)
  }

  def iterationsADMMWithHistory(primal: DiffFunction[DenseVector[Double]], rho: Double = 1.0) : State = {
    val innerIters = 1

    val iters = if (proximal == null) -1 else quadraticIters

    val primalSolve = new LBFGS[DenseVector[Double]](iters, m)
    val init = DenseVector.rand[Double](ndim, Rand.gaussian(0, 1))

    val z = DenseVector.zeros[Double](ndim)
    val u = DenseVector.zeros[Double](ndim)

    if (proximal == null) {
      return State(primalSolve.minimize(primal, init), u, z, 0, true)
    }

    var initialState = primalSolve.minimizeAndReturnState(primal, init)
    val proxPrimal = ProximalPrimal(primal, u, z, rho)

    val xHat = DenseVector.zeros[Double](ndim)
    val zOld = DenseVector.zeros[Double](ndim)

    val residual = DenseVector.zeros[Double](ndim)
    val s = DenseVector.zeros[Double](ndim)

    var k = 0

    while(k < admmIters) {
      val resultIterator = primalSolve.iterations(proxPrimal, initialState)
      var resultState = resultIterator.next()
      var i = 0
      while(i < innerIters) {
        resultState = resultIterator.next()
        i = i + 1
      }
      resultState.grad -= (u - z:*rho)
      //z-update with relaxation

      //zold = (1-alpha)*z
      //x_hat = alpha*x + zold
      zOld := z
      zOld *= 1 - alpha

      xHat := resultState.x
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
      residual := resultState.x
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

      val epsPrimal = sqrt(ndim) * ABSTOL + RELTOL * max(norm(resultState.x, 2), norm(residual, 2))
      val epsDual = sqrt(ndim) * ABSTOL + RELTOL * norm(s, 2)

      if (residualNorm < epsPrimal && sNorm < epsDual) {
        return State(resultState.x, u, z, k, true)
      }
      k = k + 1
      initialState = resultState
      initialState.grad += (u - z:*rho)
    }
    State(initialState.x, u, z, admmIters, false)
  }

  def minimize(primal: DiffFunction[DenseVector[Double]]): DenseVector[Double] = {
    iterationsADMM(primal).x
  }
}

object LinearGenerator {
  case class Cost(data: DenseMatrix[Double], labels: DenseVector[Double]) extends DiffFunction[DenseVector[Double]] {
    def calculate(x: DenseVector[Double]) = {
      val cumGradient = DenseVector.zeros[Double](x.length)
      var cumLoss = 0.0
      var i = 0
      while (i < data.rows) {
        val brzData = data(i, ::).t
        val diff = x.dot(brzData) - labels(i)
        cumGradient += brzData * (2.0 * diff)
        cumLoss += diff * diff
        i = i + 1
      }
      (cumLoss, cumGradient)
    }
  }

  def apply(ndim: Int) : (DiffFunction[DenseVector[Double]], DenseMatrix[Double], DenseVector[Double]) = {
    val rand = Rand.gaussian(0, 1)
    val data = DenseMatrix.rand[Double](ndim, ndim, rand)
    val labels = DenseVector.rand[Double](ndim, rand).map { x => if (x > 0.5) 1.0 else 0.0}
    //||ax - b||_2^{2} = x'a'ax - 2*x'a'*b + c
    val h = (data.t*data)*2.0
    val q = (data.t*labels)
    q *= -2.0
    (Cost(data, labels), h, q)
  }
}

object LogisticGenerator {
  case class Cost(data: DenseMatrix[Double],
                  labels: DenseVector[Double]) extends DiffFunction[DenseVector[Double]] {
    def calculate(x: DenseVector[Double]) = {
      val cumGradient = DenseVector.zeros[Double](x.length)
      var cumLoss = 0.0

      var i = 0
      while(i < data.rows) {
        val brzData = data(i, ::).t
        val margin: Double = -1.0 * x.dot(brzData)
        val gradientMultiplier = (1.0 / (1.0 + math.exp(margin))) - labels(i)
        val gradient = brzData * gradientMultiplier
        val loss =
          if (labels(i) > 0) {
            math.log1p(math.exp(margin)) // log1p is log(1+p) but more accurate for small p
          } else {
            math.log1p(math.exp(margin)) - margin
          }
        cumGradient += gradient
        cumLoss += loss
        i = i + 1
      }
      (cumLoss, cumGradient)
    }
  }

  def apply(ndim: Int) : DiffFunction[DenseVector[Double]] = {
    val rand = Rand.gaussian(0, 1)
    val data = DenseMatrix.rand[Double](ndim, ndim, rand)
    val labels = DenseVector.rand[Double](ndim, rand).map { x => if (x > 0.5) 1.0 else 0.0}
    Cost(data, labels)
  }
}

object NonlinearMinimizer {
  def optimizeWithOWLQN(problemSize: Int,
                        loss: DiffFunction[DenseVector[Double]],
                        lambdaL1: Double) = {
    val owlqn = new OWLQN[Int, DenseVector[Double]](-1, 7, lambdaL1)
    val init = DenseVector.rand[Double](problemSize, Rand.gaussian(0, 1))
    val state: owlqn.State = owlqn.minimizeAndReturnState(loss, init)
    state
  }

  def apply(ndim: Int, constraint: Constraint, lambda: Double = 1.0): NonlinearMinimizer = {
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
      case PROBABILITYSIMPLEX => new NonlinearMinimizer(ndim, ProjectProbabilitySimplex(1.0))
    }
  }

  def main(args: Array[String]) {
    if (args.length < 3) {
      println("Usage: NonlinearMinimizer n lambda beta")
      println("Test NonlinearMinimizer with a quadratic function of dimenion n and m equalities with lambda beta for elasticNet")
      sys.exit(1)
    }

    val problemSize = args(0).toInt
    val lambda = args(1).toDouble
    val beta = args(2).toDouble

    println(s"Generating Linear and Logistic Loss with rank ${problemSize}")

    val (quadraticCost, h, q) = LinearGenerator(problemSize)

    val lambdaL1 = lambda * beta
    val lambdaL2 = lambda * (1 - beta)

    val regularizedGram = h + (DenseMatrix.eye[Double](h.rows) :* lambdaL2)

    val sparseQp = QuadraticMinimizer(h.rows, SPARSE, lambdaL1)
    val sparseQpStart = System.nanoTime()
    val sparseQpResult = sparseQp.minimizeAndReturnState(regularizedGram, q)
    val sparseQpTime = System.nanoTime() - sparseQpStart

    val owlqnStart = System.nanoTime()
    val owlqnResult = QuadraticMinimizer.optimizeWithOWLQN(DenseVector.rand[Double](problemSize), regularizedGram, q, lambdaL1)
    val owlqnTime = System.nanoTime() - owlqnStart

    println("ElasticNet Formulation")

    println("Linear Regression")

    val owlqnObj = QuadraticMinimizer.computeObjective(regularizedGram, q, owlqnResult.x) + lambdaL1*owlqnResult.x.foldLeft(0.0){(agg, entry) => agg + abs(entry)}
    val sparseQpL1Obj = lambdaL1*sparseQpResult.x.foldLeft(0.0){(agg, entry) => agg + abs(entry)}
    val sparseQpObj = QuadraticMinimizer.computeObjective(regularizedGram, q, sparseQpResult.x) + sparseQpL1Obj
    val quadraticCostWithL2 = QuadraticMinimizer.Cost(regularizedGram, q)

    val pqnSparseStart = System.nanoTime()
    val init = DenseVector.zeros[Double](problemSize)
    val projectL1 = ProjectL1(sparseQpL1Obj)

    val pqnSparseResult = ProjectedQuasiNewton.proximal(projectL1).minimizeAndReturnState(quadraticCostWithL2, init)
    val pqnSparseTime = System.nanoTime() - pqnSparseStart
    val pqnSparseObj = QuadraticMinimizer.computeObjective(regularizedGram, q, pqnSparseResult.x)

    val nlSparse = NonlinearMinimizer(problemSize, SPARSE, lambdaL1)
    val nlSparseStart = System.nanoTime()
    val nlSparseResult = nlSparse.iterationsADMM(quadraticCostWithL2, 2.0)
    val nlSparseTime = System.nanoTime() - nlSparseStart
    val nlSparseObj = QuadraticMinimizer.computeObjective(regularizedGram, q, nlSparseResult.x) + lambdaL1*nlSparseResult.x.foldLeft(0.0){(agg, entry) => agg + abs(entry)}

    println(s"owlqn ${owlqnTime / 1e6} ms iters ${owlqnResult.iter} sparseQp ${sparseQpTime / 1e6} ms iters ${sparseQpResult.iter}")
    println(s"pqnSparseTime ${pqnSparseTime / 1e6} ms iters ${pqnSparseResult.iter}")
    println(s"nlSparseTime ${nlSparseTime/1e6} ms iters ${nlSparseResult.iterations}")

    println(s"owlqnObj $owlqnObj sparseQpObj $sparseQpObj sparseQpL1Obj $sparseQpL1Obj pqnSparseObj $pqnSparseObj nlSparseObj $nlSparseObj")

    println("Logistic Regression")

    val logisticLoss = LogisticGenerator(problemSize)
    val elasticNetLoss = DiffFunction.withL2Regularization(logisticLoss, lambdaL2)

    val owlqnLogisticStart = System.nanoTime()
    val owlqnLogisticResult = optimizeWithOWLQN(problemSize, elasticNetLoss, lambdaL1)
    val owlqnLogisticTime = System.nanoTime() - owlqnLogisticStart
    val owlqnL1Obj = lambdaL1 * owlqnLogisticResult.x.foldLeft(0.0) { (agg, entry) => agg + abs(entry)}
    val owlqnLogisticObj = elasticNetLoss.calculate(owlqnLogisticResult.x)._1 + owlqnL1Obj

    val pqnLogisticStart = System.nanoTime()
    val pqnLogisticResult = ProjectedQuasiNewton(problemSize, SPARSE, owlqnL1Obj).minimizeAndReturnState(elasticNetLoss,init)
    val pqnLogisticTime = System.nanoTime() - pqnLogisticStart
    val pqnLogisticObj = elasticNetLoss.calculate(pqnLogisticResult.x)._1

    val nlLogisticStart = System.nanoTime()
    val nlLogisticResult = nlSparse.iterationsADMM(elasticNetLoss, 2.0)
    val nlLogisticTime = System.nanoTime() - nlLogisticStart
    val nlLogisticObj = elasticNetLoss.calculate(nlLogisticResult.x)._1 + lambdaL1 * nlLogisticResult.x.foldLeft(0.0) { (agg, entry) => agg + abs(entry)}

    println(s"owlqn ${owlqnLogisticTime / 1e6} ms iters ${owlqnLogisticResult.iter} pqn ${pqnLogisticTime / 1e6} ms iters ${pqnLogisticResult.iter}")
    println(s"nl ${nlLogisticTime/1e6} ms iters ${nlLogisticResult.iterations}")
    println(s"objective owlqnLogistic $owlqnLogisticObj L1 $owlqnL1Obj pqnLogistic $pqnLogisticObj nlLogistic objective $nlLogisticObj")

    println("Linear Regression with Bounds")
    init := 0.0

    val pqnBox = ProjectedQuasiNewton(problemSize, BOX)
    val pqnBoxStart = System.nanoTime()
    val pqnBoxResult = pqnBox.minimizeAndReturnState(quadraticCost, init)
    val pqnBoxTime = System.nanoTime() - pqnBoxStart
    val pqnBoxObj = QuadraticMinimizer.computeObjective(h, q, pqnBoxResult.x)

    val qpBox = QuadraticMinimizer(problemSize, BOX)
    val qpBoxStart = System.nanoTime()
    val qpBoxResult = qpBox.minimizeAndReturnState(h, q)
    val qpBoxTime = System.nanoTime() - qpBoxStart
    val qpBoxObj = QuadraticMinimizer.computeObjective(h, q, qpBoxResult.x)

    println(s"qpBox ${qpBoxTime/1e6} ms iters ${qpBoxResult.iter}")
    println(s"pqnBox ${pqnBoxTime/1e6} ms iters ${pqnBoxResult.iter}")
    println(s"qpBoxObj $qpBoxObj pqnBoxObj $pqnBoxObj")

    println("Logistic Regression with Bounds")

    val pqnBoxLogisticStart = System.nanoTime()
    val pqnBoxLogisticResult = pqnBox.minimizeAndReturnState(elasticNetLoss, init)
    val pqnBoxLogisticTime = System.nanoTime() - pqnBoxLogisticStart

    println(s"pqnBox ${pqnBoxLogisticTime/1e6} ms iters ${pqnBoxLogisticResult.iter}")
    println(s"pqn loss ${elasticNetLoss.calculate(pqnBoxLogisticResult.x)._1}")
  }
}