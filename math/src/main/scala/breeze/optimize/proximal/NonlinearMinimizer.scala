package breeze.optimize.proximal

import breeze.linalg.{DenseMatrix, DenseVector, norm}
import breeze.math.MutableInnerProductModule
import breeze.optimize._
import breeze.optimize.proximal.Constraint._
import breeze.util.SerializableLogging
import scala.math._
import scala.math.pow
import scala.math.sqrt

/**
 * NonlinearMinimizer extends Project Quasi Newton method to Proximal Quasi Newton method
 *
 * The idea has been proposed in multiple papers but our flow is adapted from Lee et al's paper on
 * Proximal Newton-type methods for minimizing composite functions http://arxiv.org/abs/1206.1623
 *
 * Currently two inner optimizers are implemented
 *
 * 1. SpaRSA as the inner optimizer which is line search based
 * 2. History preserving ADMM as the inner optimizer which uses fixed rho
 *
 * TO DO:
 *
 * Implement FISTA from TFOCS (L parameter) and compare against SpaRSA/ADMM based innerOptimizer
 *
 * @author debasish83
 */
class NonlinearMinimizer(val proximal: Proximal,
                         val admm: Boolean = false,
                         tolerance: Double = 1e-6,
                         val m: Int = 10,
                         val initFeas: Boolean = false,
                         val testOpt: Boolean = true,
                         val maxNumIt: Int = 500,
                         val maxSrchIt: Int = 50,
                         val gamma: Double = 1e-4)
                        (implicit space: MutableInnerProductModule[DenseVector[Double],Double])
  extends FirstOrderMinimizer[DenseVector[Double], DiffFunction[DenseVector[Double]]](maxIter = maxNumIt, tolerance = tolerance) with SerializableLogging {

  val innerOptimizer = new SpectralProximalGradient[DenseVector[Double], DiffFunction[DenseVector[Double]]](
      proximal,
      testOpt = true,
      tolerance = tolerance,
      maxIter = 500,
      initFeas = true,
      minImprovementWindow = 10,
      gamma = gamma)

  type BDV = DenseVector[Double]

  type History = CompactHessian

  protected def initialHistory(f: DiffFunction[DenseVector[Double]], init: DenseVector[Double]): History = {
    new CompactHessian(m)
  }

  override protected def adjust(newX: DenseVector[Double], newGrad: DenseVector[Double], newVal: Double):(Double,DenseVector[Double]) = (newVal,-newGrad)

  private def computeGradient(x: DenseVector[Double], g: DenseVector[Double]): DenseVector[Double] = -g

  private def computeGradientNorm(x: DenseVector[Double], g: DenseVector[Double]): Double = norm(computeGradient(x, g),Double.PositiveInfinity)

  protected def chooseDescentDirection(state: State, fn: DiffFunction[DenseVector[Double]]): DenseVector[Double] = {
    import state._
    if (iter == 0) {
      computeGradient(x, grad)
    } else {
      // Update the limited-memory BFGS approximation to the Hessian
      //B.update(y, s)
      // Solve subproblem; we use the current iterate x as a guess
      val subprob = new ProjectedQuasiNewton.QuadraticSubproblem(state.adjustedValue, x, grad, history)
      val d = if (admm) {
        val p = iterationsADMMWithHistory(new CachedDiffFunction(subprob), x)
        p - x
      } else {
        val p = innerOptimizer.minimize(new CachedDiffFunction(subprob), x)
        p - x
        //	time += subprob.time
      }
      d
    }
  }

  protected def determineStepSize(state: State, fn: DiffFunction[DenseVector[Double]], dir: DenseVector[Double]): Double = {
    import state._

    if (state.iter == 0) {
      val t = scala.math.min(1.0, 1.0 / norm(state.grad, 1.0))
      val fOld = fVals.max
      val compyOld = fOld + proximal.valueAt(x)
      val lambda =  SpectralProximalGradient.curvtrack[BDV, DiffFunction[BDV]](fn, proximal, x, grad, grad, t, fVals.max, compyOld,
                                                                               gamma, maxSrchIt, testOpt, logger)
      return lambda
    }

    val dirnorm = norm(dir, Double.PositiveInfinity)
    if(dirnorm < 1E-10) return 0.0

    // Backtracking line-search
    var accepted = false
    var lambda = 1.0
    val y = x + dir
    val gTd = grad dot dir + (proximal.valueAt(y) - proximal.valueAt(x))

    var srchit = 0

    do {
      val candx = x + dir * lambda
      val candf = fn.valueAt(candx) + proximal.valueAt(candx)

      val suffdec = gamma * lambda * gTd

      if (testOpt && srchit > 0) {
        logger.debug(f"PQN:    SrchIt $srchit%4d: f $candf%-10.4f t $lambda%-10.4f\n")
      }

      if (candf < state.adjustedValue + suffdec) {
        accepted = true
      } else if (srchit >= maxSrchIt) {
        accepted = true
      } else {
        lambda *= 0.5
        srchit = srchit + 1
      }
    } while (!accepted)

    if (srchit >= maxSrchIt) {
      logger.info("PQN: Line search cannot make further progress")
      throw new LineSearchFailed(norm(state.grad,Double.PositiveInfinity), norm(dir, Double.PositiveInfinity))
    }
    lambda
  }

  protected def takeStep(state: State, dir: DenseVector[Double], stepSize: Double): DenseVector[Double] = {
    state.x + dir*stepSize
  }

  protected def updateHistory(newX: DenseVector[Double], newGrad: DenseVector[Double], newVal: Double,  f: DiffFunction[DenseVector[Double]], oldState: State): History = {
    import oldState._
    val s = newX - x
    val y = newGrad - grad
    oldState.history.updated(y, s)
  }

  //TO DO : alpha needs to be scaled based on Nesterov's acceleration
  val alpha: Double = 1.0

  val ABSTOL = 1e-4
  val RELTOL = 1e-2

  val quadraticIters = 10

  def getProximal = proximal

  def iterationsADMMWithHistory(primal: DiffFunction[DenseVector[Double]], x: DenseVector[Double], rho: Double = 1.0) : DenseVector[Double] = {
    val innerIters = 1
    val ndim = x.length

    val admmIters = math.max(1000, 40 * ndim)

    val primalSolve = new LBFGS[DenseVector[Double]](10, m)

    val z = DenseVector.zeros[Double](ndim)
    val u = DenseVector.zeros[Double](ndim)

    var initialState = primalSolve.minimizeAndReturnState(primal, x)
    val proxPrimal = NonlinearMinimizer.ProximalPrimal(primal, u, z, rho)

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

      if (residualNorm < epsPrimal && sNorm < epsDual) { return resultState.x }
      k = k + 1
      initialState = resultState
      initialState.grad += (u - z:*rho)
    }
    initialState.x
  }
}

object NonlinearMinimizer {
  /*
    Proximal modifications to Primal algorithm
    AdmmObj(x, u, z) = f(x) + u'(x-z) + rho/2*||x - z||^{2}
    dAdmmObj/dx = df/dx + u + rho(x - z)
  */
  case class ProximalPrimal(primal: DiffFunction[DenseVector[Double]],
                            u: DenseVector[Double], z: DenseVector[Double],
                            rho: Double) extends DiffFunction[DenseVector[Double]] {
    override def calculate(x: DenseVector[Double]) = {
      val (f, g) = primal.calculate(x)
      val proxObj = f + u.dot(x - z) + 0.5 * rho * pow(norm(x - z), 2)
      val proxGrad = g + u + (x - z) :* rho
      (proxObj, proxGrad)
    }
  }

  def apply(ndim: Int, constraint: Constraint=IDENTITY, lambda: Double=1.0): NonlinearMinimizer = {
    constraint match {
      case IDENTITY => new NonlinearMinimizer(ProjectIdentity())
      case POSITIVE => new NonlinearMinimizer(ProjectPos())
      case BOX => {
        val lb = DenseVector.zeros[Double](ndim)
        val ub = DenseVector.ones[Double](ndim)
        new NonlinearMinimizer(ProjectBox(lb, ub))
      }
      case EQUALITY => {
        val aeq = DenseVector.ones[Double](ndim)
        new NonlinearMinimizer(ProjectHyperPlane(aeq, 1.0))
      }
      case PROBABILITYSIMPLEX => new NonlinearMinimizer(ProjectProbabilitySimplex(lambda))
      case SPARSE => new NonlinearMinimizer(ProximalL1().setLambda(lambda))
      case _ => throw new IllegalArgumentException("NonlinearMinimizer does not support the Proximal Operator")
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
    val init = DenseVector.zeros[Double](problemSize)

    val regularizedGram = h + (DenseMatrix.eye[Double](h.rows) :* lambdaL2)

    val sparseQp = QuadraticMinimizer(h.rows, SPARSE, lambdaL1)
    val sparseQpStart = System.nanoTime()
    val sparseQpResult = sparseQp.minimizeAndReturnState(regularizedGram, q)
    val sparseQpTime = System.nanoTime() - sparseQpStart

    val owlqnStart = System.nanoTime()
    val owlqnResult = QuadraticMinimizer.optimizeWithOWLQN(init, regularizedGram, q, lambdaL1)
    val owlqnTime = System.nanoTime() - owlqnStart

    println("ElasticNet Formulation")

    println("Linear Regression")

    val owlqnObj = QuadraticMinimizer.computeObjective(regularizedGram, q, owlqnResult.x) + lambdaL1 * owlqnResult.x.foldLeft(0.0) { (agg, entry) => agg + abs(entry)}
    val sparseQpL1Obj = sparseQpResult.x.foldLeft(0.0) { (agg, entry) => agg + abs(entry)}
    val sparseQpObj = QuadraticMinimizer.computeObjective(regularizedGram, q, sparseQpResult.x) + lambdaL1 * sparseQpL1Obj
    val quadraticCostWithL2 = QuadraticMinimizer.Cost(regularizedGram, q)

    init := 0.0
    val nlSparseStart = System.nanoTime()
    val nlSparseResult = NonlinearMinimizer(problemSize, SPARSE, lambdaL1).minimizeAndReturnState(quadraticCostWithL2, init)
    val nlSparseTime = System.nanoTime() - nlSparseStart
    val nlSparseL1Obj = nlSparseResult.x.foldLeft(0.0) { (agg, entry) => agg + abs(entry)}
    val nlSparseObj = QuadraticMinimizer.computeObjective(regularizedGram, q, nlSparseResult.x) + nlSparseL1Obj

    init := 0.0
    println(s"owlqn ${owlqnTime / 1e6} ms iters ${owlqnResult.iter} sparseQp ${sparseQpTime / 1e6} ms iters ${sparseQpResult.iter}")
    println(s"nlSparseTime ${nlSparseTime / 1e6} ms iters ${nlSparseResult.iter}")
    println(s"owlqnObj $owlqnObj sparseQpObj $sparseQpObj nlSparseObj $nlSparseObj")

    println("Logistic Regression")

    val logisticLoss = LogisticGenerator(problemSize)
    val elasticNetLoss = DiffFunction.withL2Regularization(logisticLoss, lambdaL2)

    val owlqn = new OWLQN[Int, DenseVector[Double]](-1, 7, lambdaL1)

    init := 0.0
    val owlqnLogisticStart = System.nanoTime()
    val owlqnLogisticResult = owlqn.minimizeAndReturnState(elasticNetLoss, init)
    val owlqnLogisticTime = System.nanoTime() - owlqnLogisticStart
    val owlqnL1Obj = owlqnLogisticResult.x.foldLeft(0.0) { (agg, entry) => agg + abs(entry)}
    val owlqnLogisticObj = elasticNetLoss.calculate(owlqnLogisticResult.x)._1 + lambdaL1*owlqnL1Obj

    init := 0.0
    val nlLogisticStart = System.nanoTime()
    val nlLogisticResult = NonlinearMinimizer(problemSize, SPARSE, lambdaL1).minimizeAndReturnState(elasticNetLoss, init)
    val nlLogisticTime = System.nanoTime() - nlLogisticStart
    val nlLogisticObj = elasticNetLoss.calculate(nlLogisticResult.x)._1

    println(s"owlqn ${owlqnLogisticTime / 1e6} ms iters ${owlqnLogisticResult.iter} pqn ${nlLogisticTime / 1e6} ms iters ${nlLogisticResult.iter}")
    println(s"objective owlqnLogistic $owlqnLogisticObj L1 $owlqnL1Obj nlLogistic $nlLogisticObj")
  }
}
