package breeze.optimize.proximal

import breeze.linalg.{norm, DenseVector}
import breeze.math.MutableInnerProductModule
import breeze.optimize._
import breeze.optimize.proximal.LinearGenerator.Cost
import breeze.util.SerializableLogging
import breeze.optimize.proximal.Constraint._
import scala.math._
import breeze.linalg.DenseMatrix
import breeze.linalg.sum
import breeze.linalg.max
import breeze.util.Implicits._

/**
 *
 * NonlinearMinimizer solves the problem that has the following structure
 * minimize f(x) + g(x)
 *
 * g(x) represents the following constraints
 *
 * 1. x >= 0
 * 2. lb <= x <= ub
 * 3. L1(x)
 * 4. Aeq*x = beq
 * 5. aeq'x = beq
 * 6. 1'x = s, x >= 0 ProbabilitySimplex from the reference Proximal Algorithms by Boyd et al, Duchi et al
 *
 * f(x) can be a smooth convex function defined by DiffFunction or a proximal operator. For now the exposed API
 * takes DiffFunction
 *
 * g(x) is defined by a proximal operator
 *
 * For proximal algorithms like L1 through soft-thresholding and Huber Loss (look into library of proximal
 * algorithms for further details) we provide ADMM based Proximal algorithm based on the following
 * reference: https://web.stanford.edu/~boyd/papers/admm/logreg-l1/distr_l1_logreg.html
 *
 * A subset of proximal operators are projection operators. For projection, NonlinearMinimizer companion
 * object provides project API which generates a Spectral Projected Gradient (SPG) or Projected Quasi Newton (PQN)
 * solver. For projection operators like positivity, bounds, probability simplex etc, these algorithms converges
 * faster as compared to ADMM based proximal algorithm.
 *
 * TO DO
 *
 * 1. Implement FISTA / Nesterov's accelerated method and compare with ADMM
 * 2. For non-convex function experiment with TRON-like Primal solver
 *
 * @author debasish83
 */
class NonlinearMinimizer(
    proximal: Proximal,
    maxIters: Int = -1,
    innerIters: Int = 3,
    bfgsMemory: Int = 7,
    rho: Double = 1.0,
    alpha: Double = 1.0,
    abstol: Double = 1e-6,
    reltol: Double = 1e-4)
    extends SerializableLogging {
  import NonlinearMinimizer.BDV

  val lbfgs = new LBFGS[BDV](m = bfgsMemory, tolerance = abstol, maxIter = innerIters)

  case class State private[NonlinearMinimizer] (
      bfgsState: lbfgs.State,
      u: BDV,
      z: BDV,
      xHat: BDV,
      zOld: BDV,
      residual: BDV,
      s: BDV,
      admmIters: Int,
      iter: Int,
      converged: Boolean)

  private def initialState(primal: DiffFunction[BDV], init: BDV) = {
    val z = init.copy
    val u = init.copy

    val xHat = init.copy
    val zOld = init.copy

    val residual = init.copy
    val s = init.copy

    val resultState = lbfgs.minimizeAndReturnState(primal, xHat)
    val admmIters = if (maxIters < 0) max(400, 20 * z.length) else maxIters
    State(resultState, u, z, xHat, zOld, residual, s, admmIters, 0, false)
  }

  def iterations(primal: DiffFunction[BDV], init: BDV): Iterator[State] =
    Iterator
      .iterate(initialState(primal, init)) { state =>
        import state._

        val scale = sqrt(init.size) * abstol
        val proxPrimal = NonlinearMinimizer.ProximalPrimal(primal, u, z, rho)

        val resultState = lbfgs.minimizeAndReturnState(proxPrimal, bfgsState.x)
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
        val residualNorm = norm(residual)

        //history.s_norm(k) = norm(-rho*(z - zold))
        s := z
        s -= zOld
        s *= -rho
        val sNorm = norm(s)

        //TO DO : Make sure z.muli(-1) is actually needed in norm calculation
        residual := z
        residual *= -1.0

        //s = rho*u
        s := u
        s *= rho

        val epsPrimal = scale + reltol * max(norm(resultState.x), norm(residual))
        val epsDual = scale + reltol * norm(s)

        if (residualNorm < epsPrimal && sNorm < epsDual || iter > admmIters) {
          State(resultState, u, z, xHat, zOld, residual, s, admmIters, iter + 1, true)
        } else {
          State(resultState, u, z, xHat, zOld, residual, s, admmIters, iter + 1, false)
        }
      }
      .takeUpToWhere { _.converged }

  def minimize(primal: DiffFunction[BDV], init: BDV): BDV = {
    minimizeAndReturnState(primal, init).z
  }

  def minimizeAndReturnState(primal: DiffFunction[BDV], init: BDV): State = {
    iterations(primal, init).last
  }
}

object NonlinearMinimizer {
  type BDV = DenseVector[Double]

  /**
   * Proximal modifications to Primal algorithm for scaled ADMM formulation
   * AdmmObj(x, u, z) = f(x) + rho/2*||x - z + u||2
   * dAdmmObj/dx = df/dx + rho*(x - z + u)
   */
  case class ProximalPrimal[T](primal: DiffFunction[T], u: T, z: T, rho: Double)(
      implicit space: MutableInnerProductModule[T, Double])
      extends DiffFunction[T] {

    import space._

    override def calculate(x: T) = {
      val (f, g) = primal.calculate(x)
      val scale = x - z + u
      val proxObj = f + 0.5 * rho * pow(norm(scale), 2)
      val proxGrad = g + scale *:* rho
      (proxObj, proxGrad)
    }
  }

  case class Projection(proximal: Proximal) {
    def project(x: BDV): BDV = {
      proximal.prox(x)
      x
    }
  }

  /**
   * A subset of proximal operators can be represented as Projection operators and for those
   * operators, we give an option to the user to choose a projection based algorithm. The options
   * available for users are SPG (Spectral Projected Gradient) and PQN (Projected Quasi Newton)
   *
   * @param proximal operator that defines proximal algorithm
   * @return FirstOrderMinimizer to optimize on f(x) and proximal operator
   */
  def project(
      proximal: Proximal,
      maxIter: Int = -1,
      m: Int = 10,
      tolerance: Double = 1e-6,
      usePQN: Boolean = false): FirstOrderMinimizer[BDV, DiffFunction[BDV]] = {
    val projectionOp = Projection(proximal)
    if (usePQN) new ProjectedQuasiNewton(projection = projectionOp.project, tolerance = 1e-6, maxIter = maxIter, m = m)
    else
      new SpectralProjectedGradient(
        projection = projectionOp.project,
        tolerance = tolerance,
        maxIter = maxIter,
        bbMemory = m)
  }

  /**
   * A compansion object to generate projection based minimizer that can use SPG/PQN as the solver
   * @param ndim the problem dimension
   * @param constraint one of the available constraint, possibilities are x>=0; lb<=x<=ub;aeq*x = beq;
   *                   1'x = s, x >= 0; ||x||1 <= s
   * @param lambda the regularization parameter for most of the constraints
   * @return FirstOrderMinimizer to optimize on f(x) and proximal operator
   */
  def apply(
      ndim: Int,
      constraint: Constraint,
      lambda: Double,
      usePQN: Boolean = false): FirstOrderMinimizer[BDV, DiffFunction[BDV]] = {
    constraint match {
      case IDENTITY => project(ProjectIdentity())
      case POSITIVE => project(ProjectPos())
      case BOX => {
        val lb = DenseVector.zeros[Double](ndim)
        val ub = DenseVector.ones[Double](ndim)
        project(ProjectBox(lb, ub))
      }
      case EQUALITY => {
        val aeq = DenseVector.ones[Double](ndim)
        project(ProjectHyperPlane(aeq, 1.0))
      }
      case PROBABILITYSIMPLEX => project(ProjectProbabilitySimplex(lambda))
      case SPARSE => project(ProjectL1(lambda))
      case _ => throw new IllegalArgumentException("NonlinearMinimizer does not support the Projection Operator")
    }
  }

  def main(args: Array[String]) {
    if (args.length < 3) {
      println("Usage: ProjectedQuasiNewton n lambda beta")
      println(
        "Test NonlinearMinimizer with a quadratic function of dimenion n and m equalities with lambda beta for elasticNet")
      sys.exit(1)
    }

    val problemSize = args(0).toInt
    val lambda = args(1).toDouble
    val beta = args(2).toDouble

    println(s"Generating Linear and Logistic Loss with rank ${problemSize}")

    val (quadraticCost, h, q) = LinearGenerator(problemSize)

    val lambdaL1 = lambda * beta
    val lambdaL2 = lambda * (1 - beta)

    val owlqn = new OWLQN[Int, DenseVector[Double]](-1, 10, lambdaL1, 1e-6)

    val regularizedGram = h + (DenseMatrix.eye[Double](h.rows) *:* lambdaL2)

    val sparseQp = QuadraticMinimizer(h.rows, SPARSE, lambdaL1)
    val sparseQpStart = System.nanoTime()
    val sparseQpResult = sparseQp.minimizeAndReturnState(regularizedGram, q)
    val sparseQpTime = System.nanoTime() - sparseQpStart

    val init = DenseVector.zeros[Double](problemSize)
    val owlqnStart = System.nanoTime()
    val owlqnResult = owlqn.minimizeAndReturnState(Cost(regularizedGram, q), init)
    val owlqnTime = System.nanoTime() - owlqnStart

    println("ElasticNet Formulation")

    println("Linear Regression")

    val owlqnObj = QuadraticMinimizer.computeObjective(regularizedGram, q, owlqnResult.x) + lambdaL1 * owlqnResult.x
      .foldLeft(0.0) { (agg, entry) =>
        agg + abs(entry)
      }
    val sparseQpL1Obj = sparseQpResult.x.foldLeft(0.0) { (agg, entry) =>
      agg + abs(entry)
    }
    val sparseQpObj = QuadraticMinimizer.computeObjective(regularizedGram, q, sparseQpResult.x) + lambdaL1 * sparseQpL1Obj
    val quadraticCostWithL2 = QuadraticMinimizer.Cost(regularizedGram, q)

    init := 0.0
    val projectL1Linear = ProjectL1(sparseQpL1Obj)
    val nlSparseStart = System.nanoTime()
    val nlSparseResult = NonlinearMinimizer.project(projectL1Linear).minimizeAndReturnState(quadraticCostWithL2, init)
    val nlSparseTime = System.nanoTime() - nlSparseStart
    val nlSparseL1Obj = nlSparseResult.x.foldLeft(0.0) { (agg, entry) =>
      agg + abs(entry)
    }
    val nlSparseObj = QuadraticMinimizer.computeObjective(regularizedGram, q, nlSparseResult.x) + lambdaL1 * nlSparseL1Obj

    init := 0.0
    val nlProx = new NonlinearMinimizer(proximal = ProximalL1(lambdaL1))
    val nlProxStart = System.nanoTime()
    val nlProxResult = nlProx.minimizeAndReturnState(quadraticCostWithL2, init)
    val nlProxTime = System.nanoTime() - nlProxStart
    val nlProxObj = QuadraticMinimizer.computeObjective(regularizedGram, q, nlProxResult.z) + lambdaL1 * nlProxResult.z
      .foldLeft(0.0) { (agg, entry) =>
        agg + abs(entry)
      }

    println(
      s"owlqn ${owlqnTime / 1e6} ms iters ${owlqnResult.iter} sparseQp ${sparseQpTime / 1e6} ms iters ${sparseQpResult.iter}")
    println(s"nlSparseTime ${nlSparseTime / 1e6} ms iters ${nlSparseResult.iter}")
    println(s"nlProxTime ${nlProxTime / 1e6} ms iters ${nlProxResult.iter}")
    println(s"owlqnObj $owlqnObj sparseQpObj $sparseQpObj nlSparseObj $nlSparseObj nlProxObj $nlProxObj")

    val logisticLoss = LogisticGenerator(problemSize)
    val elasticNetLoss = DiffFunction.withL2Regularization(logisticLoss, lambdaL2)

    println("Linear Regression with Bounds")

    init := 0.0
    val nlBox = NonlinearMinimizer(problemSize, BOX, 0.0)

    val nlBoxStart = System.nanoTime()
    val nlBoxResult = nlBox.minimizeAndReturnState(quadraticCostWithL2, init)
    val nlBoxTime = System.nanoTime() - nlBoxStart
    val nlBoxObj = quadraticCostWithL2.calculate(nlBoxResult.x)._1

    val qpBox = QuadraticMinimizer(problemSize, BOX, 0.0)
    val qpBoxStart = System.nanoTime()
    val qpBoxResult = qpBox.minimizeAndReturnState(regularizedGram, q)
    val qpBoxTime = System.nanoTime() - qpBoxStart
    val qpBoxObj = QuadraticMinimizer.computeObjective(regularizedGram, q, qpBoxResult.x)

    println(s"qpBox ${qpBoxTime / 1e6} ms iters ${qpBoxResult.iter}")
    println(s"nlBox ${nlBoxTime / 1e6} ms iters ${nlBoxResult.iter}")
    println(s"qpBoxObj $qpBoxObj nlBoxObj $nlBoxObj")

    println("Logistic Regression with Bounds")

    init := 0.0
    val nlBoxLogisticStart = System.nanoTime()
    val nlBoxLogisticResult = nlBox.minimizeAndReturnState(elasticNetLoss, init)
    val nlBoxLogisticTime = System.nanoTime() - nlBoxLogisticStart
    val nlBoxLogisticObj = elasticNetLoss.calculate(nlBoxLogisticResult.x)._1
    println(s"Objective nl ${nlBoxLogisticObj} time ${nlBoxLogisticTime / 1e6} ms")

    println("Linear Regression with ProbabilitySimplex")

    val nlSimplex = NonlinearMinimizer(problemSize, PROBABILITYSIMPLEX, 1.0)

    init := 0.0
    val nlSimplexStart = System.nanoTime()
    val nlSimplexResult = nlSimplex.minimizeAndReturnState(quadraticCost, init)
    val nlSimplexTime = System.nanoTime() - nlSimplexStart
    val nlSimplexObj = quadraticCost.calculate(nlSimplexResult.x)._1

    val qpSimplexStart = System.nanoTime()
    val qpSimplexResult = QuadraticMinimizer(problemSize, EQUALITY).minimizeAndReturnState(h, q)
    val qpSimplexTime = System.nanoTime() - qpSimplexStart
    val qpSimplexObj = quadraticCost.calculate(qpSimplexResult.x)._1

    println(s"Objective nl $nlSimplexObj qp $qpSimplexObj")
    println(s"Constraint nl ${sum(nlSimplexResult.x)} qp ${sum(qpSimplexResult.x)}")
    println(s"time nl ${nlSimplexTime / 1e6} ms qp ${qpSimplexTime / 1e6} ms")

    println("Logistic Regression with ProbabilitySimplex")

    init := 0.0
    val nlLogisticSimplexStart = System.nanoTime()
    val nlLogisticSimplexResult = nlSimplex.minimizeAndReturnState(elasticNetLoss, init)
    val nlLogisticSimplexTime = System.nanoTime() - nlLogisticSimplexStart
    val nlLogisticSimplexObj = elasticNetLoss.calculate(nlLogisticSimplexResult.x)._1

    init := 0.0

    val nlProxSimplex = new NonlinearMinimizer(proximal = ProjectProbabilitySimplex(1.0))
    val nlProxLogisticSimplexStart = System.nanoTime()
    val nlProxLogisticSimplexResult = nlProxSimplex.minimizeAndReturnState(elasticNetLoss, init)
    val nlProxLogisticSimplexTime = System.nanoTime() - nlProxLogisticSimplexStart
    val nlProxLogisticSimplexObj = elasticNetLoss.calculate(nlProxLogisticSimplexResult.z)._1

    println(s"Objective nl ${nlLogisticSimplexObj} admm ${nlProxLogisticSimplexObj}")
    println(s"Constraint nl ${sum(nlLogisticSimplexResult.x)} admm ${sum(nlProxLogisticSimplexResult.z)}")
    println(s"time nlProjection ${nlLogisticSimplexTime / 1e6} ms nlProx ${nlProxLogisticSimplexTime / 1e6} ms")

    println("Logistic Regression with ProximalL1 and ProjectL1")

    init := 0.0
    val owlqnLogisticStart = System.nanoTime()
    val owlqnLogisticResult = owlqn.minimizeAndReturnState(elasticNetLoss, init)
    val owlqnLogisticTime = System.nanoTime() - owlqnLogisticStart
    val owlqnLogisticObj = elasticNetLoss.calculate(owlqnLogisticResult.x)._1
    val s = owlqnLogisticResult.x.foldLeft(0.0) { case (agg, entry) => agg + abs(entry) }

    init := 0.0
    val proximalL1 = ProximalL1().setLambda(lambdaL1)
    val nlLogisticProximalL1Start = System.nanoTime()
    val nlLogisticProximalL1Result = new NonlinearMinimizer(proximalL1).minimizeAndReturnState(elasticNetLoss, init)
    val nlLogisticProximalL1Time = System.nanoTime() - nlLogisticProximalL1Start

    init := 0.0
    val nlLogisticProjectL1Start = System.nanoTime()
    val nlLogisticProjectL1Result =
      NonlinearMinimizer(problemSize, SPARSE, s).minimizeAndReturnState(elasticNetLoss, init)
    val nlLogisticProjectL1Time = System.nanoTime() - nlLogisticProjectL1Start

    val proximalL1Obj = elasticNetLoss.calculate(nlLogisticProximalL1Result.z)._1
    val projectL1Obj = elasticNetLoss.calculate(nlLogisticProjectL1Result.x)._1

    println(s"Objective proximalL1 $proximalL1Obj projectL1 $projectL1Obj owlqn $owlqnLogisticObj")
    println(
      s"time proximalL1 ${nlLogisticProximalL1Time / 1e6} ms projectL1 ${nlLogisticProjectL1Time / 1e6} ms owlqn ${owlqnLogisticTime / 1e6} ms")
  }
}
