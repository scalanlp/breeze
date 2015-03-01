package breeze.optimize.proximal

import breeze.linalg.{sum, DenseMatrix, DenseVector}
import breeze.optimize.{FirstOrderMinimizer, OWLQN, DiffFunction, ProjectedQuasiNewton}
import breeze.optimize.proximal.Constraint._
import breeze.util.SerializableLogging
import scala.math._

/**
 * Proximal wrapper over PQN and OWLQN solvers, right now only Projection operators are supported
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
 * 6. 1'x = 1, x >= 0 ProbabilitySimplex from the reference Proximal Algorithms by Boyd et al.
 *
 * f(x) can be either a convex or a non-linear function.
 *
 * For convex functions use B matrix (m) with 7/10 columns, for non-convex function use B matrix with 1-3 columns
 *
 * TO DO : For non-convex function experiment with TRON-like Primal solver
 * TO DO : PQN and OWLQN will be merged using a Proximal Quasi Newton solver
 *
 * @author debasish83
 */
object NonlinearMinimizer extends SerializableLogging {
  type BDV = DenseVector[Double]

  case class Projection(proximal: Proximal) {
    def project(x: DenseVector[Double]) : DenseVector[Double] = {
      proximal.prox(x)
      x
    }
  }

  def proximal(proximal: Proximal, maxIter: Int = -1, m: Int=10, tolerance: Double=1E-7) : FirstOrderMinimizer[BDV, DiffFunction[BDV]] = {
    proximal match {
      case ProximalL1() => new OWLQN[Int, DenseVector[Double]](maxIter, m, proximal.asInstanceOf[ProximalL1].lambda, tolerance)
      case _ => new ProjectedQuasiNewton(projection = Projection(proximal).project)
    }
  }

  def apply(ndim: Int, constraint: Constraint=IDENTITY, lambda: Double=1.0): FirstOrderMinimizer[BDV, DiffFunction[BDV]] = {
    constraint match {
      case IDENTITY => proximal(ProjectIdentity())
      case POSITIVE => proximal(ProjectPos())
      case BOX => {
        val lb = DenseVector.zeros[Double](ndim)
        val ub = DenseVector.ones[Double](ndim)
        proximal(ProjectBox(lb, ub))
      }
      case EQUALITY => {
        val aeq = DenseVector.ones[Double](ndim)
        proximal(ProjectHyperPlane(aeq, 1.0))
      }
      case PROBABILITYSIMPLEX => proximal(ProjectProbabilitySimplex(lambda))
      case SPARSE => proximal(ProximalL1().setLambda(lambda))
      case _ => throw new IllegalArgumentException("NonlinearMinimizer does not support the Proximal Operator")
    }
  }

  def main(args: Array[String]) {
    if (args.length < 3) {
      println("Usage: ProjectedQuasiNewton n lambda beta")
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

    val init = DenseVector.zeros[Double](problemSize)

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
    val nlSparseObj = QuadraticMinimizer.computeObjective(regularizedGram, q, nlSparseResult.x) + lambdaL1*nlSparseL1Obj

    init := 0.0
    println(s"owlqn ${owlqnTime / 1e6} ms iters ${owlqnResult.iter} sparseQp ${sparseQpTime / 1e6} ms iters ${sparseQpResult.iter}")
    println(s"nlSparseTime ${nlSparseTime / 1e6} ms iters ${nlSparseResult.iter}")
    println(s"owlqnObj $owlqnObj sparseQpObj $sparseQpObj nlSparseObj $nlSparseObj")

    val logisticLoss = LogisticGenerator(problemSize)
    val elasticNetLoss = DiffFunction.withL2Regularization(logisticLoss, lambdaL2)

    println("Linear Regression with Bounds")

    init := 0.0
    val nlBox = NonlinearMinimizer(problemSize, BOX)

    val nlBoxStart = System.nanoTime()
    val nlBoxResult = nlBox.minimizeAndReturnState(quadraticCostWithL2, init)
    val nlBoxTime = System.nanoTime() - nlBoxStart
    val nlBoxObj = quadraticCostWithL2.calculate(nlBoxResult.x)._1

    val qpBox = QuadraticMinimizer(problemSize, BOX)
    val qpBoxStart = System.nanoTime()
    val qpBoxResult = qpBox.minimizeAndReturnState(regularizedGram, q)
    val qpBoxTime = System.nanoTime() - qpBoxStart
    val qpBoxObj = QuadraticMinimizer.computeObjective(regularizedGram, q, qpBoxResult.x)

    println(s"qpBox ${qpBoxTime / 1e6} ms iters ${qpBoxResult.iter}")
    println(s"pqnBox ${nlBoxTime / 1e6} ms iters ${nlBoxResult.iter}")
    println(s"qpBoxObj $qpBoxObj nlBoxObj $nlBoxObj")

    println("Logistic Regression with Bounds")

    init := 0.0
    val nlBoxLogisticStart = System.nanoTime()
    val nlBoxLogisticResult = nlBox.minimizeAndReturnState(elasticNetLoss, init)
    val nlBoxLogisticTime = System.nanoTime() - nlBoxLogisticStart
    val pqnBoxLogisticObj = elasticNetLoss.calculate(nlBoxLogisticResult.x)._1
    println(s"Objective pqn ${pqnBoxLogisticObj} time ${nlBoxLogisticTime / 1e6} ms")

    println("Linear Regression with ProbabilitySimplex")

    val nlSimplex = NonlinearMinimizer(problemSize, PROBABILITYSIMPLEX)

    init := 0.0
    val nlSimplexStart = System.nanoTime()
    val nlSimplexResult = nlSimplex.minimizeAndReturnState(quadraticCost, init)
    val nlSimplexTime = System.nanoTime() - nlSimplexStart
    val nlSimplexObj = quadraticCost.calculate(nlSimplexResult.x)._1

    val qpSimplexStart = System.nanoTime()
    val qpSimplexResult = QuadraticMinimizer(problemSize, EQUALITY).minimizeAndReturnState(h, q)
    val qpSimplexTime = System.nanoTime() - qpSimplexStart
    val qpSimplexObj = quadraticCost.calculate(qpSimplexResult.x)._1

    println(s"Objective pqn $nlSimplexObj qp $qpSimplexObj")
    println(s"Constraint pqn ${sum(nlSimplexResult.x)} qp ${sum(qpSimplexResult.x)}")
    println(s"time pqn ${nlSimplexTime / 1e6} ms qp ${qpSimplexTime / 1e6} ms")

    println("Logistic Regression with ProbabilitySimplex")

    init := 0.0
    val nlLogisticSimplexStart = System.nanoTime()
    val nlLogisticSimplexResult = nlSimplex.minimizeAndReturnState(elasticNetLoss, init)
    val nlLogisticSimplexTime = System.nanoTime() - nlLogisticSimplexStart
    val nlLogisticSimplexObj = elasticNetLoss.calculate(nlLogisticSimplexResult.x)._1

    println(s"Objective pqn ${nlLogisticSimplexObj}")
    println(s"Constraint pqn ${sum(nlLogisticSimplexResult.x)}")
    println(s"time pqn ${nlLogisticSimplexTime / 1e6} ms")
  }
}
