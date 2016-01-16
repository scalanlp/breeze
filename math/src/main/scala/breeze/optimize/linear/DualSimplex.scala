package breeze.optimize.linear

import breeze.numerics.{I, inf}
import breeze.linalg.{inv, scaleAdd, DenseVector, DenseMatrix}
import breeze.optimize.flow.{LPMaxFlow, FlowGraph}
import scalaxy.debug._

/**
  * http://d-nb.info/978580478/34
  * Created by dlwh on 12/23/15.
  */
class DualSimplex {

}

object DualSimplex {

  def minimize(cf: ComputationalForm) = {
    var firstBasis: Basis =  (cf.n - cf.m) until cf.n
    val init: State = State.init(cf, DenseVector.zeros(cf.n), firstBasis)
    if (!init.isDualFeasible) {
      val feasibilityProblem = feasibleBasisFindingProblem(cf)
      import feasibilityProblem._
      val improved = updateBasisBFRT(State.init(feasibilityProblem, feasibilityProblem.lowerBounds, firstBasis), 0.0, DenseVector.zeros(m))
//      println("Before solving feasibility:\n" + improved)
      val feasibleStart = solve(feasibilityProblem, improved.x, improved.basis)
//      println("found: " + feasibleStart)
      firstBasis = feasibleStart.basis
    }

    solve(cf, DenseVector.zeros(cf.n), firstBasis)
  }

  // algorithm 2 and algorithm 7
  def solve(cf: ComputationalForm, initX: DenseVector[Double], initBasis: Basis) = {
    var i = 0
    var state = State.init(cf, initX, initBasis)
    if (!state.isDualFeasible) {
      state = updateBasisBFRT(state, 0.0, DenseVector.zeros(state.m))
      state = state.copy(objective = state.x dot state.c)
    }

    assert(state.isDualFeasible, state.toString)
    while (!state.isBasisSolutionOptimal) {
      val cur = state
//      assert(cur.isDualFeasible, state.toString)
      import cur._
      //  step 2: pricing
      // p = basis(r) is the "leaving" variable, which is goint to become non-basic
      val r = basis.indexWhere(i => x(i) < cf.lowerBounds(i) || x(i) > cf.upperBounds(i))
      val p = basis(r)
      val delta = if (x(p) < lb(p)) x(p) - lb(p) else x(p) - ub(p)

      // step 3: BTran
      val rho_r = cur.A_B.t \ standardBasis(m, r)

      // step 4: pivot row
      val alpha_r = A_N.t * rho_r

      // step 5: ratio test
      val alpha_r_~ = if (x(p) < lb(p)) -alpha_r else alpha_r

      val candidates = state.nonBasis.zipWithIndex.filter{ case (j, pos) =>
        (problem.isFree(j) && alpha_r_~(pos) != 0.0) ||
          (atLowerBound(j) && alpha_r_~(pos) > 0) ||
          (atUpperBound(j) && alpha_r_~(pos) < 0)
      }.map { case (j, pos) => (j, pos, reducedCosts(j)/alpha_r_~(pos))}

      // q is the "entering" variable. it becomes a basis variable
      val (q, qpos, _) = candidates.minBy(_._3)
      //  the "dual step length": how far we can move in the direction of alpha_r without breaking dual feasibility
      val theta_D = reducedCosts(q) / alpha_r(qpos)

      // step 6: FTran
      val alpha_q = A_B \ A(::, q)

      // step 7: Basis change and update
//      println(s"swap $q for $p")

      val newZ = cur.objective + theta_D * delta

      val newY = scaleAdd.inPlace(y.copy, theta_D, rho_r)
      // the "primal step length": how far we're going to move x(q)
      val theta_P = delta / alpha_q(r)
      val newX = x.copy
      newX(q) += theta_P
      scaleAdd.inPlace(newX(basis), -theta_P, alpha_q)
      val newBasis = basis.updated(r, q)

      val newD = reducedCosts.copy
      newD(p) = -theta_D
      scaleAdd.inPlace(newD(nonBasis), -theta_D, alpha_r)
      newD(q) = 0

      state = cur.copy(y = newY, objective = newZ, x = newX, basis = newBasis, reducedCosts = newD)

//      if (i >= 5) ???
      i += 1
    }

    state
  }


  // 1 at i, 0 elsewhere
  private def standardBasis(len: Int, i: Int): DenseVector[Double] = {
    val e = DenseVector.zeros[Double](len)
    e(i) = 1.0
    e
  }

  type Basis = IndexedSeq[Int]
  // maximize x dot c s.t. A * x = b, lowerBounds <= x <= upperBounds
  case class ComputationalForm(c: DenseVector[Double],
                               A: DenseMatrix[Double], b: DenseVector[Double],
                               lowerBounds: DenseVector[Double], upperBounds: DenseVector[Double]) {
    def m = A.rows
    def n = A.cols
    def isFree (i: Int) = !hasLowerBound(i) && !hasUpperBound(i)
    def isFixed(i: Int) = lowerBounds(i) == upperBounds(i)
    def isBoxed(i: Int) = lowerBounds(i) > -inf && upperBounds(i) < inf
    def hasLowerBound(i: Int) = lowerBounds(i) > -inf
    def hasUpperBound(i: Int) = upperBounds(i) < inf

    val boxedVariables = (0 until n).filter(isBoxed)
    val unboxedVariables = (0 until n).filterNot(isBoxed)
  }

  case class State(problem: ComputationalForm,
                   x: DenseVector[Double],
                   y: DenseVector[Double],
                   basis: Basis,
                   objective: Double,
                   reducedCosts: DenseVector[Double]) {
    assert(x.length == n)
    assert(basis.length == m)
    assert(y.length == m)
    def m = A.rows
    def n = A.cols
    def A = problem.A
    def b = problem.b
    def c = problem.c
    def lb = problem.lowerBounds
    def ub = problem.upperBounds
    val nonBasis = (0 until n).filterNot(basis.toSet)
    lazy val A_B = A(::, basis).toDenseMatrix
    lazy val A_Binv = inv(A_B)
    lazy val A_N = A(::, nonBasis).toDenseMatrix
    lazy val x_N = x(nonBasis).toDenseVector
    lazy val x_B = x(basis).toDenseVector

    def atLowerBound(i: Int): Boolean = problem.lowerBounds(i) == x(i)
    def atUpperBound(i: Int): Boolean = problem.upperBounds(i) == x(i)

    lazy val isDualFeasible = {
      nonBasis.forall(j =>
        problem.isFixed(j)
       || (
          (!problem.isFree(j) || reducedCosts(j) == 0)
          && (!atLowerBound(j) || reducedCosts(j) >= 0)
          && (!atUpperBound(j) || reducedCosts(j) <= 0)
          )
      )

    }

    lazy val isBasisSolutionOptimal = {
      basis.forall(i => lb(i) <= x(i) && x(i) <= ub(i))
    }

    override def toString = {
      s"""
         |State {
         |  x: $x
         |  y: $y
         |  basis: $basis
         |  Z: $objective
         |  d: $reducedCosts
         |
         |  problem: {
         |    min c dot x s.t. A * x = b, lb <= x <= ub
         |    where
         |      c: $c
         |      A: ${A.toString.replaceAll("\n", "\n         ")}
         |      b: $b
         |      lb: $lb
         |      ub: $ub
         |  }
         |}
         |
         |
       """.stripMargin
    }

    override def clone = copy(x = x.copy, reducedCosts = reducedCosts.copy)
  }

  object State {

    // algorithm 3, step 1
    def init(cf: ComputationalForm, initX: DenseVector[Double], basis: Basis) = {
      val zero = zeros(cf, basis).copy(x = initX.copy)
      import zero.{basis => _, _}

      val initX_B = A_B \ (b - A_N * x_N)
      zero.x(basis) := initX_B

      val initY = A_B.t \ c(basis).toDenseVector

      val initCosts = cf.c - cf.A.t * initY
      initCosts(basis) := 0.0

      zero.copy(y = initY, objective = c dot x, reducedCosts = initCosts)
    }

    def zeros(problem: ComputationalForm, basis: Basis) = {
      assert(basis.length == problem.m)
      State(problem, DenseVector.zeros(problem.n), DenseVector.zeros(problem.m), basis, 0.0, DenseVector.zeros(problem.n))
    }
  }

  // ensures a dual feasible basis
  // algorithm 5, 6.
  // NB: doesn't do the d_p = -\theta_D thing.
  private def updateBasisBFRT(state: State,
                              theta_D: Double,
                              alpha: DenseVector[Double]):State = {
    var newState = state.clone
    scaleAdd.inPlace(newState.reducedCosts(state.nonBasis), theta_D, alpha(state.nonBasis))

    var variablesToFlip = for {
      j <- state.nonBasis
      if state.problem.isBoxed(j) && !state.problem.isFixed(j)
      if ((newState.atLowerBound(j) && newState.reducedCosts(j) < 0)
        || (newState.atUpperBound(j) && newState.reducedCosts(j) > 0))
    } yield j

    if (variablesToFlip.nonEmpty) {
      val (deltaZ, ã) = variablesToFlip.foldLeft((0.0, DenseVector.zeros[Double](alpha.size))) { (acc, j) =>
        val scale =
          if (newState.atLowerBound(j) && state.reducedCosts(j) < 0) {
            state.ub(j) - state.lb(j)
          } else if (newState.atUpperBound(j) && state.reducedCosts(j) > 0) {
            state.lb(j) - state.ub(j)
          } else {
            0.0
          }

        val newÃ = scaleAdd.inPlace(acc._2, scale, state.A(::, j))
        (acc._1 + scale * state.c(j), newÃ)
      }

      val deltaX_b = state.A_B \ ã
      newState.x(state.basis) -= deltaX_b

      val deltaZ_new = deltaZ + (deltaX_b dot state.c(state.basis))

      newState = newState.copy(objective = newState.objective + deltaZ_new)
    }

    for (j <- variablesToFlip) {
      if (newState.x(j) == newState.problem.lowerBounds(j)) {
        newState.x(j) = state.problem.upperBounds(j)
      } else {
        newState.x(j) = state.problem.lowerBounds(j)
      }
    }

    newState
  }

  // problem 4.7
  private def feasibleBasisFindingProblem(problem: ComputationalForm):ComputationalForm = {
    val newC = problem.c.copy
    newC(problem.boxedVariables) := 0.0
    val lowerBounds = DenseVector.tabulate(problem.n)(j => -I(problem.hasUpperBound(j) || problem.isFree(j)))
    val upperBounds = DenseVector.tabulate(problem.n)(j => I(problem.hasLowerBound(j) || problem.isFree(j)))
    lowerBounds(problem.boxedVariables) := 0.0
    upperBounds(problem.boxedVariables) := 0.0
    val newA = problem.A.copy
    newA(::, problem.boxedVariables) := 0.0
    ComputationalForm(newC, newA, DenseVector.zeros[Double](newA.rows), lowerBounds * 1E4, upperBounds * 1E4)
  }



  def main(args: Array[String]): Unit = {
    for(i <- 0 until 1000) {
      val in = System.currentTimeMillis()

      val g = new FlowGraph[Int] {

        def source = 0

        def sink = 5

        case class E(head: Int, tail: Int, override val capacity: Double, override val cost: Double) extends Edge

        val edges = Map(0 -> Seq(E(0, 1, 3, 3), E(0, 2, 3, 1)),
          1 -> Seq(E(1, 3, 2, 1), E(1, 4, 2, 1)),
          2 -> Seq(E(2, 3, 1, 4), E(2, 4, 2, 2)),
          3 -> Seq(E(3, 5, 2, 2)),
          4 -> Seq(E(4, 5, 2, 1)))

        def edgesFrom(n: Int) = edges(n).iterator
      }

      val lpm = new LPMaxFlow(g)
      assert((lpm.maxFlow._2 - 4).abs < 1E-5, lpm.toString)
      assert((lpm.minCostFlow()._2 - 20).abs < 1E-5, lpm.toString)
      println((System.currentTimeMillis() - in))
    }
  }
}