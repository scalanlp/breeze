package breeze.optimize.linear

import collection.mutable.ArrayBuffer
import breeze.linalg._
import org.apache.commons.math3.optim.linear._
import org.apache.commons.math3.optim.nonlinear.scalar._
import scala.collection.JavaConverters._

/**
 * DSL for LinearPrograms. Not thread-safe per instance. Make multiple instances
 *
 * Basic example:
 * {{{
 * val lp = new LP
 * import lp._
 * val x = new Positive("x")
 * val y = new Positive("y")
 *
 * val result = maximize ( (3 * x+ 4 * y)
 * subjectTo( x <= 3, y <= 1))
 *
 * result.valueOf(x) // 3
 *
 * }}}
 * @author dlwh
 */
class LinearProgram {
  self =>
  private var _nextId = 0
  private def nextId = {
    _nextId += 1
    _nextId - 1
  }
  private val variables = new ArrayBuffer[Variable]()

  def minimize(expression: Expression): Problem = {
    new Problem {
      override def goal: Option[GoalType] = Option(GoalType.MINIMIZE)
      override def objective: Expression = expression.objective
      override def constraints: IndexedSeq[Constraint] = expression.constraints
    }
  }

  def maximize(expression: Expression): Problem = {
    new Problem {
      override def goal: Option[GoalType] = Option(GoalType.MAXIMIZE)
      override def objective: Expression = expression.objective
      override def constraints: IndexedSeq[Constraint] = expression.constraints
    }
  }

  sealed trait Problem {
    outer =>
    def goal: Option[GoalType] = None
    def objective: Expression
    def constraints: IndexedSeq[Constraint]

    def subjectTo(constraints: Constraint*): Problem = {
      val cons = constraints
      new Problem {
        override def goal: Option[GoalType] = outer.goal
        override def objective: Expression = outer.objective
        override def constraints: IndexedSeq[Constraint] = outer.constraints ++ cons
      }
    }

    def solve(implicit solver: LinearProgram.Solver) = {
      val _goal = goal.getOrElse(throw new IllegalArgumentException("Goal is not defined."))

      if (_goal == GoalType.MAXIMIZE) {
        solver.maximize(self)(this)
      } else if (_goal == GoalType.MINIMIZE) {
        solver.minimize(self)(this)
      } else {
        throw new IllegalArgumentException(s"Unknown goal ${_goal.name()}")
      }
    }

    override def toString: String = {
      val _goal = goal match {
        case Some(g) => g.name().toLowerCase()
        case _ => "problem "
      }

      s"${_goal}    " + objective + {
        if (constraints.nonEmpty) {
          "\nsubject to  " + constraints.mkString("\n" + " " * "subject to  ".length)
        } else ""
      }
    }

  }

  /**
   * Anything that can be built up from adding/subtracting/dividing and multiplying by constants
   */
  sealed trait Expression extends Problem { outer =>
    def coefficients: Vector[Double]
    def scalarComponent: Double = 0
    def objective = this

    def constraints: IndexedSeq[Constraint] = IndexedSeq.empty

    def +(other: Expression): Expression = new Expression {
      def coefficients: Vector[Double] = outer.coefficients + other.coefficients
      override def scalarComponent: Double = outer.scalarComponent + other.scalarComponent
      override def toString = outer.toString + " + " + other
    }

    def +(other: Double): Expression = new Expression {
      def coefficients: Vector[Double] = outer.coefficients
      override def scalarComponent: Double = outer.scalarComponent + other
      override def toString = outer.toString + " + " + other
    }

    def -(other: Expression): Expression = new Expression {
      def coefficients: Vector[Double] = outer.coefficients - other.coefficients
      override def scalarComponent: Double = outer.scalarComponent - other.scalarComponent
      override def toString = outer.toString + " - " + other
    }

    def -(other: Double): Expression = new Expression {
      def coefficients: Vector[Double] = outer.coefficients
      override def scalarComponent: Double = outer.scalarComponent - other
      override def toString = outer.toString + " - " + other
    }

    def unary_- : Expression = new Expression {
      def coefficients: Vector[Double] = outer.coefficients * -1.0
      override def scalarComponent: Double = -outer.scalarComponent
      override def toString = s"-($outer)"
    }

    def <=(rhs_ : Expression): Constraint = new Constraint {
      def relation: LinearProgram.this.type#Relation = LTE

      def lhs = outer
      def rhs = rhs_
    }

    def <=(c: Double): Constraint = new Constraint {

      def relation: Relation = LTE

      def lhs = outer
      def rhs = new Expression {
        def coefficients = SparseVector.zeros[Double](variables.length)
        override def scalarComponent = c

        override def toString = c.toString
      }
    }

    def >=(rhs_ : Expression): Constraint = new Constraint {
      def relation: Relation = GTE

      def lhs = outer
      def rhs = rhs_
    }

    def >=(c: Double): Constraint = new Constraint {

      def relation: Relation = GTE

      def lhs = outer
      def rhs = new Expression {
        def coefficients = SparseVector.zeros[Double](variables.length)
        override def scalarComponent = c

        override def toString = c.toString
      }
    }

    def =:=(rhs_ : Expression): Constraint = new Constraint {
      def relation: Relation = EQ

      def lhs = outer
      def rhs = rhs_
    }

    def =:=(c: Double): Constraint = new Constraint {

      def relation: Relation = EQ

      def lhs = outer
      def rhs = new Expression {
        def coefficients = SparseVector.zeros[Double](variables.length)
        override def scalarComponent = c

        override def toString = c.toString
      }
    }

    def *(c: Double): Expression = new Expression {
      def coefficients = outer.coefficients * c
      override def scalarComponent = outer.scalarComponent * c
      override def toString = s"($outer) * $c"
    }

    def *:(c: Double): Expression = new Expression {
      def coefficients = outer.coefficients * c
      override def scalarComponent = outer.scalarComponent * c
      override def toString = s"$c * ($outer)"
    }
  }

  sealed abstract class Relation(val operator: String)
  case object LTE extends Relation("<=")
  case object GTE extends Relation(">=")
  case object EQ extends Relation("=:=")

  sealed trait Constraint { outer =>
    def lhs: Expression
    def rhs: Expression
    def relation: Relation

    override def toString() = s"$lhs ${relation.operator} $rhs"

    def standardize: Constraint = new Constraint {

      def relation: Relation = outer.relation

      def lhs = new Expression {
        def coefficients = outer.lhs.coefficients - outer.rhs.coefficients
        override def scalarComponent = 0.0
      }
      def rhs = new Expression {
        def coefficients = SparseVector.zeros[Double](variables.length)
        override def scalarComponent = outer.rhs.scalarComponent - outer.lhs.scalarComponent
      }
    }
  }

  sealed trait Variable extends Expression {
    def name: String
    def id: Int
    def size: Int = 1

    override def toString = name
  }

  case class Real(name: String = "x_" + nextId) extends Variable { variable =>
    val id = variables.length
    variables += this

    def coefficients = {
      val v = SparseVector.zeros[Double](variables.length)
      for (i <- 0 until size) v(id + i) = 1.0
      v
    }
  }

  case class Integer(name: String = "x_" + nextId) extends Variable { variable =>
    val id = variables.length
    variables += this

    def coefficients = {
      val v = SparseVector.zeros[Double](variables.length)
      for (i <- 0 until size) v(id + i) = 1.0
      v
    }
  }

  case class Binary(name: String = "x_" + nextId) extends Variable { variable =>
    val id = variables.length
    variables += this

    def coefficients = {
      val v = SparseVector.zeros[Double](variables.length)
      for (i <- 0 until size) v(id + i) = 1.0
      v
    }
  }

  /* I thought that interior point defaulted to requiring all variables to be positive. I appear to be wrong.
  case class Real(name: String="x_" + nextId) extends Variable {
    val id = variables.length
    variables += this
    variables += this

     def coefficients = {
      val v = SparseVector.zeros[Double](variables.length)
      v(id) = 1
      v(id+1) = -1
      v
    }
  }
   */

  case class Result(result: DenseVector[Double], problem: Problem) {
    def valueOf(x: Expression): Double = { (result.dot(x.coefficients)) + x.scalarComponent }
    def value = valueOf(problem.objective)
  }

  def maximize(objective: Problem)(implicit solver: LinearProgram.Solver) = {
    assume(!objective.goal.contains(GoalType.MINIMIZE), "Cannot call maximize on a minimization problem")
    solver.maximize(this)(objective)
  }

  def minimize(objective: Problem)(implicit solver: LinearProgram.Solver) = {
    assume(!objective.goal.contains(GoalType.MAXIMIZE), "Cannot call minimize on a maximization problem")
    solver.minimize(this)(objective)
  }
}

object LinearProgram {
  trait Solver {
    def maximize(lp: LinearProgram)(obj: lp.Problem): lp.Result
    def minimize(lp: LinearProgram)(obj: lp.Problem): lp.Result
  }

  implicit val mySolver = {
//    NativeLPSolver
//  } catch {
//    case ex: SecurityException =>
    ApacheSimplexSolver
//    case ex: UnsatisfiedLinkError =>
//      ApacheSimplexSolver
  }

  object ApacheSimplexSolver extends Solver {
    def maximize(lp: LinearProgram)(objective: lp.Problem): lp.Result = {
      import lp._

      val obj = new LinearObjectiveFunction(
        objective.objective.coefficients.toDenseVector.data,
        objective.objective.scalarComponent)

      val constraintSet = buildConstraints(lp)(objective)

      val sol = new SimplexSolver()
        .optimize(obj, constraintSet, GoalType.MAXIMIZE)
      Result(new DenseVector(sol.getPoint), objective)
    }

    def minimize(lp: LinearProgram)(objective: lp.Problem): lp.Result = {
      import lp._

      val obj = new LinearObjectiveFunction(
        objective.objective.coefficients.toDenseVector.data,
        objective.objective.scalarComponent)

      val constraintSet = buildConstraints(lp)(objective)

      val sol = new SimplexSolver()
        .optimize(obj, constraintSet, GoalType.MINIMIZE)
      Result(new DenseVector(sol.getPoint), objective)
    }

    private def buildConstraints(lp: LinearProgram)(objective: lp.Problem): LinearConstraintSet = {
      import lp._

      def relationToConstraintType(r: Relation) = r match {
        case LTE => Relationship.LEQ
        case GTE => Relationship.GEQ
        case EQ => Relationship.EQ
      }

      for (v <- variables)
        if (!v.isInstanceOf[lp.Variable])
          throw new UnsupportedOperationException("Apache Solver can only handle real-valued linear programs.")

      val constraints = for (c: Constraint <- objective.constraints) yield {
        val cs = c.standardize
        new LinearConstraint(
          cs.lhs.coefficients.toDenseVector.data,
          relationToConstraintType(c.relation),
          cs.rhs.scalarComponent)
      }
      new LinearConstraintSet(constraints.asJava)
    }
  }

  /*
  object NativeLPSolver extends Solver {
    LpSolve.lpSolveVersion()
    def maximize(lp: LinearProgram)(objective: lp.Problem): lp.Result = {
      val lpsol = LpSolve.makeLp(0, lp.variables.length)
      try {
        import lp._

        def relationToConstraintType(r: Relation) = r match {
          case LTE => LpSolve.LE
          case GTE => LpSolve.GE
          case EQ => LpSolve.EQ
        }

        lpsol.setVerbose(LpSolve.IMPORTANT)

        for( (v, i) <- variables.zipWithIndex) {
          v match  {
            case x: Real =>
            case x: Integer => lpsol.setInt(i+1, true)
            case x: Binary => lpsol.setBinary(i+1, true)
          }

        }

        for( c <- objective.constraints) yield {
          val cs = c.standardize
          lpsol.addConstraint(0.0 +: cs.lhs.coefficients.toDenseVector.data, relationToConstraintType(cs.relation), cs.rhs.scalarComponent)
        }
        lpsol.setObjFn(objective.objective.scalarComponent +: objective.objective.coefficients.toDenseVector.data)
        lpsol.setMaxim()


        val status = lpsol.solve()
        val result = status match {
          case 0 =>
            val result = lp.Result(new DenseVector(lpsol.getPtrVariables), objective)
            result
          case LpSolve.UNBOUNDED =>
            throw new UnboundedSolutionException
          case LpSolve.INFEASIBLE =>
            throw new InfeasibleProblem(objective)
          case _ =>
            throw new RuntimeException("Optimization failed with status: "  + lpStatusToString(status) +"(" + status +")")
        }
        result
      } finally {
        lpsol.deleteLp()
      }

    }

    def lpStatusToString(status: Int) = status match {
      case -5 => "UnknownError"
      case -4 => "DataIgnored"
      case -3 => "NoBfp"
      case -2 => "NoMemory"
      case -1 => "NotRun"
      case 0 => "Optimal"
      case 1 => "Suboptimal"
      case 2 => "Infeasible"
      case 3 => "Unbounded"
      case 4 => "Degenerate"
      case 5 => "NumFailure"
      case 6 => "UserAbort"
      case 7 => "TimeOut"
      case 8 => "Running"
      case 9 => "FutureStatus"
      case _ => "Unknown"
    }
  }
 */
}

case class InfeasibleProblem(prob: LinearProgram#Problem) extends RuntimeException
