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
  private var _nextId = 0
  private def nextId = {
    _nextId += 1
    _nextId - 1
  }
  private val variables = new ArrayBuffer[Variable]()

  sealed trait Problem { outer =>
    def objective: Expression
    def constraints: IndexedSeq[Constraint]

    def subjectTo(constraints : Constraint*):Problem = {
      val cons = constraints
      new Problem {
        def objective = outer.objective
        def constraints = outer.constraints ++ cons
      }
    }

    override def toString = (
      "maximize    " + objective + {
        if(constraints.nonEmpty) {
          "\nsubject to  " + constraints.mkString("\n" + " " * "subject to  ".length)
        } else ""
      }
    )
  }

  /**
   * Anything that can be built up from adding/subtracting/dividing and multiplying by constants
   */
  sealed trait Expression extends Problem { outer =>
    def coefficients: VectorBuilder[Double]
    def scalarComponent: Double = 0
    def objective = this

    def constraints: IndexedSeq[Constraint] = IndexedSeq.empty

    def +(other: Expression): Expression = new Expression {
      def coefficients: VectorBuilder[Double] = outer.coefficients + other.coefficients
      override def scalarComponent: Double = outer.scalarComponent + other.scalarComponent
      override def toString = outer.toString + " + " + other
    }

    def +(other: Double): Expression = new Expression {
      def coefficients: VectorBuilder[Double] = outer.coefficients
      override def scalarComponent: Double = outer.scalarComponent + other
      override def toString = outer.toString + " + " + other
    }

    def -(other: Expression): Expression = new Expression {
      def coefficients: VectorBuilder[Double] = outer.coefficients - other.coefficients
      override def scalarComponent: Double = outer.scalarComponent - other.scalarComponent
      override def toString = outer.toString + " - " + other
    }

    def -(other: Double): Expression = new Expression {
      def coefficients: VectorBuilder[Double] = outer.coefficients
      override def scalarComponent: Double = outer.scalarComponent - other
      override def toString = outer.toString + " - " + other
    }

    def unary_- : Expression = new Expression {
      def coefficients: VectorBuilder[Double] = outer.coefficients * -1.0
      override def scalarComponent: Double = -outer.scalarComponent
      override def toString = s"-($outer)"
    }

    def <=(rhs : Expression): Constraint = new Constraint(outer, LTE, rhs)
    def <=(c: Double): Constraint = new Constraint(outer, LTE, c)

    def >=(rhs : Expression): Constraint = new Constraint(outer, GTE, rhs)
    def >=(c: Double): Constraint = new Constraint(this, GTE, c)

    def =:=(rhs : Expression): Constraint = new Constraint(outer, EQ, rhs)
    def =:=(c: Double): Constraint = new Constraint(this, EQ, c)

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

  case class Constraint(lhs: Expression, relation: Relation, rhs: Expression) {
    /**
      * Converts this to an equivalent expression of the form
      *    a dot x REL b, for vector a and scalar b
      * @return
      */
    def standardized: Constraint = {
      val outLHS = lhs.coefficients - rhs.coefficients
      val outRHS = rhs.scalarComponent - lhs.scalarComponent
      Constraint(new VectorExpression(outLHS), relation, rhs)
    }

    override def toString: String = s"$lhs ${relation.operator} $rhs"
  }

  sealed trait Variable extends Expression {
    def name: String
    def id : Int
    def size: Int = 1

    override def toString = name
    def lowerBound: Double
    def upperBound: Double

    def coefficients = {
      val v = VectorBuilder.zeros[Double](variables.length)
      for(i <- 0 until size) v.add(id + i, 1.0)
      v
    }
  }

  case class Real(name: String = "x_" + nextId,
                  lowerBound: Double = Double.NegativeInfinity,
                  upperBound: Double = Double.PositiveInfinity) extends Variable { variable =>
    val id = variables.length
    variables += this
  }

  case class Integer(name: String = "x_" + nextId,
                     lowerBound: Double = Double.NegativeInfinity,
                     upperBound: Double = Double.PositiveInfinity) extends Variable { variable =>
    val id = variables.length
    variables += this

  }

  case class Binary(name: String = "x_" + nextId) extends Variable { variable =>

    override def lowerBound: Double = 0

    override def upperBound: Double = 1

    val id = variables.length
    variables += this

  }

  case class Result(result: DenseVector[Double], problem: Problem) {
    def valueOf(x: Expression):Double =  {(result dot x.coefficients) + x.scalarComponent}
    def value = valueOf(problem.objective)
  }

  def maximize(objective: Problem)(implicit solver: LinearProgram.Solver) =
    solver.maximize(this)(objective)
  def minimize(objective: Problem)(implicit solver: LinearProgram.Solver) =
    solver.minimize(this)(objective)


  // implementation stuff
  private[linear] class VectorExpression(coeffs: VectorBuilder[Double]) extends Expression {
    // sshhhhhh
    coeffs.length = -1
    override def coefficients: VectorBuilder[Double] = {
      VectorBuilder.zeros[Double](variables.length) += coeffs
    }
  }

  private[linear] implicit class ScalarExpression(override val scalarComponent: Double) extends Expression {
    override def coefficients: VectorBuilder[Double] = {
      VectorBuilder.zeros(variables.length)
    }
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

    private def buildConstraints(lp: LinearProgram)(objective: lp.Problem)
        : LinearConstraintSet = {
      import lp._

      def relationToConstraintType(r: Relation) = r match {
        case LTE => Relationship.LEQ
        case GTE => Relationship.GEQ
        case EQ => Relationship.EQ
      }

      for (v <- variables)
        if (!v.isInstanceOf[lp.Variable])
          throw new UnsupportedOperationException(
            "Apache Solver can only handle real-valued linear programs.")

      val constraints = for (c: Constraint <- objective.constraints) yield {
        val cs = c.standardize
        new LinearConstraint(cs.lhs.coefficients.toDenseVector.data,
          relationToConstraintType(c.relation), cs.rhs.scalarComponent)
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
