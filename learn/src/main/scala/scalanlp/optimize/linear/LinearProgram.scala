package scalanlp.optimize.linear

import scalala.tensor._;

import collection.mutable.ArrayBuffer
import dense.{DenseMatrix, DenseVector}
import sparse.SparseVector

/**
 * DSL for LinearPrograms. Not thread-safe per instance. Make multiple instances
 *
 * Basic example:
 * {{{
 * val lp = new LP;
 * import lp._;
 * val x = new Positive("x");
 * val y = new Positive("y");
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
  private var _nextId = 0;
  private def nextId = {
    _nextId += 1
    _nextId - 1
  }
  private val variables = new ArrayBuffer[Variable]();

  sealed trait Problem { outer =>
    def objective: Expression;
    def constraints: IndexedSeq[Constraint];

    def subjectTo(constraints : Constraint*):Problem = {
      val cons = constraints;
      new Problem {
        def objective = outer.objective;
        def constraints = outer.constraints ++ cons;
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
  sealed trait Expression extends Problem{ outer =>
    private[LinearProgram] def coefficients: Vector[Double];
    private[LinearProgram] def scalarComponent: Double = 0;
    def objective = this;

    def constraints: IndexedSeq[Constraint] = IndexedSeq.empty;

    def +(other: Expression):Expression = new Expression {
      private[LinearProgram] def coefficients: Vector[Double] = outer.coefficients + other.coefficients;
      private[LinearProgram] override def scalarComponent: Double = outer.scalarComponent + other.scalarComponent
      override def toString = outer.toString + " + " + other
    }

    def -(other: Expression):Expression = new Expression {
      private[LinearProgram] def coefficients: Vector[Double] = outer.coefficients - other.coefficients;
      private[LinearProgram] override def scalarComponent: Double = outer.scalarComponent - other.scalarComponent
      override def toString = outer.toString + " - " + other
    }

    def <=(rhs_ : Expression):Constraint = new Constraint {
      def lhs = outer;
      def rhs = rhs_;
    }

    def <=(c: Double):Constraint = new Constraint {
      def lhs = outer;
      def rhs = new Expression {
        private[LinearProgram] def coefficients = SparseVector.zeros[Double](variables.length);
        private[LinearProgram] override def scalarComponent = c;

        override def toString = c.toString
      }
    }

    def *(c: Double) = new Expression {
      private[LinearProgram] def coefficients = outer.coefficients * c;
      private[LinearProgram] override def scalarComponent = outer.scalarComponent * c;
      override def toString = outer.toString + " * " + c
    }

    def *:(c: Double) = new Expression {
      private[LinearProgram] def coefficients = outer.coefficients * c;
      private[LinearProgram] override def scalarComponent = outer.scalarComponent * c;
      override def toString = outer.toString + " * " + c
    }
  }

  sealed trait Constraint { outer =>
    def lhs: Expression
    def rhs: Expression

    override def toString() = lhs.toString + " <= " + rhs;

    def standardize: Constraint = new Constraint {
      def lhs = new Expression {
        private[LinearProgram] def coefficients = outer.lhs.coefficients - outer.rhs.coefficients;
        private[LinearProgram] override def scalarComponent = 0.0
      }
      def rhs = new Expression {
        private[LinearProgram] def coefficients = SparseVector.zeros[Double](variables.length);
        private[LinearProgram] override def scalarComponent = outer.rhs.scalarComponent - outer.lhs.scalarComponent;
      }
    }
  }

  sealed trait Variable extends Expression {
    def name: String
    def id : Int
    def size: Int = 1


    override def toString = name
  }

  case class Positive(name: String = "x_" + nextId) extends Variable { variable =>
    val id = variables.length
    variables += this;

    private[LinearProgram] def coefficients = {
      val v = SparseVector.zeros[Double](variables.length)
      for(i <- 0 until size) v(id + i) = 1.0
      v
    }
  }

  case class Real(name: String="x_" + nextId) extends Variable {
    val id = variables.length
    variables += this;
    variables += this;

    private[LinearProgram] def coefficients = {
      val v = SparseVector.zeros[Double](variables.length)
      v(id) = 1
      v(id+1) = -1
      v
    }
  }

  case class Result private[LinearProgram] (result: DenseVector[Double], problem: Problem) {
    def valueOf(x: Expression) = {(x.coefficients dot result) + x.scalarComponent};
    def value = valueOf(problem.objective);
  }



  def maximize(objective: Problem) = {
    val c = DenseVector.zeros[Double](variables.size) + objective.objective.coefficients;

    val A = DenseMatrix.zeros[Double](objective.constraints.length,variables.length);
    val b = DenseVector.zeros[Double](objective.constraints.length)

    for( (c,i) <- objective.constraints.zipWithIndex) {
      val cs = c.standardize
      A(i,::) := cs.lhs.coefficients;
      b(i) = cs.rhs.scalarComponent;
    }

    val result = InteriorPoint.minimize(A,b,-c, DenseVector.zeros[Double](variables.size) + 1.0);
    Result(result,objective)
  }

}


