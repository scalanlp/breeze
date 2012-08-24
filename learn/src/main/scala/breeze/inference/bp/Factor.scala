package breeze.inference.bp

import breeze.numerics._
import breeze.util.Index

/**
 * A Factor knows about a set of variables and can
 * score any assignment to those sets of variables.
 * By default, we assume all assignments are valid/scoreable.
 *
 * You can make some assignments invalid by overriding
 * foreachAssignment. This is useful for sparse structures
 *
 * @author dlwh
 */
trait Factor extends breeze.inference.Factor[Factor] {
  def variables: IndexedSeq[Variable[_]]
  def size = variables.map(_.size).product
  def apply(assignments: Array[Int]):Double = math.exp(logApply(assignments))
  def logApply(assignments: Array[Int]):Double

  /**Pointwise multiplication */
  def *(f: Factor) = new ProductFactor(this, f)

  /**Pointwise division */
  def /(f: Factor) = new ProductFactor(this, f, -1)

  /**May be infinite */
  lazy val logPartition = {
    val scores = new Array[Double](size)
    var off = 0
    foreachAssignment { ass =>
      scores(off) = logApply(ass)
      off += 1
    }

    logSum(scores, off)
  }

  def isConvergedTo(f: Factor, diff: Double):Boolean = {
    require(f.size == this.size)
    foreachAssignment{ass =>
      if(!closeTo(logApply(ass),f.logApply(ass), diff))
        return false
    }
    true
  }

  def foreachAssignment(f: Array[Int]=>Any) {
    val assignment = new Array[Int](variables.length)
    def rec(i: Int) {
      if(i == variables.length) {
        f(assignment)
      } else for(z <- 0 until variables(i).size) {
        assignment(i) = z
        rec(i+1)
      }
    }
    rec(0)
  }
}

object Factor {
  def fromLogFn[T1](v1: Variable[T1])(f: Int=>Double):Factor = new Factor {
    def variables = IndexedSeq(v1)

    def logApply(assignments: Array[Int]) = f(assignments(0))
  }

  def fromLogFn[T1, T2](v1: Variable[T1], v2: Variable[T2])(f: (Int,Int)=>Double):Factor = new Factor {
    def variables = IndexedSeq(v1, v2)

    def logApply(assignments: Array[Int]) = f(assignments(0), assignments(1))
  }
}

case class ProductFactor(f1: Factor, f2: Factor, scale2: Double = 1) extends Factor {
  val (variables, f2Map: Array[Int], isSameDomain) = {
    if(f1.variables.eq(f2.variables) || f1.variables.equals(f2.variables)) {
      (f1.variables, Array.range(0, f1.variables.size), true)
    } else {
      val varIndex = Index(f1.variables ++ f2.variables)
      (varIndex.iterator.toIndexedSeq, f2.variables.map(varIndex).toArray, false)
    }
  }

  def logApply(assignments: Array[Int]) = {
    if(isSameDomain) f1.logApply(assignments) + f2.logApply(assignments) * scale2
    else {
      val a1 = assignments take f1.variables.size
      val a2 = f2Map map assignments
      f1.logApply(a1) + f2.logApply(a2) * scale2
    }
  }
}
