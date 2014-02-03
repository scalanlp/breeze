package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor

/**
 * A [[breeze.generic.UFunc]] for computing the mean of objects
 */
object mean extends UFunc {
  implicit def reduceDouble[T](implicit iter: CanTraverseValues[T, Double]): Impl[T, Double] = new Impl[T, Double] {
    def apply(v: T): Double = {
      val visit = new ValuesVisitor[Double] {
        var sum = 0.0
        var n = 0
        def visit(a: Double): Unit = {
          sum += a
          n += 1
        }

        def zeros(numZero: Int, zeroValue: Double): Unit = {
          sum += numZero * zeroValue
          n += numZero
        }
      }

      iter.traverse(v, visit)

      visit.sum / visit.n
    }
  }

}
/**
 * A [[breeze.generic.UFunc]] for computing the mean and variance of objects.
 * This uses an efficient, numerically stable, one pass algorithm for computing both
 * the mean and the variance.
 */
object meanAndVariance extends UFunc {
  implicit def reduceDouble[T](implicit iter: CanTraverseValues[T, Double]): Impl[T, (Double, Double)] = new Impl[T, (Double, Double)] {
    def apply(v: T): (Double, Double) = {
      val visit = new ValuesVisitor[Double] {
        var mu = 0.0
        var s = 0.0
        var n = 0
        def visit(y: Double): Unit = {
          n += 1
          val d = y - mu
          mu = mu + 1.0/n * d
          s = s + (n-1) * d / n * d
        }

        def zeros(numZero: Int, zeroValue: Double): Unit = {
          for(i <- 0 until numZero) visit(zeroValue)
        }
      }
      iter.traverse(v, visit)
      import visit._
      (mu, s/(n-1))
    }
  }
}


/**
 * A [[breeze.generic.UFunc]] for computing the variance of objects.
 * The method just calls meanAndVariance and returns the second result.
 */
object variance extends UFunc {
  implicit def reduceDouble[T](implicit mv: meanAndVariance.Impl[T, (Double, Double)]): Impl[T, Double] = new Impl[T, Double] {
    def apply(v: T): Double = mv(v)._2
  }
}

/**
 * Computes the standard deviation by calling variance and then sqrt'ing
 */
object stddev extends UFunc {
  implicit def reduceDouble[T](implicit mv: variance.Impl[T, Double]): Impl[T, Double] = new Impl[T, Double] {
    def apply(v: T): Double = scala.math.sqrt(mv(v))
  }
}
