package breeze.optimize

import breeze.math.{InnerProductSpace, MutableInnerProductSpace, MutableVectorSpace, VectorSpace}
import breeze.linalg.operators.{OpMulMatrix, BinaryOp}
import breeze.stats.distributions.Rand
import collection.immutable.BitSet

/**
 * Represents a function for which we can easily compute the Hessian.
 *
 * For conjugate gradient methods, you can play tricks with the hessian,
 * returning an object that only supports multiplication.
 *
 * @author dlwh
 */
trait SecondOrderFunction[T, H] extends DiffFunction[T] {
  /** Calculates both the value and the gradient at a point */
  def calculate(x: T): (Double, T) = {
    val t@(v, g, _ ) =  calculate2(x)
    (v, g)
  }

  /** Calculates the value, the gradient, and the Hessian at a point */
  def calculate2(x: T): (Double, T, H)
}

object SecondOrderFunction {
  def empirical[T](f: DiffFunction[T], eps: Double = 1E-5)(implicit vs: VectorSpace[T, Double]):SecondOrderFunction[T, EmpiricalHessian[T]] = {
    new SecondOrderFunction[T, EmpiricalHessian[T]] {
      /** Calculates the value, the gradient, and the Hessian at a point */
      def calculate2(x: T): (Double, T, EmpiricalHessian[T]) = {
        val (v, grad) = f.calculate(x)
        val h = new EmpiricalHessian(f, x, grad, eps)
        (v,grad,h)
      }
    }
  }

  def minibatchEmpirical[T](f: BatchDiffFunction[T], eps: Double = 1E-5, batchSize: Int = 30000)(implicit vs: InnerProductSpace[T, Double]):SecondOrderFunction[T, EmpiricalHessian[T]] = {
    new SecondOrderFunction[T, EmpiricalHessian[T]] {
      /** Calculates the value, the gradient, and the Hessian at a point */
      def calculate2(x: T): (Double, T, EmpiricalHessian[T]) = {
        val subset = Rand.subsetsOfSize(f.fullRange, batchSize).draw()
        val (v, grad) = f.calculate(x)
        val newf = new DiffFunction[T] {
          def calculate(x: T): (Double, T) = {
            f.calculate(x, subset)
          }
        }
        val h = new EmpiricalHessian(newf, x, newf.gradientAt(x), eps)
        (v,grad,h)
      }
    }
  }
}

/**
 * The empirical hessian evaluates the derivative for multiplcation.
 *
 * H * d = \lim_e -> 0 (f'(x + e * d) - f'(x))/e
 *
 *
 * @param df
 * @param x the point we compute the hessian for
 * @param grad the gradient at x
 * @param eps a small value
 * @tparam T
 */
class EmpiricalHessian[T](df: DiffFunction[T], x: T,
                          grad: T, eps: Double = 1E-5)(implicit vs: VectorSpace[T, Double]) {
  import vs._

  def *(t: T):T = {
    (df.gradientAt(x + t * eps) - grad)/eps
  }


}

object EmpiricalHessian {
  implicit def product[T]:BinaryOp[EmpiricalHessian[T], T, OpMulMatrix, T] = {
    new BinaryOp[EmpiricalHessian[T], T, OpMulMatrix, T] {
      def apply(a: EmpiricalHessian[T], b: T):T = {
        a * b
      }
    }
  }

}

class FisherDiffFunction[T](df: BatchDiffFunction[T],
                            gradientsToKeep: Int = 1000)
                           (implicit vs: MutableInnerProductSpace[T, Double]) extends SecondOrderFunction[T, FisherMatrix[T]] {
  import vs._
  /** Calculates the value, the gradient, and an approximation to the Fisher approximation to the Hessian */
  def calculate2(x: T): (Double, T, FisherMatrix[T]) = {
    val subset = Rand.subsetsOfSize(df.fullRange, gradientsToKeep).draw()
    val toKeep = subset.map(i => df.calculate(x, IndexedSeq(i))).seq
    val (v, otherGradient) = df.calculate(x)

//    val fullGrad = toKeep.view.map(_._2).foldLeft(otherGradient * (df.fullRange.size - subset.size).toDouble )(_ += _ ) /df.fullRange.size.toDouble
//    val fullV = toKeep.view.map(_._1).foldLeft(v * (df.fullRange.size - subset.size) )(_ + _) / df.fullRange.size

    (v, otherGradient, new FisherMatrix(toKeep.map(_._2).toIndexedSeq))
  }
}

/**
 * The Fisher matrix approximates the Hessian by E[grad grad']. We further
 * approximate this with a monte carlo approximation to the expectation.
 *
 * @param grads
 * @param vs
 * @tparam T
 */
class FisherMatrix[T](grads: IndexedSeq[T])(implicit vs: MutableInnerProductSpace[T, Double]) {
  import vs._
  def *(t: T):T = {
    grads.view.map(g => g * (g dot t)).reduceLeft(_ += _) /= grads.length.toDouble
  }
}

object FisherMatrix {
  implicit def product[T]:BinaryOp[FisherMatrix[T], T, OpMulMatrix, T] = {
    new BinaryOp[FisherMatrix[T], T, OpMulMatrix, T] {
      def apply(a: FisherMatrix[T], b: T):T = {
        a * b
      }
    }
  }

}