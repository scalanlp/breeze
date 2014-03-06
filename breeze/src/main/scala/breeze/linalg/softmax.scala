package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor


/**
 * Computes the softmax (a.k.a. logSum) of an object. Softmax is defined as \log \sum_i \exp(x(i)), but
 * implemented in a more numerically stable way. Softmax is so-called because it is
 * a differentiable function that tends to look quite a lot like max. Consider
 * log(exp(30) + exp(10)). That's basically 30. We use softmax a lot in machine learning.
 */
object softmax extends UFunc {

  implicit object implDoubleDouble extends Impl2[Double, Double, Double] {
    def apply(a: Double, b: Double): Double = {
      if (a.isNegInfinity) b
      else if (b.isNegInfinity) a
      else if (a < b) b + scala.math.log1p(scala.math.exp(a - b))
      else a + scala.math.log1p(scala.math.exp(b - a))
    }
  }

  /**
   * Method for computing the max of the first length elements of an array. Arrays
   * of size 0 give Double.NegativeInfinity
   * @param arr
   * @param length
   * @return
   */
  def array(arr: Array[Double], length: Int) = {
    val m = max.array(arr, length)
    var accum = 0.0
    var i = 0
    while(i < length) {
      accum += scala.math.exp(arr(i) - m)
      i += 1
    }
    m + scala.math.log(accum)
  }

  implicit def reduceDouble[T](implicit iter: CanTraverseValues[T, Double], maxImpl: max.Impl[T, Double]): Impl[T, Double] = new Impl[T, Double] {
    def apply(v: T): Double = {

      val max = if(!iter.isTraversableAgain(v)) 0.0 else maxImpl(v)

      val visit = new ValuesVisitor[Double] {
        var accum = 0.0
        def visit(a: Double): Unit = {
          accum += scala.math.exp(a - max)
        }

        def zeros(numZero: Int, zeroValue: Double): Unit = {
          if(numZero != 0) {
            accum += (numZero * scala.math.exp(zeroValue - max))
          }
        }

        override def visitArray(arr: Array[Double], offset: Int, length: Int, stride: Int): Unit = {
          var i = 0
          var off = offset
          while(i < length) {
            accum += scala.math.exp(arr(off) - max)
            i += 1
            off += stride
          }

        }
      }

      iter.traverse(v, visit)

      max + scala.math.log(visit.accum)
    }

  }
}

object logDiff extends UFunc {
  implicit object implDoubleDouble extends Impl2[Double, Double, Double] {
    def apply(a: Double, b: Double): Double = {
      require(a >= b, s"a should be greater than b, but got $a and $b")
      if (a > b) a + scala.math.log1p(- scala.math.exp(b-a))
      else Double.NegativeInfinity
    }
  }
}
