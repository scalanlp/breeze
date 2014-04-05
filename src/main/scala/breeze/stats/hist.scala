package breeze.stats

import breeze.generic.UFunc
import breeze.macros.expand
import breeze.linalg._
import breeze.linalg.support.{CanTransformValues, CanMapValues, CanTraverseValues}
import breeze.linalg.support.CanTraverseValues.ValuesVisitor

//ToDo: minMax function to find both in one go

object hist extends UFunc {
  @expand
  implicit def defaultHist[T, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseValues[T, S]): Impl[T, DenseVector[S]] = new Impl[T,DenseVector[S]] {
    private val innerImpl = implicitly[Impl2[T, Int,DenseVector[S]]]
    def apply(v: T) = innerImpl.apply(v, 10)
  }

  @expand
  implicit def defaultHistBins[T, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseValues[T, S]): Impl2[T, Int, DenseVector[S]] = new Impl2[T,Int,DenseVector[S]] {
    private val innerImpl = implicitly[Impl3[T, Int, (Double,Double), DenseVector[S]]]
    def apply(v: T, bins: Int) = {
      val minima = min(v).toDouble
      val maxima = max(v).toDouble
      innerImpl.apply(v, bins, (minima, maxima))
    }
  }

  @expand
  implicit def canTraverseValuesImpl[T, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseValues[T, S]): Impl3[T, Int, (Double,Double), DenseVector[S]] = new Impl3[T, Int, (Double,Double), DenseVector[S]] {

    def apply(v: T, bins: Int, range: (Double,Double)): DenseVector[S] = {
      val (minima, maxima) = range
      if (maxima <= minima) {
        throw new IllegalArgumentException("Minima of a histogram must not be greater than the maxima")
      }
      val result = DenseVector.zeros[S](bins)

      val visitor = new ValuesVisitor[S] {
        def visit(a: S) = {
          val ad = a.toDouble
          val i: Int = math.floor(bins * ((ad-minima)/maxima)).toInt
          if ((i >= 0) && (i < bins)) {
            result.unsafeUpdate(i, result.unsafeValueAt(i) + 1)
          }
          if (ad == maxima) { //Include the endpoint
            result.unsafeUpdate(bins-1, result.unsafeValueAt(bins-1) + 1)
          }
        }
        def zeros(numZero: Int, zeroValue: S): Unit = {
          val i = math.floor(bins * ((zeroValue.toDouble-minima)/maxima)).toInt
          if ((i >= 0) && (i < bins)) {
            result.unsafeUpdate(i, result.unsafeValueAt(i) + numZero)
          }
        }
      }
      iter.traverse(v, visitor)

      result
    }
  }
}
