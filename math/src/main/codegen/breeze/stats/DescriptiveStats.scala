package breeze.stats

import breeze.linalg.operators.{OpAdd, OpDiv, OpLT, OpLTE, OpSet}
import breeze.linalg.scaleAdd
import breeze.linalg.support.{CanCreateZerosLike, ScalarOf}
import breeze.stats.meanAndVariance.MeanAndVariance
import breeze.stats.stddev.Impl
import breeze.stats.stddev.population.Impl
import breeze.stats.variance.Impl

import scala.reflect.ClassTag
import util.Sorting
import breeze.util.{quickSelect, quickSelectImpl}

import scala.collection.compat._

/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

import breeze.generic.UFunc
import breeze.linalg.{DenseMatrix, DenseVector, convert, cov}
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.macros.expand
import breeze.math.Complex
import breeze.numerics.isOdd
import breeze.macros.{cforRange}
import scala.collection.mutable

object accumulateAndCount extends UFunc {
  @expand
  implicit def reduce[T, @expand.args(Double, Complex, Float) Scalar](
      implicit iter: CanTraverseValues[T, Scalar],
      @expand.sequence[Scalar](0.0, Complex.zero, 0.0f) _zero: Scalar): Impl[T, (Scalar, Int)] =
    new Impl[T, (Scalar, Int)] {
      def apply(v: T): (Scalar, Int) = {
        object visit extends ValuesVisitor[Scalar] {
          var sum = _zero
          var n = 0
          def visit(a: Scalar): Unit = {
            sum += a
            n += 1
          }

          def zeros(numZero: Int, zeroValue: Scalar): Unit = {
            this.sum = this.sum + zeroValue * numZero
            n += numZero
          }
        }
        iter.traverse(v, visit)

        (visit.sum, visit.n)
      }
    }
}

sealed trait meanLowPriority { self: mean.type =>
  implicit def canMeanGeneric[T, S](implicit
                                    iter: CanTraverseValues[T, S],
                                    zerosLike: CanCreateZerosLike[S, S],
                                    setInto: OpSet.InPlaceImpl2[S, S],
                                    axpy: scaleAdd.InPlaceImpl3[S, Double, S],
                                    canMulIntoVS: OpDiv.InPlaceImpl2[S, Double],
                                   ): Impl[T, S] =
    new Impl[T, S] {
      def apply(v: T): S = {
        object visit extends ValuesVisitor[S] {
          var runningMean: S = _
          var scratch: S = _
          var count = 0L

          def visit(y: S): Unit = {
            if (count == 0) {
              init(y)
              count += 1
            } else {
              count += 1
              // runningMean = runningMean + (y - runningMean) / count
              setInto(scratch, y)
              scaleAdd.inPlace(scratch, -1.0, runningMean)
              scaleAdd.inPlace(runningMean, 1.0 / count, scratch)
            }
          }

          private def init(y: S) = {
            runningMean = zerosLike(y)
            setInto(runningMean, y)
            scratch = zerosLike(y)
          }

          def zeros(numZero: Int, zeroValue: S): Unit = {
            if (count == 0) {
              init(zeroValue)
            } else {
              if (numZero != 0)
                canMulIntoVS(runningMean, count / (count + numZero))
            }
            count += numZero
          }
        }

        iter.traverse(v, visit)
        if (visit.count == 0) throw new IllegalArgumentException("empty collection")
        else visit.runningMean
      }
    }
}


/**
 * A [[breeze.generic.UFunc]] for computing the mean of objects
 */
object mean extends UFunc with meanLowPriority {
  @expand
  implicit def reduce[@expand.args(Float, Double, Complex) S, T](
      implicit iter: CanTraverseValues[T, S],
      @expand.sequence[S](0f, 0d, Complex.zero) z: S): Impl[T, S] = new Impl[T, S] {
    def apply(v: T): S = {
      object visit extends ValuesVisitor[S] {
        var mu: S = z
        var n: Long = 0

        def visit(y: S): Unit = {
          n += 1
          val d = y - mu
          mu = mu + d / n
        }

        def zeros(numZero: Int, zeroValue: S): Unit = {
          if (numZero != 0)
            mu = mu * n / (n + numZero)
          n += numZero
        }
      }
      iter.traverse(v, visit)
      import visit._
      mu
    }
  }

}

/**
 * A [[breeze.generic.UFunc]] for computing the mean and sample variance of objects.
 * This uses an efficient, numerically stable, one pass algorithm for computing both
 * the mean and the variance.
 */
object meanAndVariance extends UFunc {
  @expand
  implicit def reduce[@expand.args(Float, Double) S, T](
      implicit iter: CanTraverseValues[T, S]): Impl[T, MeanAndVariance] = new Impl[T, MeanAndVariance] {
    def apply(v: T): MeanAndVariance = {
      object visit extends ValuesVisitor[S] {
        var mu: S = 0
        var s: S = 0
        var n: Long = 0

        def visit(y: S): Unit = {
          n += 1
          val d = y - mu
          mu = mu + d / n
          s = s + (n - 1) * d / n * d
        }

        def zeros(numZero: Int, zeroValue: S): Unit = {
          for (i <- 0 until numZero) visit(zeroValue)
        }
      }
      iter.traverse(v, visit)
      import visit._
      if (n > 1) {
        MeanAndVariance(mu, s / (n - 1), n)
      } else {
        MeanAndVariance(mu, 0, n)
      }
    }
  }

  case class MeanAndVariance(mean: Double, variance: Double, count: Long) {
    def stdDev: Double = math.sqrt(variance)
    def populationVariance: Double = if (count == 0) 0 else (count - 1).toDouble / count * variance

    def +(other: MeanAndVariance): MeanAndVariance = {
      val d = other.mean - this.mean
      val newMean = this.mean + d * other.count / (other.count + this.count)

      val m2a = this.variance * (this.count - 1)
      val m2b = other.variance * (other.count - 1)

      val m2x = m2a + m2b + d * d * (other.count * this.count) / (other.count + this.count)

      val newVariance = m2x / (other.count + this.count - 1)

      MeanAndVariance(newMean, newVariance, this.count + other.count)

    }
  }
}

/**
 * A [[breeze.generic.UFunc]] for computing the *sample* variance of objects.
 * The method just calls meanAndVariance and returns the second result.
 *
 * Call variance.population if you want population variance
 */
object variance extends UFunc {
  implicit def reduceDouble[T](implicit mv: meanAndVariance.Impl[T, meanAndVariance.MeanAndVariance]): Impl[T, Double] =
    new Impl[T, Double] {
      def apply(v: T): Double = mv(v).variance
    }

  object population extends UFunc {
    implicit def reduceDouble[T](
        implicit mv: meanAndVariance.Impl[T, meanAndVariance.MeanAndVariance]): Impl[T, Double] = new Impl[T, Double] {
      def apply(v: T): Double = mv(v).populationVariance
    }
  }
}

/**
 * Computes the *sample* standard deviation by calling variance and then sqrt'ing. Note that this is different from Excel, numpy, etc.
 *
 * Call stddev.population if you want that.
 */
object stddev extends UFunc {
  implicit def reduceDouble[T](implicit vari: variance.Impl[T, Double]): Impl[T, Double] = new Impl[T, Double] {
    def apply(v: T): Double = scala.math.sqrt(vari(v))
  }

  object population extends UFunc {
    implicit def reduceDouble[T](implicit vari: variance.population.Impl[T, Double]): Impl[T, Double] =
      new Impl[T, Double] {
        def apply(v: T): Double = scala.math.sqrt(vari(v))
      }
  }
}

/**
 * A [[breeze.generic.UFunc]] for computing the median of objects
 */
object median extends UFunc {

  @expand
  implicit def reduceArray[@expand.args(Int, Long, Double, Float) T]: median.Impl[Array[T], T] =
    new Impl[Array[T], T] {
      def apply(v: Array[T]): T = {
        if (isOdd(v.length)) {
          quickSelect(v, (v.length - 1) / 2)
        } else {
          val tempArray: Array[T] = v.clone()
          val secondMedianPosition = v.length / 2
          //quickSelectImpl does not clone the array, allowing us to access intermediate semi-sorted results for reuse in the second calculation
          (quickSelectImpl(tempArray, secondMedianPosition) +
            quickSelectImpl(tempArray, secondMedianPosition - 1)) / 2
        }
      }
    }


  implicit def reduceArrayFromQuickselectAndMean[T](implicit qs: quickSelect.Impl2[Array[T], Int, T],
                                             qsi: quickSelectImpl.Impl2[Array[T], Int, T],
                                                    mn: mean.Impl2[T, T, T]): median.Impl[Array[T], T] =
    new Impl[Array[T], T] {
      def apply(v: Array[T]): T = {
        if (isOdd(v.length)) {
          quickSelect(v, (v.length - 1) / 2)
        } else {
          val tempArray: Array[T] = v.clone()
          val secondMedianPosition = v.length / 2
          //quickSelectImpl does not clone the array, allowing us to access intermediate semi-sorted results for reuse in the second calculation
          mean(quickSelectImpl(tempArray, secondMedianPosition), quickSelectImpl(tempArray, secondMedianPosition - 1))
        }
      }
    }

  implicit def reduce[T: ClassTag](implicit arrImpl: median.Impl[Array[T], T]): median.Impl[DenseVector[T], T] = { dv =>
    if (dv.data.length == dv.length) {
      // avoid extra clone
      arrImpl(dv.data)
    } else {
      arrImpl(dv.toArray)
    }
  }

//  implicit def reduce[T: ClassTag](implicit arrImpl: median.Impl[Array[T], T]): median.Impl[DenseVector[T], T] =
//    new Impl[DenseVector[T], T] {
//      def apply(v: DenseVector[T]): T = {
//        if (isOdd(v.length)) {
//          quickSelect(v.toArray, (v.length - 1) / 2)
//        } else {
//          val tempArray: Array[T] = v.toArray.clone()
//          val secondMedianPosition = v.length / 2
//          //quickSelectImpl does not clone the array, allowing us to access intermediate semi-sorted results for reuse in the second calculation
//          (quickSelectImpl(tempArray, secondMedianPosition) +
//            quickSelectImpl(tempArray, secondMedianPosition - 1)) / 2
//        }
//      }
//    }

  implicit def reduceSeq[T: ClassTag](implicit arrImpl: median.Impl[Array[T], T]): Impl[Seq[T], T] =
    v => { median(v.toArray) }

  implicit def reduceM[T](implicit arrImpl: median.Impl[Array[T], T]): Impl[DenseMatrix[T], T] = m => median(m.toArray)

}

object covmat extends UFunc {
  implicit val matrixCovariance: Impl[DenseMatrix[Double], DenseMatrix[Double]] =
    new Impl[DenseMatrix[Double], DenseMatrix[Double]] {
      def apply(data: DenseMatrix[Double]) = cov(data)
    }

  implicit val sequenceCovariance: Impl[Seq[DenseVector[Double]], DenseMatrix[Double]] =
    new Impl[Seq[DenseVector[Double]], DenseMatrix[Double]] {
      /*
       * We roughly follow the two_pass_covariance algorithm from here: http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Covariance
       * However, we also use Bessel's correction, in order to agree with the rest of breeze.
       */
      def apply(data: Seq[DenseVector[Double]]): DenseMatrix[Double] = {
        data.headOption
          .map(firstRow => {
            val result = new DenseMatrix[Double](firstRow.size, firstRow.size)
            val dataSize = firstRow.size
            //First compute the mean
            var mean = firstRow.copy
            var numRows: Long = 1
            data.tail.foreach(x => {
              numRows += 1
              if (mean.size != x.size) {
                throw new IllegalArgumentException(
                  "Attempting to compute covariance of dataset where elements have different sizes")
              }
              cforRange(0 until firstRow.size)(i => {
                mean(i) = mean(i) + x(i)
              })

            })
            val numRowsD = numRows.toDouble
            mean = mean / numRowsD

            //Second compute the covariance
            data.foreach { x =>
              cforRange(0 until dataSize)(i => {
                val a = x(i) - mean(i)
                cforRange(0 until dataSize)(j => {
                  val b = x(j) - mean(j)
                  result(i, j) = result(i, j) + (a * b / (numRowsD - 1)) //Use
                })
              })
            }

            result
          })
          .getOrElse(new DenseMatrix[Double](0, 0))
      }
    }
}

object corrcoeff extends UFunc {
  implicit def matrixCorrelation[T](
      implicit covarianceCalculator: covmat.Impl[T, DenseMatrix[Double]]): Impl[T, DenseMatrix[Double]] =
    new Impl[T, DenseMatrix[Double]] {
      def apply(data: T) = {
        val covariance = covarianceCalculator(data)
        val d = new Array[Double](covariance.rows)
        cforRange(0 until covariance.rows)(i => {
          d(i) = math.sqrt(covariance(i, i))
        })

        cforRange(0 until covariance.rows)(i => {
          cforRange(0 until covariance.rows)(j => {
            if (i != j) {
              covariance(i, j) /= (d(i) * d(j))
            } else {
              covariance(i, j) = 1.0
            }
          })
        })
        covariance
      }
    }
}

object mode extends UFunc {
  @expand
  implicit def reduce[T, @expand.args(Double, Complex, Float, Int) Scalar](
      implicit iter: CanTraverseValues[T, Scalar],
      @expand.sequence[Scalar](Double.NaN, Complex.nan, 0.0f, 0) initialValue: Scalar): Impl[T, ModeResult[Scalar]] =
    new Impl[T, ModeResult[Scalar]] {
      def apply(v: T): ModeResult[Scalar] = {
        val visitor = new ModeVisitor[Scalar](initialValue)
        iter.traverse(v, visitor)
        ModeResult(visitor.runningMode, visitor.maxFrequency)
      }
    }

}

private class ModeVisitor[Scalar](initialValue: Scalar)
    extends ValuesVisitor[Scalar] {

  val frequencyCounts = mutable.Map[Scalar, Int]()
  var maxFrequency = 0
  var runningMode = initialValue

  def visit(value: Scalar): Unit = recordOccurrences(value, 1)

  def zeros(numZeros: Int, zeroValue: Scalar): Unit = recordOccurrences(zeroValue, numZeros)

  private def recordOccurrences(value: Scalar, count: Int): Unit = {
    frequencyCounts(value) = frequencyCounts.getOrElse(value, 0) + count
    if (frequencyCounts(value) > maxFrequency) {
      maxFrequency = frequencyCounts(value)
      runningMode = value
    }
  }

}

/**
 * A [[breeze.generic.UFunc]] for digitizing arrays.
 *
 * Each element in the bins array is assumed to be the *right* endpoint of a given bin.
 * For instance, bins=[1,3,5] represents a bin from (-infty,1], (1,3], (3,5] and (5,\infty).
 * The result returned is the index of the bin of the inputs.
 *
 * E.g., digitize([-3, 0.5, 1, 1.5, 4], [0,1,2]) = [0, 1, 1, 2, 3]
 */
object digitize extends UFunc {

  implicit def arrayVersion[T, U](
      implicit base: Impl2[DenseVector[T], DenseVector[U], DenseVector[Int]]): Impl2[Array[T], Array[U], Array[Int]] = {
    new Impl2[Array[T], Array[U], Array[Int]] {
      def apply(x: Array[T], bins: Array[U]): Array[Int] = {
        val vecResult: DenseVector[Int] = digitize(new DenseVector(x), new DenseVector(bins))
        vecResult.data
      }
    }
  }

  implicit def fromComparison[T, U](
      implicit lte: OpLTE.Impl2[T, U, Boolean],
      ltUU: OpLT.Impl2[U, U, Boolean]): Impl2[DenseVector[T], DenseVector[U], DenseVector[Int]] = {
    new Impl2[DenseVector[T], DenseVector[U], DenseVector[Int]] {
      def apply(x: DenseVector[T], bins: DenseVector[U]): DenseVector[Int] = {
        errorCheckBins(bins)
        val result = new DenseVector[Int](x.length)
        cforRange(0 until x.length) { i =>
          result(i) = bins.length
          var j = bins.length - 1
          while (j >= 0) {
            if (lte(x(i), bins(j))) {
              result(i) = j
            } else {
              j = -1
            }
            j -= 1
          }
        }
        result

      }
    }
  }
  @expand
  implicit def vecVersion[@expand.args(Int, Long, Double, Float) T]
    : Impl2[DenseVector[T], DenseVector[Double], DenseVector[Int]] =
    new Impl2[DenseVector[T], DenseVector[Double], DenseVector[Int]] {
      def apply(x: DenseVector[T], bins: DenseVector[Double]): DenseVector[Int] = {
        errorCheckBins(bins)
        val result = new DenseVector[Int](x.length)
        cforRange(0 until x.length) { i =>
          result(i) = bins.length
          var j = bins.length - 1
          while (j >= 0) {
            if (x(i) <= bins(j)) {
              result(i) = j
            } else {
              j = -1
            }
            j -= 1
          }
        }
        result
      }
    }

  private def errorCheckBins[U](bins: DenseVector[U])(implicit lt: OpLT.Impl2[U, U, Boolean]) = {
    cforRange(0 until bins.length - 1) { i =>
      require(lt(bins(i), bins(i + 1)))
    }
  }
}

/**
 * A [[breeze.generic.UFunc]] for counting bins.
 *
 * If passed a traversable object full of Int's, provided
 * those ints are larger than 0, it will return an
 * array of the bin counts. E.g.:
 * bincount(DenseVector[Int](0,1,2,3,1,3,3,3)) == DenseVector[Int](1,2,1,4)
 *
 * One can also call this on two dense vectors - the second will be treated
 * as an array of weights. E.g.:
 * val x = DenseVector[Int](0,1,2,3,1)
 * val weights = DenseVector[Double](1.0,2.0,1.0,7.0,1.0)
 * result is bincount(x, weights) == DenseVector[Double](1.0,3.0,1,7.0)
 */
object bincount extends UFunc {
  import breeze.linalg._

  @expand
  implicit def vecVersion[@expand.args(Double, Complex, Float) T]
    : Impl2[DenseVector[Int], DenseVector[T], DenseVector[T]] =
    new Impl2[DenseVector[Int], DenseVector[T], DenseVector[T]] {
      def apply(x: DenseVector[Int], weights: DenseVector[T]): DenseVector[T] = {
        require(min(x) >= 0)
        require(x.length == weights.length)
        val result = new DenseVector[T](max(x) + 1)
        cforRange(0 until x.length)(i => {
          result(x(i)) = result(x(i)) + weights(i)
        })
        result
      }
    }

  implicit def reduce[T](implicit iter: CanTraverseValues[T, Int]): Impl[T, DenseVector[Int]] =
    new Impl[T, DenseVector[Int]] {
      def apply(x: T): DenseVector[Int] = {
        require(min(x) >= 0)
        val result = new DenseVector[Int](max(x) + 1)
        class BincountVisitor extends ValuesVisitor[Int] {
          def visit(a: Int): Unit = { result(a) = result(a) + 1 }
          def zeros(numZero: Int, zeroValue: Int) = {
            result(0) = result(0) + numZero
          }
        }
        iter.traverse(x, new BincountVisitor)
        result
      }
    }

  /**
   * A [[breeze.generic.UFunc]] for counting bins.
   *
   * This differs from bincount in that the result it returns is a SparseVector.
   * The internal implementation of this could probably be significantly sped
   * up by avoiding the use of counter. Then again, in sparse situations it's
   * still a lot faster than using bincount.
   *
   */
  object sparse extends UFunc {
    import breeze.linalg._

    @expand
    implicit def vecVersion[@expand.args(Double, Complex, Float) T]
      : Impl2[DenseVector[Int], DenseVector[T], SparseVector[T]] =
      new Impl2[DenseVector[Int], DenseVector[T], SparseVector[T]] {
        def apply(x: DenseVector[Int], weights: DenseVector[T]): SparseVector[T] = {
          require(min(x) >= 0)
          require(x.length == weights.length)
          val counter = Counter[Int, T]()
          cforRange(0 until x.length)(i => {
            counter.update(x(i), counter(x(i)) + weights(i))
          })
          val builder = new VectorBuilder[T](max(x) + 1)
          counter.iterator.foreach(x => builder.add(x._1, x._2))
          builder.toSparseVector
        }
      }

    implicit def reduce[T](implicit iter: CanTraverseValues[T, Int]): Impl[T, SparseVector[Int]] =
      new Impl[T, SparseVector[Int]] {
        def apply(x: T): SparseVector[Int] = {
          require(min(x) >= 0)
          val counter = Counter[Int, Int]()

          class BincountVisitor extends ValuesVisitor[Int] {
            def visit(a: Int): Unit = { counter.update(a, counter(a) + 1) }
            def zeros(numZero: Int, zeroValue: Int) = {
              counter.update(zeroValue, counter(zeroValue) + numZero)
            }
          }
          iter.traverse(x, new BincountVisitor)
          val builder = new VectorBuilder[Int](max(x) + 1)
          counter.iterator.foreach(x => builder.add(x._1, x._2))
          builder.toSparseVector
        }
      }
  }
}

/**
 * Provides utilities for descriptive statistics, like the mean and variance.
 */
object DescriptiveStats {

  /**
   * Returns the estimate of the data at p * it.size after copying and sorting, where p in [0,1].
   */
  def percentile(it: TraversableOnce[Double], p: Double): Double = {
    if (p > 1 || p < 0) throw new IllegalArgumentException("p must be in [0,1]")
    val arr = it.toArray
    Sorting.quickSort(arr)
    percentileInPlace(arr, p)
  }

  /**
    * Returns the estimate of a pre-sorted array at p * it.size, where p in [0,1].
    * <p>
    * Note:
    * Result is invalid if the input array is not already sorted.
    * </p>
    */
  def percentileInPlace(arr: Array[Double], p: Double): Double = {
    if (p > 1 || p < 0) throw new IllegalArgumentException("p must be in [0,1]")
    // +1 so that the .5 == mean for even number of elements.
    val f = (arr.length + 1) * p
    val i = f.toInt
    if (i == 0) arr.head
    else if (i >= arr.length) arr.last
    else {
      arr(i - 1) + (f - i) * (arr(i) - arr(i - 1))
    }
  }

  /**
   * Returns both means and covariance between two vectors. Single pass algorithm.
   * <p>
   * Note:
   * Will happily compute covariance between vectors of different lengths
   * by truncating the longer vector.
   * </p>
   */
  def meanAndCov[T](it1: TraversableOnce[T], it2: TraversableOnce[T])(implicit frac: Fractional[T]) = {
    import frac.mkNumericOps
    //mu1(n-1), mu2(n-1), Cov(n-1), n-1
    val (mu1, mu2, c, n) = (it1.iterator.to(Iterable), it2.iterator.to(Iterable)).zipped.foldLeft((frac.zero, frac.zero, frac.zero, frac.zero)) { (acc, y) =>
      val (oldMu1, oldMu2, oldC, oldN) = acc
      val newN = oldN + frac.fromInt(1)
      val newMu1 = oldMu1 + ((y._1 - oldMu1) / newN)
      val newMu2 = oldMu2 + ((y._2 - oldMu2) / newN)
      val newC = oldC + ((y._1 - oldMu1) * (y._2 - newMu2)) //compute covariance in single pass
      (newMu1, newMu2, newC, newN)
    }
    if (n == 1) (mu1, mu2, 0) else (mu1, mu2, c / (n - frac.fromInt(1)))
  }

  /**
   * Returns covariance between two vectors.
   * <p>
   * Note:
   * Will happily compute covariance between vectors of different lengths
   * by truncating the longer vector.
   * </p>
   */
  def cov[T](it1: Iterable[T], it2: Iterable[T])(implicit n: Fractional[T]) = {
    meanAndCov(it1, it2)._3
  }

}
