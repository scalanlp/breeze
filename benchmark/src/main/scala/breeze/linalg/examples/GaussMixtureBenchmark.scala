package breeze.linalg
package examples

import breeze.numerics._
import breeze.stats.distributions._
import breeze.benchmark.{MyRunner, BreezeBenchmark}
import breeze.linalg.DenseVector

/**
 * Created by dlwh on 8/20/15.
 *
 * Based on code from Ivan Nikolaev
 */
class GaussMixtureBenchmark extends BreezeBenchmark {

  val x = DenseVector(5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0)
  val c = DenseVector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
  val gamma = 5.0
  private val n: Int = 1000

  def timeGMMVectors(reps: Int) = {
    val denseVectors = IndexedSeq.fill(n)(x)
    (0 until reps).foreach { i =>
      GaussMixtureTransform.samplesTransform(denseVectors, c, gamma)
    }
  }

  def timeGMMMat(reps: Int) = {
    val matrix = DenseMatrix.fill(n, 10)(5.0)
    (0 until reps).foreach { i =>
      GaussMixtureTransform.samplesTransform(matrix, c, gamma)
    }
  }

  def timeGMMMatColMajor(reps: Int) = {
    val matrix = DenseMatrix.fill(10, n)(5.0)
    (0 until reps).foreach { i =>
      GaussMixtureTransform.samplesTransformColMajor(matrix, c, gamma)
    }
  }

  def timeCenterMat(reps: Int) = {
    val matrix = DenseMatrix.fill(n, 10)(5.0)
    (0 until reps).foreach { i =>
      matrix(*, ::) - c
    }
  }

  def timeCenterMatColMajor(reps: Int) = {
    val matrix = DenseMatrix.fill(10, n)(5.0)
    (0 until reps).foreach { i =>
      matrix(::, *) - c
    }
  }

  def timeCenterVector(reps: Int) = {
    val denseVectors = IndexedSeq.fill(n)(x)
    (0 until reps).foreach { i =>
      denseVectors.foreach(_ - c)
    }
  }

}

object GaussMixtureTransform {
  def sampleTransform(sample: DenseVector[Double], centers: DenseVector[Double], gamma: Double): Double = {
    val diff: DenseVector[Double] = sample - centers
    exp(-gamma * (diff.dot(diff)))
  }

  def samplesTransform(samples: Iterable[DenseVector[Double]], centers: DenseVector[Double], gamma: Double): Double = {
    samples.map((sample: DenseVector[Double]) => sampleTransform(sample, centers, gamma)).sum
  }

  def samplesTransform(samples: DenseMatrix[Double], centers: DenseVector[Double], gamma: Double): Double = {
    val diff: DenseMatrix[Double] = samples(*, ::) - centers
    val prod = diff :*= diff
    val sum1: DenseVector[Double] = sum(prod, Axis._1) *= (-gamma)
    val exped = exp(sum1)
    val sum2 = sum(exped)
    sum2
//    sum(exp(sum(diff :*= diff, Axis._1) *= (-gamma)))
  }

  def samplesTransformColMajor(samples: DenseMatrix[Double], centers: DenseVector[Double], gamma: Double): Double = {
    val diff: DenseMatrix[Double] = samples(::, *) - centers
    val prod = diff :*= diff
    val sum1 = sum(prod, Axis._0) *= (-gamma)
    val exped = exp(sum1)
    val sum2 = sum(exped)
    sum2
    //    sum(exp(sum(diff :*= diff, Axis._1) *= (-gamma)))
  }
}

object GaussMixtureBenchmark extends MyRunner(classOf[GaussMixtureBenchmark])
