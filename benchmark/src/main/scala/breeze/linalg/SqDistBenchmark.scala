package breeze.linalg

import breeze.benchmark.{MyRunner, BreezeBenchmark}
import spire.syntax.cfor._

/**
 * Created by dlwh on 8/14/15.
 */
class SqDistBenchmark extends BreezeBenchmark {
  assert(usingNatives)

  val x1, x2 = DenseMatrix.rand(20, 200)
  val v1, v2 = DenseVector.rand(100)

  /*
  def timeCopying(reps: Int) = {
    cforRange(0 until reps) { i =>
      val t1 = -2.0 * (x1.t * x2)

      val t2 = t1(*, ::) + sum(x2 :* x2, Axis._0).t

      t2(::, *) + sum(x1 :* x1, Axis._0).t
    }
  }

  def timeVectorized(reps: Int) = {
    cforRange(0 until reps) { i =>
      val D = x1.rows
      val M = x1.cols
      val N = x2.cols
      val x1OnesT = DenseMatrix.ones[Double](M, D)
      val x2Ones = DenseMatrix.ones[Double](D, N)
      // (M x D) * D x N  -= M X N += (D x M).t * (D X N)
      (x1 :* x1).t * x2Ones -= (x1.t * x2 *= 2.0) += x1OnesT * (x2 :* x2)
    }
  }

  def timeMrkaspasImpl(reps: Int) = {
    val dataSet = DenseMatrix.rand[Double](1024, 1934)
    val input = DenseVector.rand(1024)
    val labels  = DenseVector.rand(dataSet.cols)
    cforRange(0 until reps) { i =>

      // calculates the distance => √(x2-x1)^ + (y2-y1)^
      val diffMat = dataSet(::, *) - input
      val sqDiffMat = diffMat :^ 2.0
      val sqDistances = sum(sqDiffMat(::, *)) // sum per column
      val distances = sqDistances :^ 0.5 // sqrt
      val k = 5
      val sortedDistances = argtopk(distances.t *= -1.0, k) // sort the arguments
      val mapAcc = (0 until k).map { i => labels(sortedDistances(i)) }.groupBy(identity).mapValues(_.size)
      val res = mapAcc.foldLeft((0.0, 0)) { (t, curr) => if (t._2 > curr._2) t else curr }
      res._1.toInt
    }
  }

  def timeBroadcastSubtract(reps: Int) = {
    val dataSet = DenseMatrix.rand[Double](1024, 1934)
    val input = DenseVector.rand(1024)
    cforRange(0 until reps) { i =>
      // calculates the distance => √(x2-x1)^ + (y2-y1)^
      dataSet(::, *) - input
    }
  }

  def timeBroadcastRowSubtract(reps: Int) = {
    val dataSet = DenseMatrix.rand[Double](1934, 1024)
    val input = DenseVector.rand(1024)
    cforRange(0 until reps) { i =>
      // calculates the distance => √(x2-x1)^ + (y2-y1)^
      dataSet(*, ::) - input
    }
  }

  def timeInPlacish(reps: Int) = {
    cforRange(0 until reps) { i =>
      val t1 = (x1.t * x2) *= -2.0

      t1(*, ::) += sum(x2 :* x2, Axis._0).t

      t1(::, *) += sum(x1 :* x1, Axis._0).t
    }
  }

  def timeFirstPart(reps: Int) = {
    cforRange(0 until reps) { i =>
      val t1 = (x1.t * x2) *= -2.0
    }
  }
  def timeFirstAndSecondPart(reps: Int) = {
    cforRange(0 until reps) { i =>
      val t1 = (x1.t * x2) *= -2.0

      t1(*, ::) += sum(x2 :* x2, Axis._0).t
    }
  }

  def timeFirstAndThirdPart(reps: Int) = {
    cforRange(0 until reps) { i =>
      val t1 = (x1.t * x2) *= -2.0

            t1(::, *) += sum(x1 :* x1, Axis._0).t
    }
  }

  def timeSumNoAdd(reps: Int) = {
    cforRange(0 until reps) { i =>

      sum(x2 :* x2, Axis._0).t

    }
  }

  def timeBroadcastColumnAdd(reps: Int) = {
    cforRange(0 until reps) { i =>
      val t1 = (x1.t * x2) *= -2.0

      t1(::, *) += x2(0, ::).t

    }
  }

  def timeLoopColumnAdd(reps: Int) = {
    cforRange(0 until reps) { i =>
      val t1 = (x1.t * x2) *= -2.0

      val row = x2(0, ::).t
      for(i <- 0 until t1.cols) {
        t1(::, i) += row
      }
    }
  }


  def timeLoopColumnAddWithCopy(reps: Int) = {
    cforRange(0 until reps) { i =>
      val t1 = (x1.t * x2) *= -2.0

      val row = x2(0, ::).t.copy
      for(i <- 0 until t1.cols) {
        t1(::, i) += row
      }
    }
  }
   */

  def timeVectorSquaredDistance(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { i =>
      sum += squaredDistance(v1, v2)
    }

    sum
  }

  def timeOldSquaredDistance(reps: Int) = {
    var squaredDistance = 0.0
    cforRange(0 until reps) { _ =>
      zipValues(v1, v2).foreach { (a, b) =>
        val score = a - b
        squaredDistance += (score * score)
      }
      squaredDistance
    }
  }

}

object SqDistBenchmark extends MyRunner(classOf[SqDistBenchmark])

object SqDistX extends App {
  println((new SqDistBenchmark).timeVectorSquaredDistance(10000000))
  //  (new SqDistBenchmark).timeVectorizedCopyX1(10000)
}
