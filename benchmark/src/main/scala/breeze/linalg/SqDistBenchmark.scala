package breeze.linalg

import breeze.benchmark.{MyRunner, BreezeBenchmark}
import spire.syntax.cfor._

/**
 * Created by dlwh on 8/14/15.
 */
class SqDistBenchmark extends BreezeBenchmark {
  assert(usingNatives)

  val x1, x2 = DenseMatrix.rand(20, 200)

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

  def timeVectorizedCopyX1(reps: Int) = {
    cforRange(0 until reps) { i =>
      val D = x1.rows
      val M = x1.cols
      val N = x2.cols
      val x1OnesT = DenseMatrix.ones[Double](M, D)
      val x2Ones = DenseMatrix.ones[Double](D, N)
      val x1T = x1.t.copy
      val dotProds = (x1T * x2 *= 2.0)
      x1T :*= x1T
      // (M x D) * D x N  -= M X N += (M X D) * (D X N)
      x1T * x2Ones -=
        dotProds +=
        (x1OnesT * (x2 :* x2))
    }
  }

  /*
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

}


object SqDistBenchmark extends MyRunner(classOf[SqDistBenchmark])

object SqDistX extends App {
  (new SqDistBenchmark).timeVectorized(10000)
//  (new SqDistBenchmark).timeVectorizedCopyX1(10000)
}
