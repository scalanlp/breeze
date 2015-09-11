package breeze.linalg

import breeze.benchmark.{MyRunner, BreezeBenchmark}
import spire.syntax.cfor._

/**
 * Created by dlwh on 8/14/15.
 */
class SqDistBenchmark extends BreezeBenchmark {
  assert(usingNatives)

  val x1, x2 = DenseMatrix.rand(1, 4000)

  def timeCopying(reps: Int) = {
    cforRange(0 until reps) { i =>
      val t1 = -2.0 * (x1.t * x2)

      val t2 = t1(*, ::) + sum(x2 :* x2, Axis._0).t

      t2(::, *) + sum(x1 :* x1, Axis._0).t
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
  (new SqDistBenchmark).timeCopying(1000)
}
