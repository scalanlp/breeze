package breeze.linalg

import breeze.benchmark.{MyRunner, BreezeBenchmark}
import breeze.macros._

/**
 * Created by dlwh on 8/14/15.
 */
class DenseScaleBenchmark extends BreezeBenchmark {
  assert(usingNatives)

  val dv, dv2 = DenseVector.rand(10)
//  val dv, dv2 = DenseVector.rand(100000).apply(0 to -1 by 2)

  def timeSmallDVScale(reps: Int) = {
    cforRange(0 until reps) { rep =>
      dv *= 1.0001
    }
    dv
  }

  def timeSmallDVInlineRange(reps: Int) = {
    cforRange(0 until reps) { rep =>
      val ad = dv.data
      cforRange(0 until dv.length) { i =>
        ad(i) *= 1.0001
      }
    }
    dv
  }

  def timeSmallDVScaleInline(reps: Int) = {
    val d = dv.data
    cforRange(0 until reps) { rep =>
      d(0) *= 1.0001
      d(1) *= 1.0001
      d(2) *= 1.0001
      d(3) *= 1.0001
      d(4) *= 1.0001
      d(5) *= 1.0001
      d(6) *= 1.0001
      d(7) *= 1.0001
      d(8) *= 1.0001
      d(9) *= 1.0001
    }
    dv
  }

}

object DenseScaleBenchmark extends MyRunner(classOf[DenseScaleBenchmark])
