package breeze.linalg

import breeze.benchmark.{MyRunner, BreezeBenchmark}
import spire.syntax.cfor._

/**
 * Created by dlwh on 8/14/15.
 */
class DenseScaleBenchmark extends BreezeBenchmark {
  assert(usingNatives)

  val dv, dv2 = DenseVector.rand(5)

  def timeSmallDVScale(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      dv *= 1.0001
    }
    dv2
  }

  def timeSmallDVInlineRange(reps: Int) = {
    cforRange(0 until reps) { rep =>
      val ad = dv.data
      cforRange(0 until dv.length) { i =>
        ad(i) *= 1.0001
      }
    }
    dv2
  }
  def timeSmallDVScaleInline(reps: Int) = {
    cforRange(0 until reps) { rep =>
      dv(0) *= 1.0001
      dv(1) *= 1.0001
      dv(2) *= 1.0001
      dv(3) *= 1.0001
      dv(4) *= 1.0001
    }
    dv
  }

}



object DenseScaleBenchmark extends MyRunner(classOf[DenseScaleBenchmark])
