package breeze.linalg

import breeze.benchmark.{MyRunner, BreezeBenchmark}
import breeze.linalg.operators.DenseVectorSupportMethods
import spire.syntax.cfor._

/**
 * Created by dlwh on 8/14/15.
 */
class DenseAxpyBenchmark extends BreezeBenchmark {
  assert(usingNatives)

  val dv, dv2 = DenseVector.rand(5)

  def timeSmallDVAxpy(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      axpy(0.042, dv, dv2)
    }
    dv2
  }

  def timeSmallDVInlineRange(reps: Int) = {
    cforRange(0 until reps) { rep =>
      val ad = dv.data
      val bd = dv2.data
      cforRange(0 until dv.length) { i =>
        bd(i) += 0.042 * ad(i)
      }
    }
    dv2
  }
  def timeSmallDVScaleAddInline(reps: Int) = {
    cforRange(0 until reps) { rep =>
      dv(0) += dv2(0) * 0.042
      dv(1) += dv2(1) * 0.042
      dv(2) += dv2(2) * 0.042
      dv(3) += dv2(3) * 0.042
      dv(4) += dv2(4) * 0.042
    }
    dv
  }

}


object DenseAxpyBenchmark extends MyRunner(classOf[DenseAxpyBenchmark])
