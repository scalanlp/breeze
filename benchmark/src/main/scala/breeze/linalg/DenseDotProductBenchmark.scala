package breeze.linalg

import breeze.benchmark.{MyRunner, BreezeBenchmark}
import breeze.numerics._
import spire.syntax.cfor._

/**
 * Created by dlwh on 8/14/15.
 */
class DenseDotProductBenchmark extends BreezeBenchmark {

  val dv, dv2 = DenseVector.rand(200)

  def timeSmallDVDot(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      sum += dv dot dv2
    }
    sum
  }

  def timeSmallDVInlineRange(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      val ad = dv.data
      val bd = dv2.data
      cforRange(0 until dv.length) { i =>
        sum += ad(i) * bd(i)
      }
    }
    sum
  }


  /*
  def timeSmallDVDotInline(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) ( rep =>
      sum += dv(0) * dv2(0)
        + dv(1) * dv2(1)
        + dv(2) * dv2(2)
        + dv(3) * dv2(3)
        + dv(4) * dv2(4)
    )
    sum
  }
  */
}

object DenseDotProductBenchmark extends MyRunner(classOf[DenseDotProductBenchmark])
