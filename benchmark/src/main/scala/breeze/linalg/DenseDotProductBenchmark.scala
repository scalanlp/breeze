package breeze.linalg

import breeze.benchmark.{MyRunner, BreezeBenchmark}
import breeze.linalg.operators.DenseVectorSupportMethods
import breeze.numerics._
import spire.syntax.cfor._

/**
 * Created by dlwh on 8/14/15.
 */
class DenseDotProductBenchmark extends BreezeBenchmark {

  val dv, dv2 = DenseVector.rand(5)
  val dvBig, dv2Big = DenseVector.rand(300)


  def timeDirectBigDV(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      sum += DenseVectorSupportMethods.dotProduct_Double(dvBig.data, 0, dv2Big.data, 0, dvBig.length)
    }
    sum
  }

  def timeBigDVDot(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      sum += dvBig dot dv2Big
    }
    sum
  }

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



  val UNROLL_FACTOR = 8

  def timeBigDVUnrolled(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      val ad = dvBig.data
      val bd = dv2Big.data

      val rem = (dvBig.length % UNROLL_FACTOR)
      val loops = (dvBig.length - rem)/UNROLL_FACTOR

      cforRange(0 until rem) { i =>
        sum += ad(i) * bd(i)
      }

      var offset = rem
      cforRange(0 until loops) { _ =>
        sum += (
          ad(offset + 0) * bd(offset + 0)
          + ad(offset + 1) * bd(offset + 1)
            + ad(offset + 2) * bd(offset + 2)
            + ad(offset + 3) * bd(offset + 3)
            + ad(offset + 4) * bd(offset + 4)
            + ad(offset + 5) * bd(offset + 5)
            + ad(offset + 6) * bd(offset + 6)
            + ad(offset + 7) * bd(offset + 7)
          )
        offset += UNROLL_FACTOR
      }
    }
    sum
  }

  def timeBigDVInlineRange(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      val ad = dvBig.data
      val bd = dv2Big.data
      cforRange(0 until dvBig.length) { i =>
        sum += ad(i) * bd(i)
      }
    }
    sum
  }
}

object DenseDotProductBenchmark extends MyRunner(classOf[DenseDotProductBenchmark])
