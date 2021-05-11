package breeze.linalg

import breeze.benchmark.{MyRunner, BreezeBenchmark}
import breeze.linalg.operators.DenseVectorSupportMethods
import breeze.stats.distributions.Rand
import com.github.fommil.netlib.BLAS
import breeze.macros._

/**
 * Created by dlwh on 8/14/15.
 */
class DenseAxpyBenchmark extends BreezeBenchmark {
  assert(usingNatives)

  val dv, dv2 = DenseVector.rand(5)
  val fv, fv2 = DenseVector.rand(5000, Rand.uniform.map(_.toFloat))

  def timeSaxpy(reps: Int) = {
    cforRange(0 until reps) { _ =>
      scaleAdd.inPlace(fv2, 0.042f, fv)
    }
  }

  def timeBlasSaxpy(reps: Int) = {
    cforRange(0 until reps) { _ =>
      BLAS.getInstance.saxpy(fv.length, 0.042f, fv.data, fv.stride, fv2.data, fv2.stride)
    }
  }

  def timeFVAddInlineRange(reps: Int) = {
    cforRange(0 until reps) { rep =>
      val ad = fv.data
      val bd = fv2.data
      cforRange(0 until fv.length) { i =>
        bd(i) += 0.042f * ad(i)
      }
    }
    dv2
  }

  /*
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

  val largeDV, largeDV2 = DenseVector.rand(400)
  val largeDM, largeDM2 = DenseMatrix.rand(20, 20)

  def timeLargeDMAddInPlace(reps: Int) = {
    cforRange(0 until reps) { rep =>
      largeDM += largeDM2
    }
  }

  def timeLargeDVAddInPlace(reps: Int) = {
    cforRange(0 until reps) { rep =>
      largeDV += largeDV2
    }
  }
 */

}

object DenseAxpyBenchmark extends MyRunner(classOf[DenseAxpyBenchmark])
object DenseAxpyX {
  def main(args: Array[String]): Unit = {
//    (new DenseAxpyBenchmark).timeSmallDVScaleAddInline(44400000)
  }
}
