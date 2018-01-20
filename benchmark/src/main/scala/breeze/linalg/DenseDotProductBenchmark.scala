package breeze.linalg

import breeze.benchmark.{MyRunner, BreezeBenchmark}
import breeze.linalg.operators.DenseVectorSupportMethods
import breeze.numerics._
import breeze.stats.distributions.Rand
import spire.syntax.cfor._

/**
 * Created by dlwh on 8/14/15.
 */
class DenseDotProductBenchmark extends BreezeBenchmark {

  val dv, dv2 = DenseVector.rand(5)
  val dvBig, dv2Big = DenseVector.rand(300)
  val fv, fv2 = DenseVector.rand(5, Rand.uniform.map(_.toFloat))
  val fvBig, fv2Big = DenseVector.rand(3000, Rand.uniform.map(_.toFloat))

  def timeBigDVDotMasked(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      sum += (dvBig: Vector[Double]).dot(dv2Big: Vector[Double])
    }
    sum
  }

  def timeDirectBigDV(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      sum += DenseVectorSupportMethods.dotProduct_Double(dvBig.data, 0, dv2Big.data, 0, dvBig.length)
    }
    sum
  }

  def timeBigDVZip(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      zipValues(dvBig, dv2Big).foreach { (x, y) =>
        sum += x * y
      }
    }
    sum
  }

  def timeBigDVDot(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      sum += dvBig.dot(dv2Big)
    }
    sum
  }

  def timeSmallDVDot(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      sum += dv.dot(dv2)
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

  def timeDirectBigFV(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      sum += DenseVectorSupportMethods.dotProduct_Float(fvBig.data, 0, fv2Big.data, 0, fvBig.length)
    }
    sum
  }

  def timeBigFVDot(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { rep =>
      sum += fvBig.dot(fv2Big)
    }
    sum
  }

}

object DenseDotProductBenchmark extends MyRunner(classOf[DenseDotProductBenchmark])

object DenseDotProductX extends App {
  println((new DenseDotProductBenchmark).timeBigDVDotMasked(100000000))
  //  (new DenseDotProductBenchmark).timeVectorizedCopyX1(10000)
}
