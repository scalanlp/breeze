package breeze.numerics

import breeze.benchmark.{MyRunner, BreezeBenchmark}
import breeze.linalg.{softmax, DenseVector}
import breeze.stats.distributions.Rand
import breeze.macros._

/**
 * Created by dlwh on 10/3/15.
 */
class SoftmaxBenchmark extends BreezeBenchmark {
  val dv = DenseVector.rand(5000, Rand.uniform.map(_.toFloat))

  def timeSoftmaxFloat(reps: Int) = {
    var sum = 0.0
    cforRange(0 until reps) { _ =>
      sum += softmax(dv)
    }

    sum
  }

}

object SoftmaxBenchmark extends MyRunner(classOf[SoftmaxBenchmark])
