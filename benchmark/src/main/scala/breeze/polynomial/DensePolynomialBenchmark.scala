package breeze.polynomial

import breeze.benchmark._
import breeze.linalg.BuildsRandomVectors
import breeze.stats.distributions._

import spire.math._
import spire.math.poly._
import spire.implicits._

object DensePolynomialBenchmark extends MyRunner(classOf[DensePolynomialBenchmark])

class DensePolynomialBenchmark extends BreezeBenchmark with BuildsRandomVectors {

  def randomPoly(order: Int) = {
    val uniform = Uniform(0,1)
    val array = new Array[Double](order)
    var i=0
    while (i < order) {
      array(i) = uniform.draw()
      i += 1
    }
    Polynomial.dense(array)
  }

  def timePolyOnDenseVector(reps: Int) = runWith2(reps, { randomPoly(10) }, {randomArray(1024*4) })( (poly, arr) => {
    poly(arr)
  })
}
