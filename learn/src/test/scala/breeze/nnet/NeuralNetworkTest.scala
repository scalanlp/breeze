package breeze.nnet

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import breeze.linalg.DenseVector
import breeze.optimize.{GradientCheckingDiffFunction, LBFGS}
import breeze.util.logging.ConsoleLogging
import scala.util.Random

/**
 *
 * @author dlwh
 */
class NeuralNetworkTest extends FunSuite with Checkers {

  test("Can recover a simple quadratic function") {
    def f(x: Double) = x * x
    val inputs = DenseVector.rand(400, rand = new Random(1))
    val asVector = Array.tabulate(inputs.length)(i => DenseVector(inputs(i)))
    val outputs = inputs map f
    def loss(x: DenseVector[Double], y: Double) = ((x(0)-y)*(x(0)-y), DenseVector(2 * (x(0)-y)))

    val nnObj = new NNObjective[Double](asVector zip outputs.data, loss, Array(1, 20, 5, 1))

    val lbfgs = new LBFGS[DenseVector[Double]](maxIter = 250)
    val weights = lbfgs.minimize(nnObj, nnObj.initialWeightVector)
    val nn = nnObj.extract(weights)
    // ensure squared-loss is small.
    val mse = (asVector.map(nn) zip outputs.data map (loss _).tupled).map(_._1).sum / inputs.size
    assert(mse < 1E-4, "loss was " + mse + " which is too high!")
  }

}
