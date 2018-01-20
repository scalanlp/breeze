package breeze.signal.support

import org.scalatest._
import breeze.linalg.{norm, DenseVector}
import breeze.signal._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * @author ktakagaki
 */
@RunWith(classOf[JUnitRunner])
class DesignFilterTest extends FunSuite {

  test("designFilterFirwin tested against output from scipy.signal.firwin (0.13.2-1)") {

    val testNormThreshold = 1.0E-10
    val firwin1 = designFilterFirwin[Double](
      6,
      DenseVector(0.5),
      nyquist = 1d,
      zeroPass = true,
      scale = true,
      multiplier = 1d,
      optWindow = OptWindowFunction.Hamming())
    assert(norm(testFirwin1 - firwin1.kernel) < testNormThreshold)

    val firwin2 = designFilterFirwin[Double](
      3,
      DenseVector(10.0),
      nyquist = 15.0,
      zeroPass = true,
      scale = false,
      multiplier = 1d,
      optWindow = OptWindowFunction.Hamming())
    assert(norm(testFirwin2 - firwin2.kernel) < testNormThreshold)

    val firwin3 = designFilterFirwin[Double](
      5,
      DenseVector(6.0, 10.0),
      nyquist = 15.0,
      zeroPass = false,
      scale = false,
      multiplier = 1d,
      optWindow = OptWindowFunction.Hamming())
    assert(norm(testFirwin3 - firwin3.kernel) < testNormThreshold)

    val firwin4 = designFilterFirwin[Double](
      4,
      DenseVector(1.0, 8.0),
      nyquist = 10.0,
      zeroPass = false,
      scale = false,
      multiplier = 1d,
      optWindow = OptWindowFunction.Hanning())
    assert(norm(testFirwin4 - firwin4.kernel) < testNormThreshold)

    val firwin5 = designFilterFirwin[Double](
      5,
      DenseVector(4.0, 9.0),
      nyquist = 10.0,
      zeroPass = true,
      scale = false,
      multiplier = 1d,
      optWindow = OptWindowFunction.Blackman())
    assert(norm(testFirwin5 - firwin5.kernel) < testNormThreshold)
  }

  val testFirwin1 = DenseVector(-0.0077763127191025679, 0.064454645578710029, 0.44332166714039256, 0.44332166714039256,
    0.064454645578710029, -0.0077763127191025679)
  val testFirwin2 = DenseVector(0.022053155816871686, 0.66666666666666663, 0.022053155816871686)
  val testFirwin3 = DenseVector(
    -0.018510492178744953,
    -0.01461577162249808,
    0.26666666666666661,
    -0.01461577162249808,
    -0.018510492178744953)
  val testFirwin4 = DenseVector(
    -0.000000000000000000e+00,
    3.794040820411270776e-01,
    3.794040820411272441e-01,
    -0.000000000000000000e+00)
  val testFirwin5 = DenseVector(
    -2.596504355380610970e-18,
    6.948495923029152088e-02,
    4.999999999999998335e-01,
    6.948495923029154864e-02,
    -2.596504355380610970e-18)

}
