package breeze.signal.support

import org.scalatest._
import breeze.linalg.{norm, DenseVector}
import breeze.signal._

/**
 * @author ktakagaki
 */
class FilterDesignTest extends FunSuite {


  test("firwin tested against output from scipy.signal.firwin (0.13.2-1)") {
    val testNormThreshold = 1.0E-10
    val firwin1 = KernelDesign.firwin(6, DenseVector(0.5), OptWindowFunction.Hamming(),
                    zeroPass = true, nyquist = 1d, scale = true)
    assert( norm( testFirwin1 - firwin1.kernel) < testNormThreshold )
    val firwin2 = KernelDesign.firwin(3, DenseVector(10.0), OptWindowFunction.Hamming(),
      zeroPass = true, nyquist = 15.0, scale = false)
    assert( norm( testFirwin2 - firwin2.kernel) < testNormThreshold )
    val firwin3 = KernelDesign.firwin(5, DenseVector(6.0, 10.0), OptWindowFunction.Hamming(),
      zeroPass = false, nyquist = 15.0, scale = false)
    assert( norm( testFirwin3 - firwin3.kernel) < testNormThreshold )
  }

  val testFirwin1 = DenseVector( -0.0077763127191025679, 0.064454645578710029,  0.44332166714039256, 0.44332166714039256, 0.064454645578710029, -0.0077763127191025679)
  val testFirwin2 = DenseVector( 0.022053155816871686, 0.66666666666666663, 0.022053155816871686)
  val testFirwin3 = DenseVector( -0.018510492178744953, -0.01461577162249808, 0.26666666666666661, -0.01461577162249808,-0.018510492178744953)

}
