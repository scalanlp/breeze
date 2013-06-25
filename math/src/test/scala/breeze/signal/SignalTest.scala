package breeze.signal

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import breeze.linalg.{DenseVector, norm}
import breeze.math.Complex
import breeze.signal.support.{CanFFT, CanIFFT}

/**
 * Created with IntelliJ IDEA.
 * User: takagaki
 * Date: 14.05.13
 * Time: 02:31
 * To change this template use File | Settings | File Templates.
 */
@RunWith(classOf[JUnitRunner])
class SignalTest extends FunSuite {

  val testNormThreshold = 1E-12

  val testReal16 = DenseVector[Double]( 0.814723686393179, 0.905791937075619, 0.126986816293506, 0.913375856139019,
     0.63235924622541, 0.0975404049994095, 0.278498218867048, 0.546881519204984, 0.957506835434298, 0.964888535199277,
     0.157613081677548, 0.970592781760616, 0.957166948242946, 0.485375648722841, 0.8002804688888, 0.141886338627215 )

  val testReal16C = DenseVector[Complex](
    Complex(0.814723686393179, 0),  Complex(0.905791937075619, 0),  Complex(0.126986816293506, 0),  Complex(0.913375856139019, 0),
    Complex(0.63235924622541, 0),  Complex(0.0975404049994095, 0),  Complex(0.278498218867048, 0),  Complex(0.546881519204984, 0),
    Complex(0.957506835434298, 0),  Complex(0.964888535199277, 0),  Complex(0.157613081677548, 0),  Complex(0.970592781760616, 0),
    Complex(0.957166948242946, 0),  Complex(0.485375648722841, 0),  Complex(0.8002804688888, 0),  Complex(0.141886338627215, 0)
  )

  val testReal16fft = DenseVector[Complex](
    Complex(9.75146832375171, 0),  Complex(-0.0977261644584599, 0.994224442620027),
    Complex(0.248156703823313, -0.961542739609668),  Complex(-0.973134608372272, -0.424078607189411),
    Complex(1.99837813056893, 0.119139969734688),  Complex(-0.00703114442532593, -0.5556868176116),
    Complex(0.11725195089493, -2.54990031917926),  Complex(0.506759321091583, -0.436614575872306),
    Complex(-0.301197719706247, 0),  Complex(0.506759321091583, 0.436614575872306),  Complex(0.11725195089493, 2.54990031917926),
    Complex(-0.00703114442532593, 0.5556868176116),  Complex(1.99837813056893, -0.119139969734688),
    Complex(-0.973134608372272, 0.424078607189411),  Complex(0.248156703823313, 0.961542739609668),
    Complex(-0.0977261644584599, -0.994224442620027) )

  val testReal16ifft = DenseVector[Complex](
    Complex(0.609466770234482, 0),  Complex(-0.00610788527865374, -0.0621390276637517),
    Complex(0.0155097939889571, 0.0600964212256043),  Complex(-0.060820913023267, 0.0265049129493382),
    Complex(0.124898633160558, -0.00744624810841799),  Complex(-0.000439446526582871, 0.034730426100725),
    Complex(0.00732824693093313, 0.159368769948704),  Complex(0.0316724575682239, 0.0272884109920191),
    Complex(-0.0188248574816404, 0),  Complex(0.0316724575682239, -0.0272884109920191),
    Complex(0.00732824693093313, -0.159368769948704),  Complex(-0.000439446526582871, -0.034730426100725),
    Complex(0.124898633160558, 0.00744624810841799),  Complex(-0.060820913023267, -0.0265049129493382),
    Complex(0.0155097939889571, -0.0600964212256043),  Complex(-0.00610788527865374, 0.0621390276637517) )


  test("fft 1D of DenseVector[Complex]") {
    assert( norm( fft(testReal16C) - testReal16fft ) < testNormThreshold )
  }

  test("fft 1D of DenseVector[Double]") {
    assert( norm( fft(testReal16) - testReal16fft ) < testNormThreshold )
  }

  test("ifft 1D of DenseVector[Complex]") {
    assert( norm( ifft(testReal16fft) - testReal16C ) < testNormThreshold )
  }

  test("ifft 1D of DenseVector[Double]") {
    assert( norm( ifft(testReal16) - testReal16ifft ) < testNormThreshold )
  }

}
