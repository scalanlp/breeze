package breeze.signal

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import breeze.linalg._
import breeze.math._
import scala.reflect.ClassTag


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

  test("fft 1D of Complex (A)") {
    assert( norm( fft(testReal16C) - testReal16fft ) < testNormThreshold )
  }

  test("ifft 1D of Complex (A)") {
    assert( norm( ifft(testReal16fft) - testReal16C ) < testNormThreshold )
  }


}
