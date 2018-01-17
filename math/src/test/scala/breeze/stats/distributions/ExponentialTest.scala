package breeze.stats.distributions

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

/**
 * Created by kokorins
 */
@RunWith(classOf[JUnitRunner])
class ExponentialTest
    extends FunSuite
    with Checkers
    with MomentsTestBase[Double] /*with UnivariateContinuousDistrTestBase with ExpFamTest[Exponential, Double] with HasCdfTestBase*/ {
  type Distr = Exponential

  import org.scalacheck.Arbitrary.arbitrary

  val expFam = Exponential
  override val numSamples = 40000

  def asDouble(x: Double) = x

  def fromDouble(x: Double) = x

  implicit def arbParameter = Arbitrary {
    for (rate <- arbitrary[Double].map(_.abs % 200.0 + Double.MinPositiveValue)) yield rate
  }

//  override def paramsClose(p: Double, b: Double): Boolean = (p - b).abs / (p.abs / 2 + b.abs / 2 + 1) < 2E-1

  override implicit def arbDistr = Arbitrary {
    for (rate <- arbitrary[Double].map(x => math.abs(x) % 1000.0 + Double.MinPositiveValue))
      yield new Exponential(rate)(RandBasis.mt0)
  }
}
