package breeze.stats.distributions

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.matchers.should.Matchers._
import org.scalatest.funsuite._

class InvGammaTest extends AnyFunSuite with MomentsTestBase[Double] {

  override type Distr = InvGamma

  override val numSamples: Int = 50000

  implicit val arbDistr: Arbitrary[InvGamma] = Arbitrary {
    for {
      shape <- arbitrary[Double].map { x => math.abs(x) % 10.0 + 3.1 } // Gamma pdf at 0 not defined when shape == 1
      scale <- arbitrary[Double].map { x => math.abs(x) % 8.0 + 0.1 }
    } yield new InvGamma(shape, scale)
  }

  override def asDouble(x: Double): Double = x
  override def fromDouble(x: Double): Double = x

  private val eps = 1E-9

  test("should have expect values for shape=19.31, scale=1") {
    val g = InvGamma(19.31, 1)
    g.mean shouldBe 0.05461496450 +- eps
    g.variance shouldBe 0.0001723162534 +- eps
  }

  test("should have expect values for multiple shapes") {
    val means = Seq(10.0, 0.476190476, 0.2173913043)
    val vars = Seq(Double.NaN,  0.2061430632, 0.01312749422)
    val shapes = Seq(1.1, 3.1, 5.6)

    {
      val g = InvGamma(shapes.head, 1)
      g.mean shouldBe means.head +- eps
    }

    shapes.zipWithIndex.tail.foreach({ case (shape, i) =>
      val g = InvGamma(shape, 1)
      g.mean shouldBe means(i) +- eps
      g.variance shouldBe vars(i) +- eps
    })
  }
}
