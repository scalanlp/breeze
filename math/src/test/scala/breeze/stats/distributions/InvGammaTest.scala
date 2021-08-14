package breeze.stats.distributions

import org.scalatest.matchers.should.Matchers._

class InvGammaTest extends org.scalatest.funspec.AnyFunSpec {
  private val eps = 1E-9

  describe("Inverse Gamma Distribution") {
    it("should have expect values for shape=19.31, scale=1") {
      val g = InvGamma(19.31, 1)
      g.mean shouldBe 0.05461496450 +- eps
      g.variance shouldBe 0.0001723162534 +- eps
    }

    it("should have expect values for multiple shapes") {
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
}
