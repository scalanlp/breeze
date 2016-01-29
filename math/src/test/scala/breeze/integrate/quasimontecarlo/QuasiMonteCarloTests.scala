package breeze.integrate.quasimontecarlo

import breeze.stats.distributions._
import org.scalatest.{FunSuite, WordSpec}
import org.scalatest.Matchers

class QuasiMonteCarloTest extends WordSpec with Matchers {
  def identityFunc(x: Array[Double]): Double = x(0)

  def productFunc(x: Array[Double]): Double = x(0)*x(1)

  "quasiMonteCarloIntegrate" should {
    "approximate the mean of a normal distribution" in {
      val integrationResult = quasiMonteCarloIntegrate( identityFunc _)(Gaussian(3, 5).toQuasi)(32*1024)
      math.abs(integrationResult - 3) should be < 0.001
    }
    "approximate the mean of a gamma distribution, alpha == 1" in {
      val integrationResult = quasiMonteCarloIntegrate( identityFunc _)(RejectionSampledGammaQuasiRandomVariable(1, 4))(32*1024)
      math.abs(integrationResult - 4) should be < 0.001
    }
    "approximate the mean of a gamma distribution, alpha > 1" in {
      val alpha = 3.0
      val beta = 5.0
      val integrationResult = quasiMonteCarloIntegrate( identityFunc _)(RejectionSampledGammaQuasiRandomVariable(alpha, beta))(128*1024)
      math.abs(integrationResult - alpha*beta) should be < 0.001
    }
    "approximate the mean of a gamma distribution, alpha < 1" in {
      val alpha = 0.35
      val beta = 5.0
      val integrationResult = quasiMonteCarloIntegrate( identityFunc _)(RejectionSampledGammaQuasiRandomVariable(alpha, beta))(128*1024)
      math.abs(integrationResult - alpha*beta) should be < 0.001
    }
    "approximate the mean of a product distribution" in {
      val alpha = 2.0
      val beta = 5.0
      val mu = 3.0
      val sigma = 5.0
      val integrationResult = quasiMonteCarloIntegrate( productFunc _)(Gaussian(mu, sigma).toQuasi, RejectionSampledGammaQuasiRandomVariable(alpha, beta))(16*1024*1024)
      println("Integration result: " + integrationResult + " should be " + alpha*beta*mu)
      math.abs(integrationResult - alpha*beta*mu) should be < 0.001
    }
  }
}
