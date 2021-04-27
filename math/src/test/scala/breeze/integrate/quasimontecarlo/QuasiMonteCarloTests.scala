package breeze.integrate.quasimontecarlo

/*
 Copyright 2009 Chris Stucchio

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

import breeze.stats.distributions._
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest._
import matchers.should.Matchers._

class QuasiMonteCarloTest extends AnyWordSpec {
  def identityFunc(x: Array[Double]): Double = x(0)

  def productFunc(x: Array[Double]): Double = x(0) * x(1)

  "quasiMonteCarloIntegrate" should {
    "approximate the mean of a normal distribution" in {
      val integrationResult = quasiMonteCarloIntegrate(identityFunc _)(Gaussian(3, 5).toQuasi)(128 * 1024)
      math.abs(integrationResult - 3) should be < 0.001
    }
    "approximate the mean of a gamma distribution, alpha == 1" in {
      val integrationResult =
        quasiMonteCarloIntegrate(identityFunc _)(RejectionSampledGammaQuasiRandomVariable(1, 4))(128 * 1024)
      math.abs(integrationResult - 4) should be < 0.001
    }
    "approximate the mean of a gamma distribution, alpha > 1" in {
      val alpha = 3.0
      val beta = 5.0
      val integrationResult =
        quasiMonteCarloIntegrate(identityFunc _)(RejectionSampledGammaQuasiRandomVariable(alpha, beta))(128 * 1024)
      math.abs(integrationResult - alpha * beta) should be < 0.001
    }
    "approximate the mean of a gamma distribution, alpha < 1" in {
      val alpha = 0.35
      val beta = 5.0
      val integrationResult =
        quasiMonteCarloIntegrate(identityFunc _)(RejectionSampledGammaQuasiRandomVariable(alpha, beta))(128 * 1024)
      math.abs(integrationResult - alpha * beta) should be < 0.001
    }
    "approximate the mean of a product distribution" in {
      val alpha = 2.0
      val beta = 5.0
      val mu = 3.0
      val sigma = 5.0
      val integrationResult = quasiMonteCarloIntegrate(productFunc _)(
        Gaussian(mu, sigma).toQuasi,
        RejectionSampledGammaQuasiRandomVariable(alpha, beta))(16 * 1024 * 1024)
      println("Integration result: " + integrationResult + " should be " + alpha * beta * mu)
      math.abs(integrationResult - alpha * beta * mu) should be < 0.001
    }
  }
}
