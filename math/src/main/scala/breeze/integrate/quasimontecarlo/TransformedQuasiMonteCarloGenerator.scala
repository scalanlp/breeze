package breeze.integrate.quasimontecarlo

import breeze.stats.distributions.{Exponential, RandBasis}

/*
 Copyright 2016 Chris Stucchio

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

trait ProvidesTransformedQuasiMonteCarlo {

  def quasiMonteCarloIntegrate(f: Array[Double] => Double)(variables: QuasiRandomVariableSpec*)(numSamples: Long) = {
    val generator = new TransformedQuasiMonteCarloGenerator(variables: _*)
    var fSum: Double = 0

    // cforRange for longs
    var i: Long = 0
    while (i < numSamples) {
      fSum += f(generator.getNextUnsafe)
      i += 1
    }
    fSum / numSamples
  }

  sealed trait QuasiRandomVariableSpec {
    val numInputs: Int
    def copy: QuasiRandomVariableSpec //As a performance hack, a spec may contain hidden mutable variables. So users always need to copy them before use.
  }

  trait TransformingQuasiRandomVariableSpec extends QuasiRandomVariableSpec {
    def transform(x: Array[Double], position: Int): Double
  }

  trait RejectionQuasiRandomVariableSpec extends QuasiRandomVariableSpec {
    /*
     * The function accept will be called. If true, then the function compute will be called.
     * This order will always be followed, so the implementer can cache values computed during
     * accept.
     *
     * Of course, this does require that this trait not be used concurrently, i.e. copies of the
     * spec always need to be created.
     *
     * Unfortunately in practice, a more scala-ish style of return an Option[Double] is way too slow.
     */
    def accept(x: Array[Double], position: Int): Boolean
    def compute(x: Array[Double], position: Int): Double
  }

  case class DistributionRandomVariableSpec(icdfProvider: breeze.stats.distributions.HasInverseCdf)
      extends TransformingQuasiRandomVariableSpec {
    val numInputs = 1
    def transform(x: Array[Double], position: Int): Double = icdfProvider.inverseCdf(x(position))
    def copy = DistributionRandomVariableSpec(icdfProvider)
  }

  trait RejectionSampledGammaQuasiRandomVariable extends RejectionQuasiRandomVariableSpec

  object RejectionSampledGammaQuasiRandomVariable {
    def apply(alpha: Double, beta: Double): QuasiRandomVariableSpec = {
      require(alpha > 0)
      require(beta > 0)
      if (alpha == 1.0) {
        new DistributionRandomVariableSpec(Exponential(1/beta)(RandBasis.mt0))
      } else if (alpha > 1) {
        GammaQuasiRandomVariableSpecAlphaGeq1(alpha, beta)
      } else {
        GammaQuasiRandomVariableSpecAlphaLeq1(alpha, beta)
      }
    }
  }

  case class GammaQuasiRandomVariableSpecAlphaLeq1(alpha: Double, theta: Double)
      extends RejectionSampledGammaQuasiRandomVariable {
    /*
     * Uses Algorithm 1 from http://home.iitk.ac.in/~kundu/paper120.pdf.
     */

    require(alpha < 1)
    val numInputs = 2

    private val b = (alpha + math.E) / math.E
    private val one_over_alpha = 1.0 / alpha
    private val two_to_alpha_minus_one = math.pow(2, alpha - 1)

    private var x: Double = 0
    def accept(rvs: Array[Double], position: Int): Boolean = {
      val u = rvs(position)
      val v = rvs(position + 1)

      x = -2 * math.log(1 - math.pow(u, one_over_alpha))
      val exp_minus_x_over_two = math.exp(-0.5 * x)
      v <= (math.pow(x, alpha - 1) * exp_minus_x_over_two) / (two_to_alpha_minus_one * math.pow(
        1 - exp_minus_x_over_two,
        alpha - 1))
    }

    def compute(rvs: Array[Double], position: Int): Double = (theta * x)

    def copy = GammaQuasiRandomVariableSpecAlphaLeq1(alpha, theta)
  }

  case class GammaQuasiRandomVariableSpecAlphaGeq1(alpha: Double, theta: Double)
      extends RejectionSampledGammaQuasiRandomVariable {
    /*
     * Uses Algorithm 8 from http://arxiv.org/pdf/1403.5599.pdf
     *
     * Note that Algorithm 9 from this same paper purports to work for alpha < 1, but I (Chris Stucchio, circa Jan 30 2016)
     * can't seem to make it work.
     */
    require(alpha > 1)

    val numInputs = 2
    private val a = 1.0 / math.sqrt(2 * alpha - 1.0)
    private val b = alpha - math.log(4.0)
    private val c = alpha + 1.0 / a

    private var x: Double = 0

    def accept(rvs: Array[Double], position: Int): Boolean = {
      val u = rvs(position)
      val v = rvs(position + 1)
      val y = a * math.log(u / (1 - u))
      x = alpha * math.exp(y)
      val z = u * u * v
      val r = b + (c * y) - x
      (r + 2.5040774 - 4.5 * z >= 0) || (r >= math.log(z))
    }

    def compute(rvs: Array[Double], position: Int): Double = theta * x

    def copy = GammaQuasiRandomVariableSpecAlphaGeq1(alpha, theta)
  }

  class TransformedQuasiMonteCarloGenerator(val inVariables: List[QuasiRandomVariableSpec])
      extends QuasiMonteCarloGenerator {
    def this(inVariables: QuasiRandomVariableSpec*) = this(inVariables.toList)
    val variables = inVariables.map(x => x.copy).toArray

    val dimension = variables.size
    val inputDimension = variables.map(x => x.numInputs).sum
    private val baseGenerator = new BaseUniformHaltonGenerator(inputDimension)

    private val currentValue: Array[Double] = new Array[Double](dimension)

    private var generatedCount: Long = 0
    def numGenerated: Long = generatedCount

    private var rejectedCount: Array[Long] = new Array[Long](dimension)
    def numRejections: Long = rejectedCount.sum
    def numRejectionsByVariable: Array[Long] = rejectedCount.clone

    def getNextUnsafe = {
      var accepted = false
      while (!accepted) {
        accepted = true
        val next = baseGenerator.getNextUnsafe
        var inputPosition = 0
        var i = 0
        while ((i < dimension) && accepted) {
          variables(i) match {
            case (v: TransformingQuasiRandomVariableSpec) => { currentValue(i) = v.transform(next, inputPosition) }
            case (v: RejectionQuasiRandomVariableSpec) => {
              if (v.accept(next, inputPosition)) {
                currentValue(i) = v.compute(next, inputPosition)
              } else {
                rejectedCount(i) = rejectedCount(i) + 1
                accepted = false
              }
            }
          }
          inputPosition = inputPosition + variables(i).numInputs
          i = i + 1
        }
      }
      generatedCount += 1
      currentValue
    }
  }

}
