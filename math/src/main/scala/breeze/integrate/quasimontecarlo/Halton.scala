package breeze.integrate.quasimontecarlo

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

import java.util.Vector
import spire.implicits.cfor
import breeze.linalg._

object Halton {
  val HALTON_MAX_DIMENSION = 1229

  private def readClasspathFileToIntArray(filename: String): Array[Int] = {
    /*
     * Reads a file from the classpath to an array int's.
     * The file should be stored as text, with integers separated by a ',' and perhaps arbitrary whitespace, including newlines.
     */
    val fileStream = this.getClass().getClassLoader().getResourceAsStream(filename)
    val lines = scala.io.Source.fromInputStream(fileStream).getLines()
    val nums = lines.flatMap(x => x.split(',')).map(x => x.replaceAll("\\s+",""))
    nums.map(x => x.toInt).toArray
  }

  lazy val PRIMES = readClasspathFileToIntArray("primes.txt")
  lazy val EA_PERMS = readClasspathFileToIntArray("quasimontecarlo_halton_ea_perms.txt")

  def integrate(func: Array[Double] => Double)(dimension: Int, numSamples: Long): Double = {
    val gen = new BaseUniformHaltonGenerator(dimension)
    var result: Double = 0
    cfor(0)(i => i < numSamples, i => i+1)(i => {
      result += func(gen.getNextUnsafe)
    })
    result / numSamples
  }
}

class BaseUniformHaltonGenerator(val dimension: Int) extends QuasiMonteCarloGenerator {
  /*
   * Provides a generalized Halton sequence:
   * https://en.wikipedia.org/wiki/Halton_sequence
   * http://www.genetic-programming.org/hc2013/DeRainville-ACM-MCS-2012-Paper.pdf
   *
   * This is a fairly primitive, highly optimized class that probably shouldn't be used in user code.
   *
   * This generates a sequence of *uniform* quasi-random variables, i.e. variables that are
   * can be used to approximate \int f(x) dU(x) where dU(x) is the uniform distribution on
   * [0,1].
   */

  private val bases = java.util.Arrays.copyOfRange(Halton.PRIMES, 0, dimension)

  private var count: Long = 0
  private val counters: Array[Vector[Int]] = List.fill(dimension)({ new java.util.Vector[Int]() }).toArray
  val permutations: Array[Array[Long]] =
    (0 to dimension).map(i => {
      val vv = new Array[Long](Halton.PRIMES(i))
      cfor(0)(j => j < Halton.PRIMES(i), j => j+1)(j => {
        vv(j) = j
      })
      shuffle(vv)
      vv
    }).toArray

  private val currentValue = new Array[Double](dimension)

  private var generatedCount: Long = 0
  def numGenerated: Long = generatedCount

  def getNextUnsafe = {
    cfor(0)(j => j < dimension, j => j + 1)(j => {
      var lIndex: Int = 0
      while((lIndex < counters(j).size()) && (counters(j).get(lIndex) == (bases(j)-1))) {
	counters(j).set(lIndex, 0)
	lIndex += 1
      }

      if (lIndex == counters(j).size()) {
        counters(j).add(1)
      } else {
        counters(j).set(lIndex, counters(j).get(lIndex) + 1)
      }

      var lCountSizeI: Int = counters(j).size()
      var lBasesPow: Long = bases(j)
      var lValue: Double = permutations(j)(counters(j).get(lCountSizeI-1))
      cfor(lCountSizeI-1)(k => k >= 1, k => k - 1)(k => {
	lValue += permutations(j)(counters(j).get(k-1)) * lBasesPow
	lBasesPow *= bases(j)
      })

      currentValue(j) = lValue.toDouble / lBasesPow.toDouble
    })
    generatedCount += 1
    currentValue
  }
}
