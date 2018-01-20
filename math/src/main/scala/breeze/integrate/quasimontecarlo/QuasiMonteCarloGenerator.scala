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

trait QuasiMonteCarloGenerator {
  /*
   * Mutable class that generates a sequence of quasi-random variables.
   */

  val dimension: Int

  /*
   * This is unsafe in the following sense.
   * Future calls to getNext, getNextInto, getNextUnsafe may
   * mutate the return value of this function.
   * Further, if you mutate the return value, then future calls
   * to this generator might be completely wrong.
   *
   * Using this is purely a performance optimization and should
   * be done extremely carefully.
   */
  def getNextUnsafe: Array[Double] // This must return an array of length this.dimension

  def getNext: Array[Double] = getNextUnsafe.clone()

  def getNextInto(to: Array[Double]) = java.lang.System.arraycopy(getNextUnsafe, 0, to, 0, dimension)

  def numGenerated: Long
}
