package breeze.stats.distributions

/*
 Copyright 2009 David Hall, Daniel Ramage
 
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

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.scalacheck._
import org.junit.runner.RunWith
import breeze.linalg.{SparseVector, DenseVector}

@RunWith(classOf[JUnitRunner])
class MultinomialTest extends FunSuite with Checkers with Matchers {
  // can't use the standard moment tester tools for a categorial distribution, so let's just roll our ownkj

  def TestDist = DenseVector[Double](0.2, 0.5, 0.3)
  def TestParams = TestDist * 2.0
  def NSamples = 1e7.toInt

  test("multinomial with naive sampling") {
    val mult = new Multinomial[DenseVector[Double],Int](TestParams)
    val accumNaive = DenseVector.zeros[Double](3)
    (0 until NSamples) foreach { i => accumNaive(mult.drawNaive()) += 1 }
    accumNaive /= NSamples.toDouble
    accumNaive(2) should be (TestDist(2) +- 1e-3)
  }

  test("multinomial with alias sampling") {
    val mult = new Multinomial[DenseVector[Double],Int](TestParams)
    val accumAlias = DenseVector.zeros[Double](3)
    (0 until NSamples) foreach { i => accumAlias(mult.draw()) += 1 }
    accumAlias /= NSamples.toDouble
    accumAlias(2) should be (TestDist(2) +- 1e-3)
  }
}
