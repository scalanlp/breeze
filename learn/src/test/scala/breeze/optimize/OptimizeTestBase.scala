package breeze.optimize

/*
 Copyright 2011 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
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

import breeze.linalg._

class OptimizeTestBase extends FunSuite with Checkers {
  import Arbitrary._
  implicit val arbVector : Arbitrary[DenseVector[Double]] = Arbitrary(for {
    n <- arbitrary[Int] suchThat { _ > 0 }
    d <- arbitrary[Double] map { _ % 10000 }
  } yield ( DenseVector.tabulate(n%3 + 1)(i => scala.math.random *d )))

  implicit val arbDoubleCounter: Arbitrary[Counter[String,Double]] = Arbitrary(for {
    v <- arbitrary[DenseVector[Double]]
  } yield {
    val c = Counter[String,Double]()
    for(i <- 0 until v.size) {
      c(i + "") = v(i)
    }
    c
  })

}
