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

import breeze.linalg._
import org.scalacheck._
import org.scalatest._
import org.scalatestplus.scalacheck._

trait OptimizeTestBaseTrait {
  import org.scalacheck.Arbitrary._
  implicit val arbVector: Arbitrary[DenseVector[Double]] = Arbitrary {
    RandomInstanceSupport
      .genDenseVector[Double](10, RandomInstanceSupport.genReasonableDouble.arbitrary.map(_ % 10 + 1e-2))
  }

  implicit val arbVectorFloat: Arbitrary[DenseVector[Float]] = Arbitrary {
    RandomInstanceSupport
      .genDenseVector[Float](10, RandomInstanceSupport.genReasonableFloat.arbitrary.map(_ % 10 + 1e-2f))
  }

  implicit def arbSV: Arbitrary[SparseVector[Double]] =
    Arbitrary {
      RandomInstanceSupport
        .genSparseVector[Double](10, RandomInstanceSupport.genReasonableDouble.arbitrary.map(_ % 10 + 1e-2))
    }

  implicit val arbDoubleCounter: Arbitrary[Counter[String, Double]] = Arbitrary(for {
    v <- arbitrary[DenseVector[Double]]
  } yield {
    val c = Counter[String, Double]()
    for (i <- 0 until v.size) {
      c(i + "") = v(i)
    }
    c
  })

}

class OptimizeTestBase extends FunSuite with Checkers with OptimizeTestBaseTrait
