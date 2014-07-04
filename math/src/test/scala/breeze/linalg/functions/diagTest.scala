package breeze.linalg.functions

/*
 Copyright 2012 David Hall

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
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

/**
 * breeze
 * 7/3/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 */
@RunWith(classOf[JUnitRunner])
class diagTest extends FunSuite with Checkers {
  val testDV = DenseVector(0.1,1.1,2.1,3.1,4.1)
  val testDM = DenseMatrix.tabulate[Double](5,5)((r,c) => if (r == c) r.toDouble + 0.1 else 0.0)
  val testCSC = CSCMatrix.tabulate[Double](5,5)((r,c) => if (r == c) r.toDouble + 0.1 else 0.0)
  val testSV = SparseVector(0.1,1.1,2.1,3.1,4.1)

  test("diagEquiv") {
    assert(diag(testDV) === testDM)
    assert(diag(testDM) === testDV)
    assert(diag(testSV) === testCSC)
    assert(diag(testCSC) === testSV)
  }

}
