package breeze.optimize.linear

/*
 Copyright 2010 David Hall, Daniel Ramage

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


import org.junit.runner.RunWith

import breeze.linalg._

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class InteriorPointTest extends FunSuite {
  // from: http://en.wikipedia.org/wiki/Karmarkar's_algorithm
  test("Small example") {
    val x0 = DenseVector(.2,.2)
    val c = DenseVector(-1.,-1.)
    val A = DenseMatrix.zeros[Double](11,2)
    val b = DenseVector.zeros[Double](11)

    for(i <- 0 to 10) {
      val p = i / 10.0
      A(i,0) = 2 * p
      A(i,1) = 1
      b(i) = p * p + 1
    }

    val x = InteriorPoint.minimize(A=A,b=b,c=c,x0=x0)
    assert( (A * x - b).activeValuesIterator.forall(_ < 1E-4), (A * x))
    assert( (x(0) - 0.5).abs < 1E-3, x(0))
    assert( (x(1) - 0.75).abs < 1E-3, x(1))
  }

  test("dsl") {
//    http://www.tu-chemnitz.de/mathematik/discrete/manuals/cplex/doc/getstart/html/cpxGSilocplex13.html
    val lp = new LinearProgram()
    import lp._
    val x0 = Real()
    val x1 = Real()
    val x2 = Real()

    val lpp =  ( (x0 +  x1 * 2 + x2 * 3 )
        subjectTo ( x0 * -1 + x1 + x2 <= 20)
        subjectTo ( x0 - x1 * 3 + x2 <= 30)
        subjectTo ( x0 <= 40 )
    )

    val result = maximize( lpp)

    assert( (result.result - DenseVector(40.,17.5,42.5)).norm(2) < 1E-4)


  }
}