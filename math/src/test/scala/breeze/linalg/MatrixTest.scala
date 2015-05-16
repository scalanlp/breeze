package breeze.linalg

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
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith
import breeze.math.Complex

@RunWith(classOf[JUnitRunner])
class MatrixTest extends FunSuite with Checkers {
  test("Multiply") {
    val a = Matrix((1.0, 2.0, 3.0),(4.0, 5.0, 6.0))
    val ad = DenseMatrix((1.0, 2.0, 3.0),(4.0, 5.0, 6.0))
    val b = Matrix((7.0, -2.0, 8.0),(-3.0, -3.0, 1.0),(12.0, 0.0, 5.0))
    val bd = DenseMatrix((7.0, -2.0, 8.0),(-3.0, -3.0, 1.0),(12.0, 0.0, 5.0))
    val c = DenseVector(6.0,2.0,3.0)
    assert( (a * b: Matrix[Double]) === Matrix((37.0, -8.0, 25.0), (85.0, -23.0, 67.0)))
    assert((ad * b :DenseMatrix[Double])=== DenseMatrix((37.0, -8.0, 25.0), (85.0, -23.0, 67.0)))
    assert(a * c === DenseVector(19.0,52.0))
    assert(b * c === DenseVector(62.0, -21.0, 87.0))
//    assert(b.t * c === DenseVector(72.0, -18.0, 65.0))
//    assert(a.t * DenseVector(4.0, 3.0) === DenseVector(16.0, 23.0, 30.0))

    // should be dense
//    val x = a * a.t
//    assert(x === DenseMatrix((14.0,32.0),(32.0,77.0)))

    // should be dense
//    val y = a.t * a
//    assert(y === DenseMatrix((17.0,22.0,27.0),(22.0,29.0,36.0),(27.0,36.0,45.0)))

//    val z : DenseMatrix[Double] = b * (b + 1.0)
//    assert(z === DenseMatrix((164.0,5.0,107.0),(-5.0,10.0,-27.0),(161.0,-7.0,138.0)))
  }

  test("big multiply bug around 256") {
    val phi2: Matrix[Double] = DenseMatrix.ones[Double](400, 5)
    val w2: Matrix[Double] = DenseMatrix.ones[Double](5, 24)

    val theta2 = (phi2 * w2)//.toDenseMatrix
    assert(theta2(256,0) != 0)
  }

  test("Setting") {
    val a: Matrix[Double] = Matrix((1.0, 2.0, 3.0),(4.0, 5.0, 6.0))
    val b = Matrix((7.0, -2.0, 8.0),(-3.0, -3.0, 1.0))
    val c = DenseMatrix((3.0, -1.0, 9.0),(-2.0, -2.0, 2.0))
    a := b
    assert(a === b)
    a := c
    assert(a === c)
  }



  test("Generic  ops") {
    // mostly for coverage
    val a = Matrix.create[String](1,1, Array("SSS"))
    intercept[IndexOutOfBoundsException] {
      a(3,3) = ":("
      assert(false, "Shouldn't be here!")
    }
    assert(a(0,0) === "SSS")
    intercept[IndexOutOfBoundsException] {
      a(3,3)
      assert(false, "Shouldn't be here!")
    }
    a(0,0) = ":("
    assert(a(0,0) === ":(")
  }

  test("Multiply Complex") {

    val a = Matrix((Complex(1,1), Complex(2,2), Complex(3,3)),
      (Complex(4,4), Complex(5,5), Complex(6,6)))
    val b = Matrix((Complex(7,7), Complex(-2,-2), Complex(8,8)),
      (Complex(-3,-3), Complex(-3,-3), Complex(1,1)),
      (Complex(12,12), Complex(0,0), Complex(5,5)))
    val c = DenseVector(Complex(6,0), Complex(2,0), Complex(3,0))
    val cs = SparseVector(Complex(6,0), Complex(2,0), Complex(3,0))
    val value: Matrix[Complex] = a * b
    assert(value === Matrix((Complex(0,74), Complex(0,-16), Complex(0,50)),
      (Complex(0,170), Complex(0,-46), Complex(0,134))))
    assert(b * c === DenseVector(Complex(62,62), Complex(-21,-21), Complex(87,87)))
    assert(b * cs === DenseVector(Complex(62,62), Complex(-21,-21), Complex(87,87)))
//    assert(b.t * c === DenseVector(Complex(72,-72), Complex(-18,18), Complex(65,-65)))
  }

  test("Other complex") {
    val a = Matrix((Complex(1,1), Complex(2,2), Complex(3,3)),
      (Complex(4,4), Complex(5,5), Complex(6,6)))


    a += Complex(1, 1)

    assert(a === Matrix((Complex(2,2), Complex(3,3), Complex(4,4)),
      (Complex(5,5), Complex(6,6), Complex(7,7))))

    a -= Complex(1, 1)

    assert(a === Matrix((Complex(1,1), Complex(2,2), Complex(3,3)),
      (Complex(4,4), Complex(5,5), Complex(6,6))))
  }


//  test("MapValues") {
//    val a : Matrix[Int] = Matrix((1,0,0),(2,3,-1))
//
//    val b1 : Matrix[Int] = a.mapValues(_ + 1)
//    assert(b1 === Matrix((2,1,1),(3,4,0)))
//
//    val b2 : Matrix[Double] = a.mapValues(_ + 1.0)
//    assert(b2 === Matrix((2.0,1.0,1.0),(3.0,4.0,0.0)))
//  }
}

