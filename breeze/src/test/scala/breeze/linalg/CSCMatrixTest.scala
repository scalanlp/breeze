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
import breeze.math.Complex
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class CSCMatrixTest extends FunSuite with Checkers {
  test("Multiply") {
    val a = CSCMatrix((1.0, 2.0, 3.0),(4.0, 5.0, 6.0))
    val ad = DenseMatrix((1.0, 2.0, 3.0),(4.0, 5.0, 6.0))
    val b = CSCMatrix((7.0, -2.0, 8.0),(-3.0, -3.0, 1.0),(12.0, 0.0, 5.0))
    val bd = DenseMatrix((7.0, -2.0, 8.0),(-3.0, -3.0, 1.0),(12.0, 0.0, 5.0))
    val c = DenseVector(6.0,2.0,3.0)
    assert( (a * b: CSCMatrix[Double]) === CSCMatrix((37.0, -8.0, 25.0), (85.0, -23.0, 67.0)))
    assert((a * bd :DenseMatrix[Double])=== DenseMatrix((37.0, -8.0, 25.0), (85.0, -23.0, 67.0)))
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

  test("Multiply Int") {
    val a = CSCMatrix((1, 2, 3),(4, 5, 6))
    val b = CSCMatrix((7, -2, 8),(-3, -3, 1),(12, 0, 5))
    val bd = DenseMatrix((7, -2, 8),(-3, -3, 1),(12, 0, 5))
    val c = DenseVector(6,2,3)
    val cs = SparseVector(3)( (1,2))
    assert(a * b === CSCMatrix((37, -8, 25), (85, -23, 67)))
    assert(a * bd === DenseMatrix((37, -8, 25), (85, -23, 67)))
    assert(a * c === DenseVector(19,52))
    assert(b * c === DenseVector(62, -21, 87))
    assert(a * cs === SparseVector(4, 10))
    assert(b * cs === SparseVector(3)((0, -4), (1, -6)))

//    assert(b.t * c === DenseVector(72, -18, 65))
//    assert(a.t * DenseVector(4, 3) === DenseVector(16, 23, 30))

    // should be dense
//    val x = a * a.t
//    assert(x === DenseMatrix((14,32),(32,77)))

    // should be dense
//    val y = a.t * a
//    assert(y === DenseMatrix((17,22,27),(22,29,36),(27,36,45)))

//    val z : DenseMatrix[Double] = b * (b + 1.0)
//    assert(z === DenseMatrix((164,5,107),(-5,10,-27),(161,-7,138)))
  }
  
  test("Transpose") {
    val a = CSCMatrix.zeros[Int](2,3)
    a(0,0) = 1
    a(1,2) = 2
    
    val expected = CSCMatrix.zeros[Int](3,2)
    expected(0,0) = 1
    expected(2,1) = 2
    
    assert(a.t === expected)
  }
  
  test("Transpose Complex") {
    val a = CSCMatrix.zeros[Complex](2,3)
    a(0,0) = Complex(1,1)
    a(1,2) = Complex(-2,-2)
    
    val expected = CSCMatrix.zeros[Complex](3,2)
    expected(0,0) = Complex(1,-1)
    expected(2,1) = Complex(-2,2)
    
    assert(a.t === expected)
  }

  test("Generic CSC ops") {
    // mostly for coverage
    val a = CSCMatrix.create[String](1,1, Array("SSS"))
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

  test("Builder, simple") {
    val builder = new CSCMatrix.Builder[Double](3, 3)
    builder.add(1, 1, 2.0)
  }

  test("Builder, full") {
    val builder = new CSCMatrix.Builder[Double](2, 3)
    builder.add(0, 1, 2.0)
    builder.add(1, 1, 5.0)
    builder.add(0, 2, 3.0)
    builder.add(1, 0, 4.0)
    builder.add(1, 2, 6.0)
    builder.add(0, 0, 1.0)
    val cs = builder.result()
    val a = CSCMatrix((1.0, 2.0, 3.0),(4.0, 5.0, 6.0))
    assert(cs === a)
  }

  test("Builder, repeated full") {
    val builder = new CSCMatrix.Builder[Double](2, 3)
    builder.add(0, 1, 2.0)
    builder.add(1, 2, 3.0)
    builder.add(1, 1, 5.0)
    builder.add(0, 2, 3.0)
    builder.add(1, 0, 4.0)
    builder.add(1, 2, 3.0)
    builder.add(0, 0, 1.0)
    val cs = builder.result()
    val a = CSCMatrix((1.0, 2.0, 3.0),(4.0, 5.0, 6.0))
    assert(cs === a)
  }

  test("MapValues") {
    val a : CSCMatrix[Int] = CSCMatrix((1,0,0),(2,3,-1))

    val b1 : CSCMatrix[Int] = a.mapValues(_ + 1)
    assert(b1 === CSCMatrix((2,1,1),(3,4,0)))

    val b2 : CSCMatrix[Double] = a.mapValues(_ + 1.0)
    assert(b2 === CSCMatrix((2.0,1.0,1.0),(3.0,4.0,0.0)))
  }

  test("addition/subtraction") {
    val a : CSCMatrix[Int] = CSCMatrix((1,0,0),(2,3,-1))
    val b : CSCMatrix[Int] = CSCMatrix((0,1,0),(2,3,-1))
    assert(a + b === CSCMatrix((1, 1, 0), (4,6,-2)))
    assert(a - b === CSCMatrix((1, -1, 0), (0,0,0)))
  }
}

