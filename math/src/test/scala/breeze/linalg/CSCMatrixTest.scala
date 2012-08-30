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

@RunWith(classOf[JUnitRunner])
class CSCMatrixTest extends FunSuite with Checkers {
  test("Multiply") {
    val a = CSCMatrix((1., 2., 3.),(4., 5., 6.))
    val ad = DenseMatrix((1., 2., 3.),(4., 5., 6.))
    val b = CSCMatrix((7., -2., 8.),(-3., -3., 1.),(12., 0., 5.))
    val bd = DenseMatrix((7., -2., 8.),(-3., -3., 1.),(12., 0., 5.))
    val c = DenseVector(6.,2.,3.)
    assert( (a * b: CSCMatrix[Double]) === CSCMatrix((37., -8., 25.), (85., -23., 67.)))
    assert((a * bd :DenseMatrix[Double])=== DenseMatrix((37., -8., 25.), (85., -23., 67.)))
    assert((ad * b :DenseMatrix[Double])=== DenseMatrix((37., -8., 25.), (85., -23., 67.)))
    assert(a * c === DenseVector(19.,52.))
    assert(b * c === DenseVector(62., -21., 87.))
//    assert(b.t * c === DenseVector(72., -18., 65.))
//    assert(a.t * DenseVector(4., 3.) === DenseVector(16., 23., 30.))

    // should be dense
//    val x = a * a.t
//    assert(x === DenseMatrix((14.,32.),(32.,77.)))

    // should be dense
//    val y = a.t * a
//    assert(y === DenseMatrix((17.,22.,27.),(22.,29.,36.),(27.,36.,45.)))

//    val z : DenseMatrix[Double] = b * (b + 1.0)
//    assert(z === DenseMatrix((164.,5.,107.),(-5.,10.,-27.),(161.,-7.,138.)))
  }

  test("Multiply Int") {
    val a = CSCMatrix((1, 2, 3),(4, 5, 6))
    val b = CSCMatrix((7, -2, 8),(-3, -3, 1),(12, 0, 5))
    val bd = DenseMatrix((7, -2, 8),(-3, -3, 1),(12, 0, 5))
    val c = DenseVector(6,2,3)
    assert(a * b === CSCMatrix((37, -8, 25), (85, -23, 67)))
    assert(a * bd === DenseMatrix((37, -8, 25), (85, -23, 67)))
    assert(a * c === DenseVector(19,52))
    assert(b * c === DenseVector(62, -21, 87))
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
    val cs = builder.result()
    assert(cs === CSCMatrix((0.0, 0.0, 0.0), (0.0, 2.0, 0.0), (0.0, 0.0, 0.0)))
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
    val a = CSCMatrix((1., 2., 3.),(4., 5., 6.))
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
    val a = CSCMatrix((1., 2., 3.),(4., 5., 6.))
    assert(cs === a)
  }
}

