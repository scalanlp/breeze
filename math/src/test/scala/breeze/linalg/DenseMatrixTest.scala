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
class DenseMatrixTest extends FunSuite with Checkers {

  test("Slicing") {
    val m = DenseMatrix((0,1,2),
                        (3,4,5))

    // slice sub-matrix
    val s1  = m(0 to 1, 1 to 2)
    assert(s1 === DenseMatrix((1,2),(4,5)))
    s1 += 1
    assert(m === DenseMatrix((0,2,3),(3,5,6)))

    // slice row
    val s2 : DenseMatrix[Int] = m(0, ::)
    assert(s2.valuesIterator sameElements DenseVector(0,2,3).valuesIterator)
    s2 *= 2
    assert(m === DenseMatrix((0,4,6),(3,5,6)))

    // slice column
    val s3 : DenseVector[Int] = m(::, 1)
    assert(s3 === DenseVector(4,5))
    s3 -= 1
    assert(m === DenseMatrix((0,3,6),(3,4,6)))

    // slice rows
    val s4 = m(1 to 1, ::)
//    assert(s4 === DenseMatrix((3,4,6)))

    val mbig = DenseMatrix(
      (0,1,2,3,4,5),
      (3,4,5,6,7,8),
      (3,4,5,6,7,8),
      (5,4,5,9,7,8)
    )

    val sbig1 = mbig(::, 0 to 2 by 2)
    assert(sbig1 === DenseMatrix(
      (0,2),
      (3,5),
      (3,5),
      (5,5)
    ))

    // slice columns
    val s5 = m(::, 1 to 2)
    assert(s5 === DenseMatrix((3,6),(4,6)))

    // slice part of a row
    val s6a = m(0, 1 to 2)
    s6a += 1
    assert(m === DenseMatrix((0,4,7),(3,4,6)))

    // slice part of a column
    val s7a = m(0 to 1, 0)
    s7a += 2
    val s7b = m(0 to 1,0);
    s7b += 1;
    assert(m === DenseMatrix((3,4,7),(6,4,6)))
  }

  test("Transpose") {
    val m = DenseMatrix((1,2,3),(4,5,6))

    // check that the double transpose gives us back the original
    assert(m.t.t == m)

    // check static type and write-through
    val t = m.t
    assert(t === DenseMatrix((1,4),(2,5),(3,6)))
    t(0,1) = 0
    assert(m === DenseMatrix((1,2,3),(0,5,6)))
  }

  test("Sliced Transpose") {
    val m = DenseMatrix((0, 1, 2),
      (3, 4, 5))

    // column of original looks same as row of tranpose
    val sm1 = m(::, 1)
    val smt1 = m.t apply (1, ::)
    assert(sm1.valuesIterator sameElements smt1.valuesIterator)

    val sm2 = m(::, 2)
    val smt2 = m.t apply (2, ::)
    assert(sm2.valuesIterator sameElements smt2.valuesIterator)

    val sm1c = m(1, ::)
    val smt1c = m.t apply (::, 1)
    assert(sm1c.valuesIterator sameElements smt1c.valuesIterator, sm1c.toString + " is not " + smt1c.toString)

    val sm2c = m(0, ::)
    val smt2c = m.t apply (::, 0)
    assert(sm2c.valuesIterator sameElements smt2c.valuesIterator)

    // slice sub-matrix
    val s1 = m(0 to 1, 1 to 2)
    assert(s1 === DenseMatrix((1, 2), (4, 5)))

    val t1 = s1.t
    assert(t1 === DenseMatrix((1, 4), (2, 5)))

    val t1b = m.t apply (1 to 2, 0 to 1)
    assert(t1 === t1b)

    val s2 = m(0 to 1, 1)
    val t2 = m.t apply (1, 0 to 1)
    assert(s2.valuesIterator sameElements t2.valuesIterator)

    val s3 = m(0, 0 to 1)
    val t3 = m.t apply (0 to 1, 0)
    assert(s3.valuesIterator sameElements t3.valuesIterator)

    {
      val s2 = m(0 to 1, ::)
      val t2 = m.t apply (::, 0 to 1)
      assert(s2.t === t2)
      assert(s2 === t2.t)

      val s3 = m(::, 0 to 1)
      val t3 = m.t apply (0 to 1, ::)
      assert(s3.t === t3)
      assert(s3 === t3.t)
    }
  }

  test("Min/Max") {
    val m = DenseMatrix((1,0,0),(2,3,-1))
    assert(m.argmin === (1,2))
    assert(m.argmax === (1,1))
    assert(m.min === -1)
    assert(m.max === 3)
  }

  test("MapValues") {
    val a : DenseMatrix[Int] = DenseMatrix((1,0,0),(2,3,-1))

    val b1 : DenseMatrix[Int] = a.mapValues(_ + 1)
    assert(b1 === DenseMatrix((2,1,1),(3,4,0)))

    val b2 : DenseMatrix[Double] = a.mapValues(_ + 1.0)
    assert(b2 === DenseMatrix((2.0,1.0,1.0),(3.0,4.0,0.0)))
  }

  /*
  test("Map") {
    val a : DenseMatrix[Int] = DenseMatrix((1,0,0),(2,3,-1))

    val b1 : DenseMatrix[Int] = a.mapTriples((i,j,v) => i + v)
    assert(b1 === DenseMatrix((1,0,0),(3,4,0)))

    val b2 : DenseMatrix[Double] = a.mapTriples((i,j,v) => j + v.toDouble)
    assert(b2 === DenseMatrix((1.0,1.0,2.0),(2.0,4.0,1.0)))
  }

  test("Triples") {
    val a : DenseMatrix[Int] = DenseMatrix((1,0,0),(2,3,-1))

    var s = 0

    // foreach
    s = 0
    for ((i,j,v) <- a.triples) s += v
    assert(s === a.sum)

    // filter
    s = 0
    for ((i,j,v) <- a.triples; if i % 2 == 0 || j % 2 == 0) s += v
    assert(s === 1+2-1)

//    // map
//    val b1 : DenseMatrix[Double] = for ((i,j,v) <- a) yield v * 2.0
//    assert(b1 === DenseMatrix((2.0,0.0,0.0),(4.0,6.0,-2.0)))
//
//    // map with filter
//    val b2 : DenseMatrix[Int] = for ((i,j,v) <- a; if j == 0) yield v * 2
//    assert(b2 === DenseMatrix((2,0,0),(4,0,0)))
  }

*/

  test("set") {
    {
      val a = DenseMatrix.zeros[Int](2,2)
      val b = DenseMatrix((1,0),(2,3))
      a := b
      assert(a === b)

    }
    val a = DenseMatrix.zeros[Int](2,3)
    val b = DenseMatrix((1,0,5),(2,3,-1))
    a := b
    assert(a === b)
  }

  test("horzcat") {
    val a : DenseMatrix[Int] = DenseMatrix((1,0,5),(2,3,-1))
    val result: DenseMatrix[Int] = DenseMatrix((1,0,5,1,0, 5),(2,3,-1,2,3,-1))
    assert(DenseMatrix.horzcat(a,a) === result)
  }

  test("vertcat") {
    val a : DenseMatrix[Int] = DenseMatrix((1,0,5),(2,3,-1))
    val result: DenseMatrix[Int] = DenseMatrix((1,0,5),(2,3,-1),(1,0,5),(2,3,-1))
    assert(DenseMatrix.vertcat(a,a) === result)
  }


  test("Multiply") {
    val a = DenseMatrix((1., 2., 3.),(4., 5., 6.))
    val b = DenseMatrix((7., -2., 8.),(-3., -3., 1.),(12., 0., 5.))
    val c = DenseVector(6.,2.,3.)
    val cs = SparseVector(6.,2.,3.)
    assert(a * b === DenseMatrix((37., -8., 25.), (85., -23., 67.)))
    assert(a * c === DenseVector(19.,52.))
    assert(b * c === DenseVector(62., -21., 87.))
    assert(a * cs === DenseVector(19.,52.))
    assert(b * cs === DenseVector(62., -21., 87.))
    assert(b.t * c === DenseVector(72., -18., 65.))
    assert(a.t * DenseVector(4., 3.) === DenseVector(16., 23., 30.))

    // should be dense
    val x = a * a.t
    assert(x === DenseMatrix((14.,32.),(32.,77.)))

    // should be dense
    val y = a.t * a
    assert(y === DenseMatrix((17.,22.,27.),(22.,29.,36.),(27.,36.,45.)))

    val z : DenseMatrix[Double] = b * (b + 1.0)
    assert(z === DenseMatrix((164.,5.,107.),(-5.,10.,-27.),(161.,-7.,138.)))
  }


  test("Multiply Int") {
    val a = DenseMatrix((1, 2, 3),(4, 5, 6))
    val b = DenseMatrix((7, -2, 8),(-3, -3, 1),(12, 0, 5))
    val c = DenseVector(6,2,3)
    assert(a * b === DenseMatrix((37, -8, 25), (85, -23, 67)))
    assert(a * c === DenseVector(19,52))
    assert(b * c === DenseVector(62, -21, 87))
    assert(b.t * c === DenseVector(72, -18, 65))
    assert(a.t * DenseVector(4, 3) === DenseVector(16, 23, 30))

    // should be dense
    val x = a * a.t
    assert(x === DenseMatrix((14,32),(32,77)))

    // should be dense
    val y = a.t * a
    assert(y === DenseMatrix((17,22,27),(22,29,36),(27,36,45)))

    val z : DenseMatrix[Int] = b * (b + 1)
    assert(z === DenseMatrix((164,5,107),(-5,10,-27),(161,-7,138)))
  }

  test("Multiply Float") {
    val a = DenseMatrix((1.0f, 2.0f, 3.0f),(4.0f, 5.0f, 6.0f))
    val b = DenseMatrix((7.0f, -2.0f, 8.0f),(-3.0f, -3.0f, 1.0f),(12.0f, 0.0f, 5.0f))
    val c = DenseVector(6.0f,2.0f,3.0f)
    val cs = SparseVector(6.0f,2.0f,3.0f)
    assert(a * b === DenseMatrix((37.0f, -8.0f, 25.0f), (85.0f, -23.0f, 67.0f)))
    assert(a * c === DenseVector(19.0f,52.0f))
    assert(b * c === DenseVector(62.0f, -21.0f, 87.0f))
    assert(a * cs === DenseVector(19.0f,52.0f))
    assert(b * cs === DenseVector(62.0f, -21.0f, 87.0f))
    assert(b.t * c === DenseVector(72.0f, -18.0f, 65.0f))
    assert(a.t * DenseVector(4.0f, 3.0f) === DenseVector(16.0f, 23.0f, 30.0f))

    // should be dense
    val x = a * a.t
    assert(x === DenseMatrix((14.0f,32.0f),(32.0f,77.0f)))

    // should be dense
    val y = a.t * a
    assert(y === DenseMatrix((17.0f,22.0f,27.0f),(22.0f,29.0f,36.0f),(27.0f,36.0f,45.0f)))

    val z : DenseMatrix[Float] = b * (b + 1.0f)
    assert(z === DenseMatrix((164.0f,5.0f,107.0f),(-5.0f,10.0f,-27.0f),(161.0f,-7.0f,138.0f)))
  }


  test("Trace") {
    assert(DenseMatrix((1,2),(4,5)).trace === 1 + 5)
    assert(DenseMatrix((1,2,3),(3,4,5),(5,6,7)).trace == 1 + 4 + 7)
    assert(DenseMatrix((1,2,3),(4,5,6),(7,8,9)).trace === 1 + 5 + 9)
  }

  /*
  test("Reshape") {
    val m : DenseMatrix[Int] = DenseMatrix((1,2,3),(4,5,6))
    val r : DenseMatrix[Int] = m.reshape(3,2)
    assert(m.data eq r.data)
    assert(r.rows === 3)
    assert(r.cols === 2)
    assert(r === DenseMatrix((1,5),(4,3),(2,6)))
  }
  */

  test("Solve") {
    // square solve
    val r1 : DenseMatrix[Double] = DenseMatrix((1.0,3.0),(2.0,0.0)) \ DenseMatrix((1.0,2.0),(3.0,4.0))
    assert(r1 === DenseMatrix((1.5, 2.0), (-1.0/6, 0.0)))

    // matrix-vector solve
    val r2 : DenseVector[Double] = DenseMatrix((1.0,3.0,4.0),(2.0,0.0,6.0)) \ DenseVector(1.0,3.0)
    assert(r2 === DenseVector(0.1813186813186811, -0.3131868131868131, 0.43956043956043944))

    // wide matrix solve
    val r3 : DenseMatrix[Double] = DenseMatrix((1.0,3.0,4.0),(2.0,0.0,6.0)) \ DenseMatrix((1.0,2.0),(3.0,4.0))
    assert(r3 === DenseMatrix((0.1813186813186811,   0.2197802197802196),
                              (-0.3131868131868131, -0.1978021978021977),
                              (0.43956043956043944,  0.5934065934065933)))

    // tall matrix solve
    val r4 : DenseMatrix[Double] = DenseMatrix((1.0,3.0),(2.0,0.0),(4.0,6.0)) \ DenseMatrix((1.0,4.0),(2.0,5.0),(3.0,6.0))
    assert(r4 === DenseMatrix((0.9166666666666667,    1.9166666666666672),
                             (-0.08333333333333352, -0.08333333333333436)))
  }


  test("sum") {
    assert(sum(DenseMatrix((1.0,3.0),(2.0,4.0)), Axis._0) === DenseMatrix((3., 7.)))
    assert(sum(DenseMatrix((1.0,3.0),(2.0,4.0)), Axis._1) === DenseVector(4., 6.))
    assert(sum(DenseMatrix((1.0,3.0),(2.0,4.0))) === 10.0)
  }

  test("normalize rows and columns") {
    assert(normalize(DenseMatrix((1.0,3.0),(2.0,4.0)), Axis._0, 1) === DenseMatrix((1.0/3.0, 3.0/7.0), (2.0/3.0,4.0/7.0)))
    assert(normalize(DenseMatrix((1.0,3.0),(2.0,4.0)), Axis._1, 1) === DenseMatrix((1.0/4.0, 3.0/4.0), (2.0/6.0,4.0/6.0)))

  }

  test("Generic Dense ops") {
    // mostly for coverage
    val a = DenseMatrix.create[String](1,1, Array("SSS"))
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

    a := ":)"
    assert(a(0,0) === ":)")
    val b = DenseMatrix.zeros[String](1,1)
    b := a
    assert(b === a)

  }
}

