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
import org.scalatest.funsuite._
import matchers.should.Matchers._
import org.scalatestplus.scalacheck._
import breeze.math.Complex
import breeze.numerics._
import breeze.stats.distributions.Rand
import breeze.storage.Zero
import breeze.util.DoubleImplicits

import scala.reflect.ClassTag

class DenseMatrixTest extends AnyFunSuite with Checkers with DoubleImplicits with MatrixTestUtils {

  test("Slicing") {
    val m = DenseMatrix((0, 1, 2), (3, 4, 5))

    // slice sub-matrix
    val s1 = m(0 to 1, 1 to 2)
    assert(s1 === DenseMatrix((1, 2), (4, 5)))
    s1 += 1
    assert(m === DenseMatrix((0, 2, 3), (3, 5, 6)))

    // slice row
    val s2 = m(0, ::)
    assert(s2 === DenseVector(0, 2, 3).t)
    s2 *= 2
    assert(m === DenseMatrix((0, 4, 6), (3, 5, 6)))

    // slice column
    val s3: DenseVector[Int] = m(::, 1)
    assert(s3 === DenseVector(4, 5))
    s3 -= 1
    assert(m === DenseMatrix((0, 3, 6), (3, 4, 6)))

    // slice rows
    val s4 = m(1 to 1, ::)
    assert(s4 === DenseMatrix((3, 4, 6)))

    val mbig = DenseMatrix(
      (0, 1, 2, 3, 4, 5),
      (3, 4, 5, 6, 7, 8),
      (3, 4, 5, 6, 7, 8),
      (5, 4, 5, 9, 7, 8)
    )

    val sbig1 = mbig(::, 0 to 2 by 2)
    assert(
      sbig1 === DenseMatrix(
        (0, 2),
        (3, 5),
        (3, 5),
        (5, 5)
      ))

    // slice columns
    val s5 = m(::, 1 to 2)
    assert(s5 === DenseMatrix((3, 6), (4, 6)))

    // slice part of a row
    val s6a = m(0, 1 to 2)
    s6a += 1
    assert(m === DenseMatrix((0, 4, 7), (3, 4, 6)))

    // slice part of a column
    val s7a = m(0 to 1, 0)
    s7a += 2
    val s7b = m(0 to 1, 0)
    s7b += 1
    assert(m === DenseMatrix((3, 4, 7), (6, 4, 6)))
  }

  test("Multiple Slicing") {
    val m = new DenseMatrix[Int](6, 6, (1 to 36).toArray)
    val slice1 = m(1 to 3, 1 to 3)
    assert(slice1(::, 1) === DenseVector(14, 15, 16))
    assert(slice1(::, 1 to 2) === DenseMatrix((14, 20), (15, 21), (16, 22)))
  }

  test("Transpose") {
    val m = DenseMatrix((1, 2, 3), (4, 5, 6))

    // check that the double transpose gives us back the original
    assert(m.t.t == m)

    // check static type and write-through
    val t = m.t
    assert(t === DenseMatrix((1, 4), (2, 5), (3, 6)))
    t(0, 1) = 0
    assert(m === DenseMatrix((1, 2, 3), (0, 5, 6)))
  }

  test("Sliced Transpose") {
    val m = DenseMatrix((0, 1, 2), (3, 4, 5))

    // column of original looks same as row of tranpose
    val sm1 = m(::, 1)
    val smt1 = m.t(1, ::)
    assert(sm1.t === smt1)

    val sm2 = m(::, 2)
    val smt2 = m.t(2, ::)
    assert(sm2.t === smt2)

    val sm1c = m(1, ::)
    val smt1c = m.t(::, 1)
    assert(sm1c === smt1c.t)

    val sm2c = m(0, ::)
    val smt2c = m.t(::, 0)
    assert(sm2c === smt2c.t)

    // slice sub-matrix
    val s1 = m(0 to 1, 1 to 2)
    assert(s1 === DenseMatrix((1, 2), (4, 5)))

    val t1 = s1.t
    assert(t1 === DenseMatrix((1, 4), (2, 5)))

    val t1b = m.t(1 to 2, 0 to 1)
    assert(t1 === t1b)

    val s2 = m(0 to 1, 1)

    val t2 = m.t(1, 0 to 1)
    assert(s2 === t2.t)

    val s3 = m(0, 0 to 1)
    val t3 = m.t(0 to 1, 0)
    assert(s3.t === t3)

    {
      val s2 = m(0 to 1, ::)
      val t2 = m.t(::, 0 to 1)
      assert(s2.t === t2)
      assert(s2 === t2.t)

      val s3 = m(::, 0 to 1)
      val t3 = m.t(0 to 1, ::)
      assert(s3.t === t3)
      assert(s3 === t3.t)
    }
  }

  test("#759 - slice assignment broken for transpose matrices") {
    def okay(x: DenseMatrix[Double]) = {
      val res = DenseMatrix.zeros[Double](5, 2)
      res(::, 1 until 2) := x
      res
    }
    def problem[T: ClassTag: Zero](x: DenseMatrix[T]) ={
      val res = DenseMatrix.zeros[T](5, 2)
      res(::, 1 until 2) := x
      res
    }

    val x = DenseMatrix.ones[Double](1, 5).t
    assert(okay(x) == problem(x))
  }

  test("Min/Max") {
    val m = DenseMatrix((1, 0, 0), (2, 3, -1))
    assert(argmin(m) === (1, 2))
    assert(argmax(m) === (1, 1))
    assert(min(m) === -1)
    assert(max(m) === 3)
    assert(minMax(m) === (-1, 3))
    assert(ptp(m) === 4)
  }

  test("elementwise max") {
    val v = DenseVector(2, 0, 3, 2, -1).asDenseMatrix
    val v2 = DenseVector(3, -1, 3, 4, -4).asDenseMatrix

    assert(max(v, v2) === DenseVector(3, 0, 3, 4, -1).asDenseMatrix)
    assert(max(v, 2) === DenseVector(2, 2, 3, 2, 2).asDenseMatrix)

    assert(min(v, 2) === DenseVector(2, 0, 2, 2, -1).asDenseMatrix)
  }

  test("Min/Max[Float]") {
    val m = convert(DenseMatrix((1, 0, 0), (2, 3, -1)), Float)
    assert(argmin(m) === (1, 2))
    assert(argmax(m) === (1, 1))
    assert(min(m) === -1)
    assert(max(m) === 3)
    assert(minMax(m) === (-1.0f, 3.0f))
    assert(ptp(m) === 4)
  }

  test("Min/Max[Double]") {
    val m = convert(DenseMatrix((1, 0, 0), (2, 3, -1)), Double)
    assert(argmin(m) === (1, 2))
    assert(argmax(m) === (1, 1))
    assert(min(m) === -1)
    assert(max(m) === 3)
    assert(minMax(m) === (-1.0, 3.0))
    assert(ptp(m) === 4)
  }

  test("Min/Max[Long]") {
    val m = convert(DenseMatrix((1, 0, 0), (2, 3, -1)), Long)
    assert(argmin(m) === (1, 2))
    assert(argmax(m) === (1, 1))
    assert(min(m) === -1)
    assert(max(m) === 3)
    assert(minMax(m) === (-1L, 3L))
    assert(ptp(m) === 4)
  }

  test("MapValues") {
    val a: DenseMatrix[Int] = DenseMatrix((1, 0, 0), (2, 3, -1))

    val b1: DenseMatrix[Int] = a.mapValues(_ + 1)
    assert(b1 === DenseMatrix((2, 1, 1), (3, 4, 0)))

    val b2: DenseMatrix[Double] = a.mapValues(_ + 1.0)
    assert(b2 === DenseMatrix((2.0, 1.0, 1.0), (3.0, 4.0, 0.0)))

    val b3 = a.t.mapValues(_ + 1)
    assert(b3 === DenseMatrix((2, 3), (1, 4), (1, 0)))
  }

  /*
  test("Map Triples") {
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
    assert(s === sum(a))

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
      val a = DenseMatrix.zeros[Int](2, 2)
      val b = DenseMatrix((1, 0), (2, 3))
      a := b
      assert(a === b)

    }
    val a = DenseMatrix.zeros[Int](2, 3)
    val b = DenseMatrix((1, 0, 5), (2, 3, -1))
    a := b
    assert(a === b)
  }

  test("horzcat") {
    val a: DenseMatrix[Int] = DenseMatrix((1, 0, 5), (2, 3, -1))
    val result: DenseMatrix[Int] = DenseMatrix((1, 0, 5, 1, 0, 5), (2, 3, -1, 2, 3, -1))
    assert(DenseMatrix.horzcat(a, a) === result)
  }

  test("vertcat") {
    val a: DenseMatrix[Int] = DenseMatrix((1, 0, 5), (2, 3, -1))
    val result: DenseMatrix[Int] = DenseMatrix((1, 0, 5), (2, 3, -1), (1, 0, 5), (2, 3, -1))
    assert(DenseMatrix.vertcat(a, a) === result)
  }

  test("Multiply") {
    val a = DenseMatrix((1.0, 2.0, 3.0), (4.0, 5.0, 6.0))
    val b = DenseMatrix((7.0, -2.0, 8.0), (-3.0, -3.0, 1.0), (12.0, 0.0, 5.0))
    val c = DenseVector(6.0, 2.0, 3.0)
    val cs = SparseVector(6.0, 2.0, 3.0)
    assert(a * b === DenseMatrix((37.0, -8.0, 25.0), (85.0, -23.0, 67.0)))
    assert(a * c === DenseVector(19.0, 52.0))
    assert(b * c === DenseVector(62.0, -21.0, 87.0))
    assert(a * cs === DenseVector(19.0, 52.0))
    assert(b * cs === DenseVector(62.0, -21.0, 87.0))
    assert(b.t * c === DenseVector(72.0, -18.0, 65.0))

    assert(a.t * DenseVector(4.0, 3.0) === DenseVector(16.0, 23.0, 30.0))
    assert(c.t * a.t === (a * c).t)

    // should be dense
    val x: DenseMatrix[Double] = a * a.t
    assert(x === DenseMatrix((14.0, 32.0), (32.0, 77.0)))

    // should be dense
    val y: DenseMatrix[Double] = a.t * a
    assert(y === DenseMatrix((17.0, 22.0, 27.0), (22.0, 29.0, 36.0), (27.0, 36.0, 45.0)))

    val z: DenseMatrix[Double] = b * (b + 1.0)
    assert(z === DenseMatrix((164.0, 5.0, 107.0), (-5.0, 10.0, -27.0), (161.0, -7.0, 138.0)))
  }

  test("Multiply Int") {
    val a = DenseMatrix((1, 2, 3), (4, 5, 6))
    val b = DenseMatrix((7, -2, 8), (-3, -3, 1), (12, 0, 5))
    val c = DenseVector(6, 2, 3)
    assert(a * b === DenseMatrix((37, -8, 25), (85, -23, 67)))
    assert(a * c === DenseVector(19, 52))
    assert(b * c === DenseVector(62, -21, 87))
    assert(b.t * c === DenseVector(72, -18, 65))
    assert(a.t * DenseVector(4, 3) === DenseVector(16, 23, 30))

    // should be dense
    val x = a * a.t
    assert(x === DenseMatrix((14, 32), (32, 77)))

    // should be dense
    val y = a.t * a
    assert(y === DenseMatrix((17, 22, 27), (22, 29, 36), (27, 36, 45)))

    val z: DenseMatrix[Int] = b * ((b + 1): DenseMatrix[Int])
    assert(z === DenseMatrix((164, 5, 107), (-5, 10, -27), (161, -7, 138)))
  }

  test("Multiply Boolean") {
    val a = DenseMatrix((true, true, true), (true, true, true))
    val b = DenseMatrix((true, false, true), (true, false, true), (true, false, true))
    assert(a * b === DenseMatrix((true, false, true), (true, false, true)))
  }

  test("Multiply Float") {
    val a = DenseMatrix((1.0f, 2.0f, 3.0f), (4.0f, 5.0f, 6.0f))
    val b = DenseMatrix((7.0f, -2.0f, 8.0f), (-3.0f, -3.0f, 1.0f), (12.0f, 0.0f, 5.0f))
    val c = DenseVector(6.0f, 2.0f, 3.0f)
    val cs = SparseVector(6.0f, 2.0f, 3.0f)
    assert(a * b === DenseMatrix((37.0f, -8.0f, 25.0f), (85.0f, -23.0f, 67.0f)))
    assert(a * c === DenseVector(19.0f, 52.0f))
    assert(b * c === DenseVector(62.0f, -21.0f, 87.0f))
    assert(a * cs === DenseVector(19.0f, 52.0f))
    assert(b * cs === DenseVector(62.0f, -21.0f, 87.0f))
    assert(b.t * c === DenseVector(72.0f, -18.0f, 65.0f))
    assert(a.t * DenseVector(4.0f, 3.0f) === DenseVector(16.0f, 23.0f, 30.0f))

    // should be dense
    val x = a * a.t
    assert(x === DenseMatrix((14.0f, 32.0f), (32.0f, 77.0f)))

    // should be dense
    val y = a.t * a
    assert(y === DenseMatrix((17.0f, 22.0f, 27.0f), (22.0f, 29.0f, 36.0f), (27.0f, 36.0f, 45.0f)))

    val z: DenseMatrix[Float] = b * (b + 1.0f)
    assert(z === DenseMatrix((164.0f, 5.0f, 107.0f), (-5.0f, 10.0f, -27.0f), (161.0f, -7.0f, 138.0f)))
  }

  test("Multiply Complex") {

    val a = DenseMatrix((Complex(1, 1), Complex(2, 2), Complex(3, 3)), (Complex(4, 4), Complex(5, 5), Complex(6, 6)))
    val b = DenseMatrix(
      (Complex(7, 7), Complex(-2, -2), Complex(8, 8)),
      (Complex(-3, -3), Complex(-3, -3), Complex(1, 1)),
      (Complex(12, 12), Complex(0, 0), Complex(5, 5)))
    val c = DenseVector(Complex(6, 0), Complex(2, 0), Complex(3, 0))
    val cs = SparseVector(Complex(6, 0), Complex(2, 0), Complex(3, 0))
    val value: DenseMatrix[Complex] = a * b
    assert(
      value === DenseMatrix(
        (Complex(0, 74), Complex(0, -16), Complex(0, 50)),
        (Complex(0, 170), Complex(0, -46), Complex(0, 134))))
    assert(b * c === DenseVector(Complex(62, 62), Complex(-21, -21), Complex(87, 87)))
    assert(b * cs === DenseVector(Complex(62, 62), Complex(-21, -21), Complex(87, 87)))
    assert(b.t * c === DenseVector(Complex(72, -72), Complex(-18, 18), Complex(65, -65)))
  }

  test("Multiply BigDecimal") {
    val a = DenseMatrix((1, 2, 3), (4, 5, 6)).mapValues(BigDecimal(_))
    val b = DenseMatrix((7, -2, 8), (-3, -3, 1), (12, 0, 5)).mapValues(BigDecimal(_))
    val c = DenseVector(6, 2, 3).mapValues(BigDecimal(_))
    assert(
      a.*(b) === DenseMatrix((37, -8, 25), (85, -23, 67))
        .mapValues(BigDecimal(_)))
    assert(a * c === DenseVector(19, 52).mapValues(BigDecimal(_)))
    assert(b * c === DenseVector(62, -21, 87).mapValues(BigDecimal(_)))
    assert(b.t * c === DenseVector(72, -18, 65).mapValues(BigDecimal(_)))
    assert(a.t * DenseVector(4, 3).mapValues(BigDecimal(_)) === DenseVector(16, 23, 30).mapValues(BigDecimal(_)))

    // should be dense
    val x = a * a.t
    assert(x === DenseMatrix((14, 32), (32, 77)).mapValues(BigDecimal(_)))

    // should be dense
    val y = a.t * a
    assert(y === DenseMatrix((17, 22, 27), (22, 29, 36), (27, 36, 45)).mapValues(BigDecimal(_)))

    val z: DenseMatrix[BigDecimal] = b * ((b + BigDecimal(1)): DenseMatrix[BigDecimal])
    assert(z === DenseMatrix((164, 5, 107), (-5, 10, -27), (161, -7, 138)).mapValues(BigDecimal(_)))
  }

  test("toDenseVector") {
    val a = DenseMatrix((1, 2, 3), (4, 5, 6))
    val b = a(0 to 1, 1 to 2)
    val c = b.t
    assert(a.toDenseVector === DenseVector(1, 4, 2, 5, 3, 6))
    assert(b.toDenseVector === DenseVector(2, 5, 3, 6))
    assert(c.toDenseVector === DenseVector(2, 3, 5, 6))
  }

  test("flattenView") {
    val a = DenseMatrix((1, 2, 3), (4, 5, 6))
    a.flatten(true)(2) = 4
    assert(a === DenseMatrix((1, 4, 3), (4, 5, 6)))
  }

  test("Trace") {
    assert(trace(DenseMatrix((1, 2), (4, 5))) === 1 + 5)
    assert(trace(DenseMatrix((1, 2, 3), (3, 4, 5), (5, 6, 7))) == 1 + 4 + 7)
    assert(trace(DenseMatrix((1, 2, 3), (4, 5, 6), (7, 8, 9))) === 1 + 5 + 9)
  }

  test("Reshape") {
    val m: DenseMatrix[Int] = DenseMatrix((1, 2, 3), (4, 5, 6))
    val r: DenseMatrix[Int] = m.reshape(3, 2, true)
    assert(m.data eq r.data)
    assert(r.rows === 3)
    assert(r.cols === 2)
    assert(r === DenseMatrix((1, 5), (4, 3), (2, 6)))
  }

  test("Solve") {
    // square solve
    val r1: DenseMatrix[Double] = DenseMatrix((1.0, 3.0), (2.0, 0.0)) \ DenseMatrix((1.0, 2.0), (3.0, 4.0))
    assert(r1 === DenseMatrix((1.5, 2.0), (-1.0 / 6, 0.0)))

    // matrix-vector solve
    val r2: DenseVector[Double] = DenseMatrix((1.0, 3.0, 4.0), (2.0, 0.0, 6.0)) \ DenseVector(1.0, 3.0)
    assert(norm(r2 - DenseVector(0.1813186813186811, -0.3131868131868131, 0.43956043956043944), inf) < 1E-5)

    // wide matrix solve
    val r3: DenseMatrix[Double] = DenseMatrix((1.0, 3.0, 4.0), (2.0, 0.0, 6.0)) \ DenseMatrix((1.0, 2.0), (3.0, 4.0))
    matricesNearlyEqual(
      r3,
      DenseMatrix(
        (0.1813186813186811, 0.2197802197802196),
        (-0.3131868131868131, -0.1978021978021977),
        (0.43956043956043944, 0.5934065934065933)))

    // tall matrix solve
    val r4: DenseMatrix[Double] = DenseMatrix((1.0, 3.0), (2.0, 0.0), (4.0, 6.0)) \ DenseMatrix(
      (1.0, 4.0),
      (2.0, 5.0),
      (3.0, 6.0))
    assert(max(abs(
      r4 - DenseMatrix((0.9166666666666667, 1.9166666666666672), (-0.08333333333333352, -0.08333333333333436)))) < 1E-5)
  }

  test("Solve Float") {
    // square solve
    val r1: DenseMatrix[Float] = DenseMatrix((1.0f, 3.0f), (2.0f, 0.0f)) \ DenseMatrix((1.0f, 2.0f), (3.0f, 4.0f))
    assert(r1 === DenseMatrix((1.5f, 2.0f), (-1.0f / 6, 0.0f)))

    // matrix-vector solve
    val r2: DenseVector[Float] = DenseMatrix((1.0f, 3.0f, 4.0f), (2.0f, 0.0f, 6.0f)) \ DenseVector(1.0f, 3.0f)
    assert(norm(r2 - DenseVector(0.1813186813186811f, -0.3131868131868131f, 0.43956043956043944f)) < 1E-5)

    // wide matrix solve
    val r3: DenseMatrix[Float] = DenseMatrix((1.0f, 3.0f, 4.0f), (2.0f, 0.0f, 6.0f)) \ DenseMatrix(
      (1.0f, 2.0f),
      (3.0f, 4.0f))
    assert(
      max(
        abs(
          r3 - DenseMatrix(
            (0.1813186813186811f, 0.2197802197802196f),
            (-0.3131868131868131f, -0.1978021978021977f),
            (0.43956043956043944f, 0.5934065934065933f)))) < 1E-5)

    // tall matrix solve
    val r4: DenseMatrix[Float] = DenseMatrix((1.0f, 3.0f), (2.0f, 0.0f), (4.0f, 6.0f)) \ DenseMatrix(
      (1.0f, 4.0f),
      (2.0f, 5.0f),
      (3.0f, 6.0f))
    assert(
      max(
        abs(r4 - DenseMatrix(
          (0.9166666666666667f, 1.9166666666666672f),
          (-0.08333333333333352f, -0.08333333333333436f)))) < 1E-5)
  }

  test("GH#29 transpose solve is broken") {
    val A = DenseMatrix((1.0, 0.0), (1.0, -1.0))
    val t = DenseVector(1.0, 0.0)

    assert(A \ t === DenseVector(1.0, 1.0))
    assert(A.t \ t === DenseVector(1.0, 0.0))
  }

  test("sum") {
    // Test square and rectangular matrices
    val A = DenseMatrix((1.0, 3.0), (2.0, 4.0))
    assert(sum(A, Axis._0) === DenseVector(3.0, 7.0).t)
    assert(sum(A(::, *)) === DenseVector(3.0, 7.0).t)
    assert(sum(DenseMatrix((1.0, 3.0, 5.0), (2.0, 4.0, 6.0)), Axis._0) === DenseVector(3.0, 7.0, 11.0).t)
    assert(sum(DenseMatrix((1.0, 3.0), (2.0, 4.0), (5.0, 6.0)), Axis._0) === DenseVector(8.0, 13.0).t)

    assert(sum(A, Axis._1) === DenseVector(4.0, 6.0))
    assert(sum(DenseMatrix((1.0, 3.0, 5.0), (2.0, 4.0, 6.0)), Axis._1) === DenseVector(9.0, 12.0))
    assert(sum(DenseMatrix((1.0, 3.0), (2.0, 4.0), (5.0, 6.0)), Axis._1) === DenseVector(4.0, 6.0, 11.0))
    assert(sum(A) === 10.0)
  }

  test("normalize rows and columns") {
    val A = DenseMatrix((1.0, 3.0), (2.0, 4.0))
    assert(normalize(A, Axis._0, 1) === DenseMatrix((1.0 / 3.0, 3.0 / 7.0), (2.0 / 3.0, 4.0 / 7.0)))
    assert(normalize(A, Axis._1, 1) === DenseMatrix((1.0 / 4.0, 3.0 / 4.0), (2.0 / 6.0, 4.0 / 6.0)))
    // handle odd sized matrices (test for a bug.)
    val dm = DenseMatrix.tabulate(2, 5)((i, j) => i * j * 1.0 + 1)
    dm := normalize(dm, Axis._1, 2)
    assert(abs(sum(dm(0, ::).t.map(x => x * x)) - 1.0) < 1E-4, dm.toString + " not normalized!")
  }

  test("Generic Dense ops") {
    // mostly for coverage
    val a = DenseMatrix.create[String](1, 1, Array("SSS"))
    intercept[IndexOutOfBoundsException] {
      a(3, 3) = ":("
      assert(false, "Shouldn't be here!")
    }
    assert(a(0, 0) === "SSS")
    intercept[IndexOutOfBoundsException] {
      a(3, 3)
      assert(false, "Shouldn't be here!")
    }

    a(0, 0) = ":("
    assert(a(0, 0) === ":(")

    a := ":)"
    assert(a(0, 0) === ":)")
    val b = DenseMatrix.zeros[String](1, 1)
    b := a
    assert(b === a)

  }

  test("toString with no rows doesn't throw") {
    DenseMatrix.zeros[Double](0, 2).toString
  }

  test("GH #30: Shaped solve of transposed and slice matrix does not work") {
    val A = DenseMatrix((1.0, 0.0), (1.0, -1.0))
    val i = DenseMatrix.eye[Double](2)
    val res = i \ A.t(::, 1)
    assert(res === DenseVector(1.0, -1.0))
    val res2 = i \ A(1, ::).t
    assert(res2 === DenseVector(1.0, -1.0))
  }

  test("GH #148: out of bounds slice throws") {
    val temp2 = DenseMatrix.tabulate(5, 5)((x: Int, y: Int) => x + y * 10)
    intercept[IndexOutOfBoundsException] {
      temp2(Range(4, 6), 3)
    }
  }

  test("softmax on dm slices") {
    val a = DenseMatrix((1.0, 2.0, 3.0))
    assert(softmax(a(::, 1)) === 2.0)

  }

  test("Delete") {
    val a = DenseMatrix((1, 2, 3), (4, 5, 6), (7, 8, 9))
    assert(a.delete(0, Axis._0) === DenseMatrix((4, 5, 6), (7, 8, 9)))
    assert(a.delete(1, Axis._0) === DenseMatrix((1, 2, 3), (7, 8, 9)))
    assert(a.delete(2, Axis._0) === DenseMatrix((1, 2, 3), (4, 5, 6)))

    assert(a.delete(0, Axis._1) === DenseMatrix((2, 3), (5, 6), (8, 9)))
    assert(a.delete(1, Axis._1) === DenseMatrix((1, 3), (4, 6), (7, 9)))
    assert(a.delete(2, Axis._1) === DenseMatrix((1, 2), (4, 5), (7, 8)))

    assert(a.delete(Seq(0, 2), Axis._1) === DenseMatrix(2, 5, 8))
    assert(a.delete(Seq(1, 2), Axis._1) === DenseMatrix(1, 4, 7))

    assert(a.delete(Seq(0, 2), Axis._0) === DenseMatrix((4, 5, 6)))
    assert(a.delete(Seq(1, 2), Axis._0) === DenseMatrix((1, 2, 3)))
  }

  test("Big Int zeros are the right thing") {
    val dm = DenseMatrix.zeros[BigInt](1, 1)
    assert(dm(0, 0) === BigInt(0))
  }

  test("BigInt multiply") {
    val m = DenseMatrix((BigInt(1), BigInt(1)), (BigInt(1), BigInt(0)))
    val m2 = DenseMatrix((1, 1), (1, 0))
    assert(m * m === convert(m2 * m2, Int))
  }

  test("comparisons") {
    val one = DenseMatrix.ones[Double](5, 6)
    val zero = DenseMatrix.zeros[Double](5, 6)
    assert((one >:> zero) === DenseMatrix.ones[Boolean](5, 6))
  }

//  test("Some ill-typedness") {
//    import shapeless.test.illTyped
//    illTyped {
//      """
//        val one = DenseMatrix.ones[Double](5, 6)
//        val z = DenseVector.zeros[Double](5)
//        (z + one)
//      """
//    }
//  }

  test("ensure we don't crash on weird strides") {
    val dm = DenseMatrix.zeros[Double](3, 3)

    assert((dm(::, 0 until 0) * dm(0 until 0, ::)) === dm)
    assert((dm(0 until 0, ::) * dm(::, 0 until 0)) === DenseMatrix.zeros[Double](0, 0))
//    assert( (dm(::, 2 until 0 by -1) * dm(2 until 0 by -1, ::)) === dm)
  }

  test("Ensure a += a.t gives the right result") {
    val dm = DenseMatrix.rand[Double](3, 3)
    val dmdmt = dm + dm.t
    dm += dm.t
    assert(dm === dmdmt)
  }

  test("#221") {
    val data = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2,
      3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val mat = new DenseMatrix(rows = 10, data, offset = 0).t
    val area = mat(3 until 6, 2 until 7)
    assert(area === DenseMatrix((3, 4, 5, 6, 7), (3, 4, 5, 6, 7), (3, 4, 5, 6, 7)))

    assert(area.t === DenseMatrix((3, 4, 5, 6, 7), (3, 4, 5, 6, 7), (3, 4, 5, 6, 7)).t)

    val sl2t = area.t(0 until area.cols, 1 until area.rows)
    assert(
      sl2t.offset === area.offset + area.majorStride,
      sl2t.data(area.offset + area.majorStride) + " " + area.offset)
    assert(sl2t.t === DenseMatrix((3, 4, 5, 6, 7), (3, 4, 5, 6, 7)))

    val sl2 = area(1 until area.rows, 0 until area.cols)
    assert(sl2 === DenseMatrix((3, 4, 5, 6, 7), (3, 4, 5, 6, 7)))
  }

  test("DenseMatrix construction with list of lists") {
    val dm = DenseMatrix(
      List(List(1, 2, 3, 0, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 2, 3, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 1, 2, 3)): _*)
  }

  test("#265: slices of :: and IndexedSeq") {
    val dm = DenseMatrix((0, 1, 2), (3, 4, 5))
    assert(dm(::, IndexedSeq(2, 1, 0)).toDenseMatrix === fliplr(dm))
    assert(dm(IndexedSeq(1, 0), ::).toDenseMatrix === flipud(dm))
  }

  test("#278: don't crash on solve when majorStride == 0") {
    val d = DenseVector[Double]()
    val m = DenseMatrix.tabulate(0, 0) { case x => 0.0 }
    assert(m \ d === d)

  }

  test("#283: slice of dm by dm boolean") {
    val dm = DenseMatrix((0, 1, 2), (3, 4, 5))
    dm(dm >:= 2) := 3
    assert(dm === DenseMatrix((0, 1, 3), (3, 3, 3)))
  }

  test("#286: argsort diverging implicit") {
    val dm = DenseMatrix((0.1f), (0.0f))

    assert(argsort(dm) === IndexedSeq((1, 0), (0, 0)))
  }

  test("#289: sigmoid dm slice") {
    val m = DenseMatrix.zeros[Double](10, 10)
    assert(sigmoid(m(::, 0 to 5)) === DenseMatrix.fill(10, 6)(0.5))
    assert(sigmoid(m(::, 3 to 5)) === DenseMatrix.fill(10, 3)(0.5))
  }

  test("#336 argmax for Dense Matrices") {
    val m = DenseMatrix.zeros[Double](3, 3)
    m(2, ::) := DenseVector(1.0, 2.0, 3.0).t
    assert(argmax(m(2, ::).t) === 2)
    assert(max(m(2, ::).t) === 3.0)
  }

  test("lhs scalars") {
    assert(1.0 /:/ (DenseMatrix.fill(2, 2)(10.0)) === DenseMatrix.fill(2, 2)(1 / 10.0))
    assert(1.0 -:- (DenseMatrix.fill(2, 2)(10.0)) === DenseMatrix.fill(2, 2)(-9.0))
  }

  test("mapping ufunc") {
    val r = DenseMatrix.rand(100, 100)
    val explicit = new DenseMatrix(100, 100, r.data.map(math.sin))
    assert(sin(r) == explicit)
    sin.inPlace(r)
    assert(explicit == r)
  }

  test("mapping ufunc, strides") {
    val r = (DenseMatrix.rand(100, 100)).apply(10 until 27, 4 until 37 by 4)
    var explicit = new DenseMatrix(100, 100, r.data.map(math.sin))
    explicit = explicit(10 until 27, 4 until 37 by 4)
    assert(sin(r) == explicit)
    sin.inPlace(r)
    assert(explicit == r)
  }

  test("#449") {

    val m = DenseMatrix.rand(10, 10)

    m(List(1, 2, 3), 0 to 0) := 5d //WORKS FINE
    m(List(1, 2, 3), 0) := 5d //NOT WORKING
    m(1 to 3, 0) := 5d //WORKING

    m(List(1, 2, 3), 0 to 0) := m(List(1, 2, 3), 0 to 0) //WORKS FINE
    m(List(1, 2, 3), 0) := m(List(1, 2, 3), 0) //NOT WORKING
    m(1 to 3, 0) := m(1 to 3, 0) //WORKS FINE

  }

  test("#476: DM * DV when rows == 0") {
    val m = DenseMatrix.zeros[Double](0, 10)
    val v = DenseVector.zeros[Double](10)
    assert(m * v == DenseVector.zeros[Double](0))
    val m2 = DenseMatrix.zeros[Double](10, 0)
    val v2 = DenseVector.zeros[Double](0)
    assert(m2 * v2 == DenseVector.zeros[Double](10))
  }

  test("#534: DenseMatrix construction from empty row sequence") {
    val rows = Seq.empty[Seq[Double]]
    val matrix = DenseMatrix(rows: _*)
    assert(matrix.rows == 0)
    assert(matrix.cols == 0)
  }

  test("#577: Empty DenseMatrix can be transposed") {
    val m = new DenseMatrix(0, 0, Array.empty[Double])
    val mt = m.t
    assert(mt.rows == 0)
    assert(mt.cols == 0)
    assert(m === mt)
  }

  test("#592: can take an empty column or row slice") {
    val m = DenseMatrix.fill(5, 5)(0)
    m(4 until 4, 0 until 5)
    m(0 until 5, 4 until 4)
  }

  test("#559: reshape of transpose matrix") {
    val a = DenseMatrix((1, 4, 7), (2, 5, 8), (3, 6, 9)) //Matrix A
    val b = DenseMatrix((1, 2, 3), (4, 5, 6), (7, 8, 9)) // Matrix B
    assert(a != b)
    assert(a == b.t)
    assert(a.reshape(9, 1) == b.t.reshape(9, 1))
  }

  test("#620 solving transposed matrices, tall.t") {
    val W: DenseMatrix[Double] = DenseMatrix((1.0, 3.0), (2.0, 0.0), (4.0, 6.0)).t
    val y = DenseVector(1.0, 2.0)

    val target = DenseVector(0.166667, -0.083333, 0.250000)

    solveCompare(W, y, target)
  }

  test("#620 solving transposed matrices, wide.t") {
    val W: DenseMatrix[Double] = DenseMatrix((1.0, 2.0, 4.0), (3.0, 0.0, 6.0)).t
    val y = DenseVector(1.0, 2.0, 3.0)

    val target = DenseVector(0.916667, -0.083333)

    solveCompare(W, y, target)
  }

  private def solveCompare(W: DenseMatrix[Double], y: DenseVector[Double], target: DenseVector[Double]) = {
    val WcopyY = W.copy \ y.copy
    val Wy = W \ y.copy

    assert(norm(Wy - WcopyY) < 1e-3)
    assert(norm(Wy - target) < 1e-3)
  }

  test("#682 bug in slice outer product") {
    val c = DenseVector(3, 1)
    val a = DenseMatrix((3, 1), (-1, -2))

    val rr = c * c.t
    val yy = a(0, ::).t * a(0, ::)
    assert(rr == yy)
  }

  // TODO: we should profile just copying to Double if we have BLAS (and even if we don't...)
  // this is hilariously slow somehow.
  test("large matrix multiply, int") {
    val rI = DenseMatrix.rand[Int](2002, 2002, Rand.randInt(-3, 3))
    val rD = convert(rI, Double)
    assert((rI * rI).mapValues(_.toDouble) === (rD * rD))
  }

  test("#772 - weird copy bug") {
    val rows = 3
    val cols = 4
    val data = Array[Double](1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    val dm = new DenseMatrix(rows, cols, data, 0, cols, isTranspose = true)
    val sm = dm(2 until 3, 0 until 2)
    assert(sm == sm.copy)
  }

}

trait MatrixTestUtils {
  def matricesNearlyEqual(A: Matrix[Double], B: Matrix[Double], threshold: Double = 1E-6): Unit = {
    for (i <- 0 until A.rows; j <- 0 until A.cols)
      A(i, j) should be(B(i, j) +- threshold)
  }

}
