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

import breeze.linalg.eig.Eig
import breeze.linalg.eigSym.EigSym
import breeze.linalg.qr.QR
import breeze.linalg.qrp.QRP
import breeze.linalg.svd.SVD
import org.scalacheck.{Arbitrary,Gen,Prop}
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith
import breeze.util.DoubleImplicits
import breeze.numerics._
import breeze.stats.{mean, median, meanAndVariance}
import breeze.{math => bmath}

/**
 *
 * @author dlwh
 */

@RunWith(classOf[JUnitRunner])
class LinearAlgebraTest extends FunSuite with Checkers with Matchers with DoubleImplicits {
  test("kron") {
    val a = DenseMatrix((1,2),(3,4))
    val b = DenseMatrix((0,5),(6,7))
    assert(kron(a,b) === DenseMatrix((0,5,0,10),(6,7,12,14),(0,15,0,20),(18,21,24,28)))
  }

  test("ranks") {
    assert(ranks(DenseVector(1,2,3)).toList  === List(1.0,2.0,3.0))
    assert(ranks(DenseVector(3,-1,2)).toList === List(3.0,1.0,2.0))
    assert(ranks(DenseVector(1,2,3,3)).toList === List(1.0,2.0,3.5,3.5))
    assert(ranks(DenseVector(1,2,3,3,3)).toList === List(1.0,2.0,4.0,4.0,4.0))
  }

  test("cholesky") {
    val A = DenseMatrix((1.0,0.0,0.0),(2.0,3.0,0.0),(4.0,5.0,6.0))
    val Sigma = A * A.t
    assert(cholesky(Sigma) === A)
  }

  test("eigSym") {
    val A = DenseMatrix((9.0,0.0,0.0),(0.0,82.0,0.0),(0.0,0.0,25.0))
    val EigSym(lambda, evs) = eigSym(A)
    assert(lambda === DenseVector(9.0,25.0,82.0))
    assert(evs === DenseMatrix((1.0,0.0,0.0),(0.0,0.0,1.0),(0.0,1.0,0.0)))
  }

  test("LUfactorization") {
    val (m, _) = LU(DenseMatrix(( 29, 42, -4, 50, 1),
                           ( 20,-31, 32, 21, 2),
                           (-47,-20, 24,-22, 3),
                           (  3, 17,-45, 23, 4)))
    val aux = DenseMatrix((-47.0000, -20.0000, 24.0000, -22.0000, 3.0000),
                     ( -0.4255, -39.5106, 42.2127,  11.6382, 3.2765),
                     ( -0.6170,  -0.7506, 42.4964,  45.1620, 5.3107),
                     ( -0.0638,  -0.3979, -0.6275,  54.5694, 8.8282))
    matricesNearlyEqual(m, aux, 1E-4)
  }

  test("det") {
    val A = DenseMatrix((9,26,21),(48,3,11),(7,48,26))
    det(A) should be (13446.99999999 +- 1e-6)

    val B = DenseMatrix((1,2,3),(4,5,-6),(7,8,9))
    det(B) should be (-72.0 +- 1e-16)

    val C = DenseMatrix((1,2,3),(2,4,6),(0,-1,0)) // 1st and 2nd row linearly dep.
    det(C) should be (0.0 +- 1e-6)

    val D = DenseMatrix((-1,1,-1),(1,2,3),(3,-10,1))
    det(D) should be (-8.0 +- 1E-6)
  }

  test("logdet") {
    val A = DenseMatrix((9,26,21),(48,3,11),(7,48,26))
    val (signA, detA) = logdet(A)
    detA should be (math.log(13446.99999999) +- 1e-8)
    signA should be (1.0 +- 1e-8)

    val B = DenseMatrix((1,2,3),(4,5,-6),(7,8,9))
    val (signB, detB) = logdet(B)
    detB should be (math.log(72.0) +- 1e-15)
    assert(signB === -1.0)

    val C = DenseMatrix((1,2,3),(2,4,6),(0,-1,0)) // 1st and 2nd row linearly dep.
    val (signC, detC) = logdet(C)
    detC should be (Double.NegativeInfinity +- 1e-15)
    assert(signC === 0.0)

    val D = DenseMatrix((-1,1,-1),(1,2,3),(3,-10,1))
    val (signD, detD) = logdet(D)
    detD should be (math.log(8) +- 1e-8)
    assert(signD === -1.0)
  }

  test("cond") {
    val A = DenseMatrix( (1.0, 0.0, -1.0), (0.0, 1.0, 0.0), (1.0, 0.0, 1.0))
    assert((cond(A) - math.sqrt(2)).abs < 1E-6, cond(A))

    A(0, 0) = -1.0 // row 0 and row 2 are linearly dependent now
    assert(cond(A) === Double.PositiveInfinity)

  }

  test("inv") {
    val X = DenseMatrix(( 29.0, 42.0, -4.0, 50.0),
                   ( 20.0,-31.0, 32.0, 21.0),
                   (-47.0,-20.0, 24.0,-22.0),
                   (  3.0, 17.0,-45.0, 23.0))
    val I = DenseMatrix.eye[Double](4)
    matricesNearlyEqual(inv(X) * X, I)
  }

  test("pinv") {
    val X = DenseMatrix((54.0, 95.0), (23.0, 25.0), (70.0, 41.0), (31.0, 19.0))
    val I = DenseMatrix.eye[Double](2)
    matricesNearlyEqual(pinv(X) * X, I)
  }

  test("pinv conditioning: #304") {
    val m = DenseMatrix((0d,3d,6d), (0d,4d,7d), (0d,5d,9d))
    val mi = pinv(m)
    val eye: DenseMatrix[Double] = DenseMatrix.eye[Double](3)
    eye(0, 0) = 0.0
    matricesNearlyEqual(mi * m, eye)
  }

  test("cross") {
    // specific example; with prime elements
    val (v1, v2, r) = (DenseVector(13, 3, 7), DenseVector(5, 11, 17), DenseVector(-26, -186, 128))
    assert(cross(v1, v2) === r)
    assert(cross(v2, v1) === r * -1)

    // test using a re-write of the cross-product equation and a scalacheck arbitrary generator
    implicit def arb3DVector: Arbitrary[DenseVector[Double]] = Arbitrary {
      for {
	els <- Gen.containerOfN[Array, Double](3, Gen.chooseNum[Double](-100.0, 100.0))
      } yield DenseVector(els(0), els(1), els(2))
    }
    check {(a: DenseVector[Double], b: DenseVector[Double]) =>
      val r = DenseVector(
        a(1) * b(2) - a(2) * b(1),
        a(2) * b(0) - a(0) * b(2),
        a(0) * b(1) - a(1) * b(0))
      cross(a, b) == r
      cross(b, a) == r * -1.0
    }

    // test the failure that should occur if a or b does not have 3 components
    val v4comp = DenseVector(1,2,3,4)
    intercept[IllegalArgumentException] {
      cross(v4comp, v4comp)
    }
  }

  test("rank") {
    val r1 = DenseMatrix((1.0,2.0,3.0), (1.0,2.0,3.0), (1.0,2.0,3.0))  // rank 1 matrix
    val r2 = DenseMatrix((1.0,2.0,3.0), (4.0,5.0,6.0), (7.0,8.0,9.0))  // rank 2 matrix
    val r3 = DenseMatrix((1.0,2.0,3.0), (4.0,5.0,6.0), (6.0,8.0,9.0))  // rank 3 matrix
    assert(rank(r1) === 1)
    assert(rank(r2) === 2)
    assert(rank(r3) === 3)
  }

  test("qr") {
    val A = DenseMatrix((1.0, 1.0, 1.0), (4.0, 2.0, 1.0), (16.0, 4.0, 1.0))
    val QR(_Q, _R) = qr(A)

    assert( trace(_Q.t * _Q).closeTo(_Q.rows) )
    for(i <- 0 until _R.rows; j <- 0 until i) {
      assert(_R(i,j) === 0.0)
    }

    val reA: DenseMatrix[Double] = _Q * _R
    matricesNearlyEqual(reA, A)

  }

  test("qr A[m, n], m < n") {
    val A = DenseMatrix((1.0, 1.0, 1.0, 1.0), (4.0, 2.0, 1.0, 1.0), (16.0, 4.0, 1.0, 1.0))
    val QR(_Q, _R) = qr(A)

    assert((_Q.rows, _Q.cols) == (A.rows, A.rows))
    assert((_R.rows, _R.cols) == (A.rows, A.cols))

    assert( trace(_Q.t * _Q).closeTo(min(A.rows, A.cols)) )
    for(i <- 0 until _R.rows; j <- 0 until i) {
      assert(_R(i,j) === 0.0)
    }

    val reA: DenseMatrix[Double] = _Q * _R
    matricesNearlyEqual(reA, A)
  }

  test("qr A[m, n], m > n") {
    val A = DenseMatrix((1.0, 1.0, 1.0), (4.0, 2.0, 1.0), (16.0, 4.0, 1.0), (32.0, 8.0, 1.0))
    val QR(_Q, _R) = qr(A)

    assert((_Q.rows, _Q.cols) == (A.rows, A.rows))
    assert((_R.rows, _R.cols) == (A.rows, A.cols))

    assert( trace(_Q.t * _Q).closeTo(max(A.rows, A.cols)) )
    for(i <- 0 until _R.rows; j <- 0 until i) {
      assert(_R(i,j) === 0.0)
    }

    val reA: DenseMatrix[Double] = _Q * _R
    matricesNearlyEqual(reA, A)
  }


  test("qr just[QR]") {
    val A = DenseMatrix((1.0, 1.0, 1.0), (4.0, 2.0, 1.0), (16.0, 4.0, 1.0))
    val QR(_Q, _R) = qr(A)
    val _Q2 = qr.justQ(A)
    assert (_Q2 === _Q)
    assert (_R === qr.justR(A))
  }

  test("qrp") {
    val A = DenseMatrix((1.0, 1.0, 1.0), (4.0, 2.0, 1.0), (16.0, 4.0, 1.0))
    val QRP(_QQ, _RR, _P, _) = qrp(A)
    val ap = A * convert(_P, Double)
    assert(max(abs(_QQ * _RR - ap)) < 1E-8)
  }


  test("qr reduced A[m, n], m < n") {
    val A = DenseMatrix((1.0, 1.0, 1.0, 1.0), (4.0, 2.0, 1.0, 1.0), (16.0, 4.0, 1.0, 1.0))
    val QR(_Q, _R) = qr.reduced(A)

    assert((_Q.rows, _Q.cols) == (A.rows, min(A.rows, A.cols)))
    assert((_R.rows, _R.cols) == (min(A.rows, A.cols), A.cols))

    assert( trace(_Q.t * _Q).closeTo(min(A.rows, A.cols)) )
    for(i <- 0 until _R.rows; j <- 0 until i) {
      assert(_R(i,j) === 0.0)
    }

    val reA: DenseMatrix[Double] = _Q * _R
    matricesNearlyEqual(reA, A)
  }

  test("qr reduced A[m, n], m = n") {
    val A = DenseMatrix((1.0, 1.0, 1.0), (4.0, 2.0, 1.0), (16.0, 4.0, 1.0))
    val QR(_Q, _R) = qr.reduced(A)

    assert((_Q.rows, _Q.cols) == (A.rows, min(A.rows, A.cols)))
    assert((_R.rows, _R.cols) == (min(A.rows, A.cols), A.cols))

    assert( trace(_Q.t * _Q).closeTo(min(A.rows, A.cols)) )
    for(i <- 0 until _R.rows; j <- 0 until i) {
      assert(_R(i,j) === 0.0)
    }

    val reA: DenseMatrix[Double] = _Q * _R
    matricesNearlyEqual(reA, A)
  }

  test("qr reduced A[m, n], m > n") {
    val A = DenseMatrix((1.0, 1.0, 1.0), (4.0, 2.0, 1.0), (16.0, 4.0, 1.0), (32.0, 8.0, 1.0))
    val QR(_Q, _R) = qr.reduced(A)

    assert((_Q.rows, _Q.cols) == (A.rows, min(A.rows, A.cols)))
    assert((_R.rows, _R.cols) == (min(A.rows, A.cols), A.cols))

    assert( trace(_Q.t * _Q).closeTo(min(A.rows, A.cols)) )
    for(i <- 0 until _R.rows; j <- 0 until i) {
      assert(_R(i,j) === 0.0)
    }

    val reA: DenseMatrix[Double] = _Q * _R
    matricesNearlyEqual(reA, A)
  }

  test("qr reduced just[QR]") {
    val A = DenseMatrix((1.0, 1.0, 1.0), (4.0, 2.0, 1.0), (16.0, 4.0, 1.0))
    val QR(_Q, _R) = qr.reduced(A)
    val _Q2 = qr.reduced.justQ(A)
    assert (_Q2 === _Q)
    assert (_R === qr.reduced.justR(A))
  }


  test("simple eig test") {
    val Eig(w, _, v) = eig(diag(DenseVector(1.0, 2.0, 3.0)))
    assert(w === DenseVector(1.0, 2.0, 3.0))
    assert(v === diag(DenseVector(1.0, 1.0, 1.0)))
  }

  test("complex eig test") {
    // complex, get it?
    val Eig(w, wi, v) = eig(DenseMatrix((1.0, -1.0), (1.0, 1.0)))
    assert(w === DenseVector(1.0, 1.0))
    assert(wi === DenseVector(1.0, -1.0))
    assert(max(abs(v - diag(DenseVector(0.7071067811865475, -0.7071067811865475)))) < 1E-7)
    // TODO, we seem to throw out VI... these seems bad...
  }

  test("svd A(m, n), m > n") {
    val m = DenseMatrix((2.0,4.0),(1.0,3.0),(0.0,0.0),(0.0,0.0))
    val SVD(u, s, vt) = svd(m)

    // u and vt are unitary
    trace(u.t * u) should be (u.rows.toDouble +- 1E-5)
    trace(vt * vt.t) should be (vt.rows.toDouble +- 1E-5)

    // s is sorted by size of singular value, and be nonnegative
    for(i <- 1 until s.length) {
      assert(s(i) <= s(i-1), s"s($i) > s(${i-1}): ${s(i)} > ${s(i-1)}")
      assert(s(i) >= 0, s"s($i) < 0: ${s(i)}")
    }


    val ss = DenseMatrix.zeros[Double](m.rows, m.cols)
    diag(ss(0 until s.length, 0 until s.length)) := s
    val reM: DenseMatrix[Double] =  u * ss * vt
    matricesNearlyEqual(reM, m)
  }

  test("svd A(m, n), m < n") {
    val m = DenseMatrix((2.0,4.0),(1.0,3.0),(0.0,0.0),(0.0,0.0)).t
    val SVD(u, s, vt) = svd(m)

    // u and vt are unitary
    trace(u.t * u) should be (u.rows.toDouble +- 1E-5)
    trace(vt * vt.t) should be (vt.rows.toDouble +- 1E-5)

    // s is sorted by size of singular value, and be nonnegative
    for(i <- 1 until s.length) {
      assert(s(i) <= s(i-1), s"s($i) > s(${i-1}): ${s(i)} > ${s(i-1)}")
      assert(s(i) >= 0, s"s($i) < 0: ${s(i)}")
    }


    val ss = DenseMatrix.zeros[Double](m.rows, m.cols)
    diag(ss(0 until s.length, 0 until s.length)) := s
    val reM: DenseMatrix[Double] =  u * ss * vt
    matricesNearlyEqual(reM, m)
  }

  test("svd float A(m, n), m > n") {
    val m: DenseMatrix[Float] = DenseMatrix((2.0f,4.0f),(1.0f,3.0f),(0.0f,0.0f),(0.0f,0.0f))
    val SVD(u, s, vt) = svd(m)

    // u and vt are unitary
    trace(u.t * u) should be (u.rows.toFloat +- 1E-5f)
    trace(vt * vt.t) should be (vt.rows.toFloat +- 1E-5f)

    // s is sorted by size of singular value, and be nonnegative
    for(i <- 1 until s.length) {
      assert(s(i) <= s(i-1), s"s($i) > s(${i-1}): ${s(i)} > ${s(i-1)}")
      assert(s(i) >= 0, s"s($i) < 0: ${s(i)}")
    }


    val ss = DenseMatrix.zeros[Float](m.rows, m.cols)
    diag(ss(0 until s.length, 0 until s.length)) := s
    val reM: DenseMatrix[Float] =  u * ss * vt
    // matricesNearlyEqual(reM, m)
    for(i <- 0 until reM.rows; j <- 0 until reM.cols)
      reM(i,j) should be (m(i, j) +- 1E-6f)
  }

  test("svd float A(m, n), m < n") {
    val m: DenseMatrix[Float] = DenseMatrix((2.0f,4.0f),(1.0f,3.0f),(0.0f,0.0f),(0.0f,0.0f)).t
    val SVD(u, s, vt) = svd(m)

    // u and vt are unitary
    trace(u.t * u) should be (u.rows.toFloat +- 1E-5f)
    trace(vt * vt.t) should be (vt.rows.toFloat +- 1E-5f)

    // s is sorted by size of singular value, and be nonnegative
    for(i <- 1 until s.length) {
      assert(s(i) <= s(i-1), s"s($i) > s(${i-1}): ${s(i)} > ${s(i-1)}")
      assert(s(i) >= 0, s"s($i) < 0: ${s(i)}")
    }


    val ss = DenseMatrix.zeros[Float](m.rows, m.cols)
    diag(ss(0 until s.length, 0 until s.length)) := s
    val reM: DenseMatrix[Float] =  u * ss * vt
    // matricesNearlyEqual(reM, m)
    for(i <- 0 until reM.rows; j <- 0 until reM.cols)
      reM(i,j) should be (m(i, j) +- 1E-5f)
  }

  test("svd reduced A(m, n), m > n") {
    val m = DenseMatrix((2.0,4.0),(1.0,3.0),(0.0,0.0),(0.0,0.0))
    val SVD(u, s, vt) = svd.reduced(m)

    // u and vt are unitary
    trace(u.t * u) should be (u.cols.toDouble +- 1E-5)
    trace(vt * vt.t) should be (vt.rows.toDouble +- 1E-5)

    // s is sorted by size of singular value, and be nonnegative
    for(i <- 1 until s.length) {
      assert(s(i) <= s(i-1), s"s($i) > s(${i-1}): ${s(i)} > ${s(i-1)}")
      assert(s(i) >= 0, s"s($i) < 0: ${s(i)}")
    }


    val ss = DenseMatrix.zeros[Double](m.rows min m.cols, m.rows min m.cols)
    diag(ss(0 until s.length, 0 until s.length)) := s
    val reM: DenseMatrix[Double] =  u * ss * vt
    matricesNearlyEqual(reM, m)
  }

  test("svd reduced A(m, n), m < n") {
    val m = DenseMatrix((2.0,4.0),(1.0,3.0),(0.0,0.0),(0.0,0.0)).t
    val SVD(u, s, vt) = svd.reduced(m)

    // u and vt are unitary
    trace(u.t * u) should be (u.cols.toDouble +- 1E-5)
    trace(vt * vt.t) should be (vt.rows.toDouble +- 1E-5)

    // s is sorted by size of singular value, and be nonnegative
    for(i <- 1 until s.length) {
      assert(s(i) <= s(i-1), s"s($i) > s(${i-1}): ${s(i)} > ${s(i-1)}")
      assert(s(i) >= 0, s"s($i) < 0: ${s(i)}")
    }


    val ss = DenseMatrix.zeros[Double](m.rows min m.cols, m.rows min m.cols)
    diag(ss(0 until s.length, 0 until s.length)) := s
    val reM: DenseMatrix[Double] =  u * ss * vt
    matricesNearlyEqual(reM, m)
  }

  test("svd reduced A(m, n), m = n") {
    val m = DenseMatrix((2.0,4.0),(1.0,3.0))
    val SVD(u, s, vt) = svd.reduced(m)

    // u and vt are unitary
    trace(u.t * u) should be (u.cols.toDouble +- 1E-5)
    trace(vt * vt.t) should be (vt.rows.toDouble +- 1E-5)

    // s is sorted by size of singular value, and be nonnegative
    for(i <- 1 until s.length) {
      assert(s(i) <= s(i-1), s"s($i) > s(${i-1}): ${s(i)} > ${s(i-1)}")
      assert(s(i) >= 0, s"s($i) < 0: ${s(i)}")
    }


    val ss = DenseMatrix.zeros[Double](m.rows min m.cols, m.rows min m.cols)
    diag(ss(0 until s.length, 0 until s.length)) := s
    val reM: DenseMatrix[Double] =  u * ss * vt
    matricesNearlyEqual(reM, m)
  }

  test("svd reduced float A(m, n), m > n") {
    val m = DenseMatrix((2.0f,4.0f),(1.0f,3.0f),(0.0f,0.0f),(0.0f,0.0f))
    val SVD(u, s, vt) = svd.reduced(m)

    // u and vt are unitary
    trace(u.t * u) should be (u.cols.toFloat +- 1E-5f)
    trace(vt * vt.t) should be (vt.rows.toFloat +- 1E-5f)

    // s is sorted by size of singular value, and be nonnegative
    for(i <- 1 until s.length) {
      assert(s(i) <= s(i-1), s"s($i) > s(${i-1}): ${s(i)} > ${s(i-1)}")
      assert(s(i) >= 0, s"s($i) < 0: ${s(i)}")
    }


    val ss = DenseMatrix.zeros[Float](m.rows min m.cols, m.rows min m.cols)
    diag(ss(0 until s.length, 0 until s.length)) := s
    val reM: DenseMatrix[Float] =  u * ss * vt
    // matricesNearlyEqual(reM, m)
    for(i <- 0 until reM.rows; j <- 0 until reM.cols)
      reM(i,j) should be (m(i, j) +- 1E-6f)
  }

  test("svd reduced float A(m, n), m = n") {
    val m = DenseMatrix((2.0f,4.0f),(1.0f,3.0f))
    val SVD(u, s, vt) = svd.reduced(m)

    // u and vt are unitary
    trace(u.t * u) should be (u.cols.toFloat +- 1E-5f)
    trace(vt * vt.t) should be (vt.rows.toFloat +- 1E-5f)

    // s is sorted by size of singular value, and be nonnegative
    for(i <- 1 until s.length) {
      assert(s(i) <= s(i-1), s"s($i) > s(${i-1}): ${s(i)} > ${s(i-1)}")
      assert(s(i) >= 0, s"s($i) < 0: ${s(i)}")
    }


    val ss = DenseMatrix.zeros[Float](m.rows min m.cols, m.rows min m.cols)
    diag(ss(0 until s.length, 0 until s.length)) := s
    val reM: DenseMatrix[Float] =  u * ss * vt
    // matricesNearlyEqual(reM, m)
    for(i <- 0 until reM.rows; j <- 0 until reM.cols)
      reM(i,j) should be (m(i, j) +- 1E-5f)
  }

  test("svd reduced float A(m, n), m < n") {
    val m = DenseMatrix((2.0f,4.0f),(1.0f,3.0f),(0.0f,0.0f),(0.0f,0.0f)).t
    val SVD(u, s, vt) = svd.reduced(m)

    // u and vt are unitary
    trace(u.t * u) should be (u.cols.toFloat +- 1E-5f)
    trace(vt * vt.t) should be (vt.rows.toFloat +- 1E-5f)

    // s is sorted by size of singular value, and be nonnegative
    for(i <- 1 until s.length) {
      assert(s(i) <= s(i-1), s"s($i) > s(${i-1}): ${s(i)} > ${s(i-1)}")
      assert(s(i) >= 0, s"s($i) < 0: ${s(i)}")
    }


    val ss = DenseMatrix.zeros[Float](m.rows min m.cols, m.rows min m.cols)
    diag(ss(0 until s.length, 0 until s.length)) := s
    val reM: DenseMatrix[Float] =  u * ss * vt
    // matricesNearlyEqual(reM, m)
    for(i <- 0 until reM.rows; j <- 0 until reM.cols)
      reM(i,j) should be (m(i, j) +- 1E-5f)
  }

  test("csc svd"){
    val m1 = DenseMatrix((2.0,4.0,0.0,1.0,2.0),(1.0,0.0,2.0,1.0,0.0),
      (1.0,3.0,2.0,1.0,9.0),(0.0,0.0,2.0,0.0,5.0),(0.0,1.5,0.0,0.0,5.0),(1.5,0.0,2.0,0.0,5.0))
    val m2 = CSCMatrix((2.0,4.0,0.0,1.0,2.0),(1.0,0.0,2.0,1.0,0.0),
      (1.0,3.0,2.0,1.0,9.0),(0.0,0.0,2.0,0.0,5.0),(0.0,1.5,0.0,0.0,5.0),(1.5,0.0,2.0,0.0,5.0))

    def checkCols(m1 : DenseMatrix[Double], m2 : DenseMatrix[Double]) = {
      for (i <- 0 until m1.cols) {
        val v1 = if (m1(::,i).valueAt(0) > 0) m1(::,i) else -m1(::,i)
        val v2 = if (m2(::,i).valueAt(0) > 0) m2(::,i) else -m2(::,i)
        assert(max(abs(v1 - v2)) < 1E-5)
        assert(abs(norm(v1) - 1.0) < 1E-5)
        assert(abs(norm(v2) - 1.0) < 1E-5)
      }
    }

    val SVD(u1, s1, vt1) = svd(m1)
    val SVD(u2, s2, vt2) = svd(m2,2)
    assert(max(abs(s1.slice(0,2) - s2)) < 1E-5)
    checkCols(u1(::, 0 until 2), u2)
    checkCols(vt1(0 until 2, ::).t, vt2.t)

    val SVD(u1t, s1t, vt1t) = svd(m1.t)
    val SVD(u2t, s2t, vt2t) = svd(m2.t,2)
    assert(max(abs(s1t.slice(0,2) - s2t)) < 1E-5)
    checkCols(u1t(::, 0 until 2), u2t)
    checkCols(vt1t(0 until 2, ::).t, vt2t.t)
  }

  test("small pow test") {
    val X = DenseMatrix(( .7, .2), (.3, .8))
    assert(mpow(X, 1) === X)
    assert( max(abs(mpow(X, .5) - DenseMatrix((.82426, 0.11716), (.17574, 0.88284)))) < 1E-5, mpow(X, .5))
  }

  test("diff test") {
    val testThreshold = 1.0E-15
    val xDouble = DenseVector( .7, .2, .3, .8)
    assert( norm( diff(xDouble) - DenseVector(-0.5, 0.1, 0.5) ) < testThreshold)
    assert( norm( diff(xDouble, 2) - DenseVector(0.6, 0.4) ) < testThreshold)
    val x1 = DenseVector( .7)
    assert(diff(x1) == DenseVector[Double]())
    val xInt = DenseVector( 7, 2, 3, 8)
    assert(diff(xInt, 3) == DenseVector(-2))
  }

  test("reverse test") {
    val xDouble = DenseVector( .7, .2, .3, .8)
    assert( reverse(xDouble) == DenseVector(.8, .3, .2, .7)  )
    val xEmpty = DenseVector[Long]()
    assert( reverse(xEmpty) == DenseVector[Long]() )

    val a = SparseVector.tabulate(5)(identity)
    val b = SparseVector.zeros[Double](5)
    assert(reverse(a) === SparseVector.tabulate(5)((i: Int) => 4 - i))
    assert(reverse(b) === b)
    b(2) = 2.0
    assert(reverse(b) === b)
    b(0) = 0.1
    assert(reverse(b) === SparseVector[Double](5)((2,2.0),(4,0.1)))
    b(4) = 4.0
    assert(reverse(b) === SparseVector[Double](5)((0,4.0),(2,2.0),(4,0.1)))
  }

  test("reshape test") {

    val asv = SparseVector.tabulate(6)(identity(_) + 1)
    assert(reshape(asv,2,3) === CSCMatrix((1,2,3),(4,5,6)))
    assert(reshape(asv,3,2) === CSCMatrix((1,2),(3,4),(5,6)))

    val bsv = SparseVector.zeros[Double](6)
    assert(reshape(bsv,2,3) === CSCMatrix.zeros[Double](2,3))

    val acsc = CSCMatrix.tabulate(2,3)((i,j) => (i+1) * (j+1) + (i+1))
    val ad = DenseMatrix.tabulate(2,3)((i,j) => (i+1) * (j+1) + (i+1))
    assert(reshape(acsc,3,2).toDense == reshape(ad,3,2))
    assert(reshape(acsc,1,6).toDense == reshape(ad,1,6))
    assert(reshape(acsc,6,1).toDense == reshape(ad,6,1))

    val bcsc = CSCMatrix.zeros[Int](5,3)
    val rcsc = CSCMatrix.zeros[Int](3,5)
    val colRowGen = for {
      r <- Gen.choose(0,4)
      c <- Gen.choose(0,2)
    } yield (r,c)

    val pcLists = Prop.forAll(colRowGen){ case (r: Int,c: Int) =>
      val dld = c * 5 + r
      bcsc(r, c) = dld
      rcsc(dld % 3, dld / 3) = dld
      reshape(bcsc, 3, 5) === rcsc &&
        reshape(rcsc, 5, 3) === bcsc
    }
    check(pcLists)
  }

  test("diag test") {
    val testDV = DenseVector(0.1,1.1,2.1,3.1,4.1)
    val testDM = DenseMatrix.tabulate[Double](5,5)((r,c) => if (r == c) r.toDouble + 0.1 else 0.0)
    val testCSC = CSCMatrix.tabulate[Double](5,5)((r,c) => if (r == c) r.toDouble + 0.1 else 0.0)
    val testSV = SparseVector(0.1,1.1,2.1,3.1,4.1)

    assert(diag(testDV) === testDM)
    assert(diag(testDM) === testDV)
    assert(diag(testSV) === testCSC)
    assert(diag(testCSC) === testSV)
  }

  test("accumulate test") {
    val xDouble = DenseVector( .7, .2, .3, .8)
    assert( norm(accumulate(xDouble) - DenseVector(.7, .9, 1.2, 2.0)) < 1.0E-15  )
    val xInt = DenseVector( 7, 2, 3, 8)
    assert( norm(accumulate(xInt) - DenseVector(7, 9, 12, 20)) < 1.0E-15  )
    val xEmpty = DenseVector[Long]()
    assert( accumulate(xEmpty) == DenseVector[Long]() )
  }


  /**
   * Test based on the values in Lindsay Smith's tutorial:
   *
   * http://www.cs.otago.ac.nz/cosc453/student_tutorials/principal_components.pdf
   */
  test("pca") {

    // The data
    val smithData = DenseMatrix(
      (2.5,2.4), (0.5,0.7), (2.2,2.9), (1.9,2.2), (3.1,3.0),
      (2.3,2.7), (2.0,1.6), (1.0,1.1), (1.5,1.6), (1.1,0.9))

    // The correct answers bundled up.
    object smithTruth {

      val centeredData = DenseMatrix(
        (0.69                ,  0.4900000000000002   ),
        (-1.31               ,  -1.2099999999999997  ),
        (0.3900000000000001  ,  0.9900000000000002   ),
        (0.08999999999999986 ,  0.2900000000000005   ),
        (1.29                ,  1.0900000000000003   ),
        (0.48999999999999977 ,  0.7900000000000005   ),
        (0.18999999999999995 ,  -0.3099999999999996  ),
        (-0.81               ,  -0.8099999999999996  ),
        (-0.31000000000000005,  -0.3099999999999996  ),
        (-0.71               ,  -1.0099999999999998  ))

      val covmat = DenseMatrix(
        (0.6165555555555556,  0.6154444444444445),
        (0.6154444444444445,  0.7165555555555555))

      val eigenvalues =
        DenseVector(1.2840277121727839, 0.04908339893832735)

      val eigenvectors = DenseMatrix(
        (-0.6778733985280118,  -0.735178655544408),
        (-0.735178655544408,   0.6778733985280118))

      val scores = DenseMatrix(
        ( -0.8279701862010882,   -0.17511530704691552 ),
        (1.7775803252804288  ,  0.14285722654428046   ),
        (-0.9921974944148888 ,  0.3843749888804126    ),
        (-0.27421041597539964,  0.13041720657412714   ),
        (-1.6758014186445402 ,  -0.2094984612567533   ),
        (-0.9129491031588082 ,  0.17528244362036988   ),
        (0.099109437498444   ,  -0.34982469809712086  ),
        (1.1445721637986597  ,  0.04641725818328124   ),
        (0.43804613676244986 ,  0.017764629675083132  ),
        (1.2238205550547403  ,  -0.16267528707676204  ))
    }

    val pca = princomp(smithData)

    matricesNearlyEqual(smithData(*,::) - pca.center, smithTruth.centeredData)
    matricesNearlyEqual(pca.covmat, smithTruth.covmat)
    matricesNearlyEqual(pca.covmat, smithTruth.covmat)
    vectorsNearlyEqual(pca.eigenvalues, smithTruth.eigenvalues)
    matricesNearlyEqual(pca.loadings, smithTruth.eigenvectors)
    matricesNearlyEqual(pca.scores, smithTruth.scores)

  }

  def vectorsNearlyEqual(A: DenseVector[Double], B: DenseVector[Double], threshold: Double = 1E-6) {
    for(i <- 0 until A.length)
      A(i) should be (B(i) +- threshold)
  }

  def matricesNearlyEqual(A: DenseMatrix[Double], B: DenseMatrix[Double], threshold: Double = 1E-6) {
    for(i <- 0 until A.rows; j <- 0 until A.cols)
      A(i,j) should be (B(i, j) +- threshold)
  }

  test("RangeExtender test") {
    val xInt = DenseVector(0, 1, 2, 3, 4, 5)

    val rangeIncl = 3 to 5
    val rangeInclN1 = 3 to -1
    val rangeInclN2 = -3 to 5
    val rangeInclN3 = -1 to 3 by -1
    val rangeInclN4 = 5 to -3 by -1
    val rangeInclN5 = -1 to -3 by -1

    val rangeExcl = 0 until 5
    val rangeExclN1 = 3 until -1
    val rangeExclN2 = -1 until 3 by -1

    assert( xInt( rangeIncl ) == DenseVector(3, 4, 5), "range inclusive" )
//    println( rangeInclN1.start + " " + rangeInclN1.end + " " + rangeInclN1.step)
//    val rangeInclN1r = rangeInclN1.getRangeWithoutNegativeIndexes( xInt.length )
//    println( rangeInclN1r.start + " " + rangeInclN1r.end + " " + rangeInclN1r.step)
    assert( xInt( rangeInclN1 ) == DenseVector(3, 4, 5), "range inclusive, negative end"  )
    assert( xInt( rangeInclN2 ) == DenseVector(3, 4, 5), "range inclusive, negative start"  )
    assert( xInt( rangeInclN3 ) == DenseVector(5, 4, 3), "range inclusive, negative start/step"  )
    assert( xInt( rangeInclN4 ) == DenseVector(5, 4, 3), "range inclusive, negative end/step"  )
    assert( xInt( rangeInclN5 ) == DenseVector(5, 4, 3), "range inclusive, negative start/end/step"  )

    assert( xInt( rangeExcl ) == DenseVector(0, 1, 2, 3, 4), "range exclusive" )
    intercept[IllegalArgumentException]{ xInt(rangeExclN1) }
    intercept[IllegalArgumentException]{ xInt(rangeExclN2) }
  }

}
