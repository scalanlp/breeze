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
import org.scalacheck.{Arbitrary,Gen}
import scala.util.Random
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith

/**
 *
 * @author dlwh
 */

@RunWith(classOf[JUnitRunner])
class LinearAlgebraTest extends FunSuite with Checkers with ShouldMatchers {
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
    val A = DenseMatrix((1.,0.,0.),(2.,3.,0.),(4.,5.,6.))
    val Sigma = A * A.t
    assert(cholesky(Sigma) === A)
  }

  test("eigSym") {
    val A = DenseMatrix((9.,0.,0.),(0.,82.,0.),(0.,0.,25.))
    val (lambda, Some(evs)) = eigSym(A, true)
    assert(lambda === DenseVector(9.,25.,82.))
    assert(evs === DenseMatrix((1.,0.,0.),(0.,0.,1.),(0.,1.,0.)))
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
    assert(m.iterator forall { case (idx,v) => math.abs(v-aux(idx)) < 1e-4})
  }

  test("det") {
    val A = DenseMatrix((9,26,21),(48,3,11),(7,48,26))
    det(A) should be (13446.99999999 plusOrMinus 1e-8)

    val B = DenseMatrix((1,2,3),(4,5,-6),(7,8,9))
    det(B) should be (-72.0 plusOrMinus 1e-15)

    val C = DenseMatrix((1,2,3),(2,4,6),(0,-1,0)) // 1st and 2nd row linearly dep.
    det(C) should be (0.0 plusOrMinus 1e-15)

    val D = DenseMatrix((-1,1,-1),(1,2,3),(3,-10,1))
    det(D) should be (-8.0 plusOrMinus 1e-8)
  }

  test("inv") {
    val X = DenseMatrix(( 29.0, 42.0, -4.0, 50.0),
                   ( 20.0,-31.0, 32.0, 21.0),
                   (-47.0,-20.0, 24.0,-22.0),
                   (  3.0, 17.0,-45.0, 23.0))
    val I = DenseMatrix.eye[Double](4)
    assert((inv(X) * X).iterator forall { case (idx,v) => math.abs(v-I(idx)) < 1e-15})
  }

  test("pinv") {
    val X = DenseMatrix((54.0, 95.0), (23.0, 25.0), (70.0, 41.0), (31.0, 19.0))
    val I = DenseMatrix.eye[Double](2)
    assert((pinv(X) * X).iterator forall { case (idx,v) => math.abs(v-I(idx)) < 1e-15})
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
    val r1 = DenseMatrix((1.,2.,3.), (1.,2.,3.), (1.,2.,3.))  // rank 1 matrix
    val r2 = DenseMatrix((1.,2.,3.), (4.,5.,6.), (7.,8.,9.))  // rank 2 matrix
    val r3 = DenseMatrix((1.,2.,3.), (4.,5.,6.), (6.,8.,9.))  // rank 3 matrix
    assert(rank(r1) === 1)
    assert(rank(r2) === 2)
    assert(rank(r3) === 3)
  }

  test("qr") {
    val A = DenseMatrix((1.0, 1.0, 1.0), (4.0, 2.0, 1.0), (16.0, 4.0, 1.0))
    val (_Q, _R) = qr(A)

    val expectedQ = DenseMatrix((-0.06052275326,-0.6022239035,-0.7960297521),
                           (-0.2420910130,-0.7648243574,0.5970223140),
                           (-0.9683640522,0.2288450833,-0.09950371901))
    assert(_Q.rows === (expectedQ.rows))
    assert(_Q.cols === (expectedQ.cols))
    _Q foreachPair { case ((i,j), v) => v should be (expectedQ(i,j) plusOrMinus 1e-8) }

    val expectedR = DenseMatrix((-16.52271164,-4.418160988,-1.270977818),
      (0.,-1.216492285,-1.138203178),
      (0.,0.,-0.2985111571))
    _R.rows should  be (expectedR.rows)
    _R.cols should  be (expectedR.cols)
    _R foreachPair { case ((i,j), v) => v should be (expectedR(i,j) plusOrMinus 1e-8) }
  }
  test("qrp") {
    val A = DenseMatrix((1.0, 1.0, 1.0), (4.0, 2.0, 1.0), (16.0, 4.0, 1.0))
    val (_QQ, _RR, _P, _) = qrp(A)
    val ap = A * _P.values.map(_.toDouble)
    _QQ * _RR foreachPair { case ((i,j), v) => v should be (ap(i,j) plusOrMinus 1e-8) }
  }

  test("mean and variance") {
    val r = new Random(0)
    val data =  Array.fill(100000)(r.nextGaussian)
    val (m,v) = meanAndVariance(data)
    val (m2,v2) = meanAndVariance(data.iterator)
    assert(breeze.numerics.closeTo(m,0.0,1E-2), m + " should be 0")
    assert(breeze.numerics.closeTo(v,1.0,1E-2), v + " should be 1")
    assert(m2 === m)
    assert(v2 === v)
  }

  test("simple eig test") {
    val (w, _, v) = eig(diag(DenseVector(1.0, 2.0, 3.0)))
    assert(w === DenseVector(1.0, 2.0, 3.0))
    assert(v === diag(DenseVector(1.0, 1.0, 1.0)))
  }

  test("complex eig test") {
    // complex, get it?
    val (w, wi, v) = eig(DenseMatrix((1.0, -1.0), (1.0, 1.0)))
    assert(w === DenseVector(1., 1.))
    assert(wi === DenseVector(1., -1.))
    assert(v === diag(DenseVector(0.7071067811865475, -0.7071067811865475)))
    // TODO, we seem to throw out VI... these seems bad...
  }

  test("svd") {
    val m = DenseMatrix((2.0,4.0),(1.0,3.0),(0.0,0.0),(0.0,0.0))
    val (u, s, vt) = svd(m)
    assert( (u - DenseMatrix((-0.8174155604703632, -0.5760484367663208, 0.0,0.0),
                             (-0.5760484367663208, 0.8174155604703633, 0.0, 0.0),
                             (0.0,0.0,1.0,0.0),
                             (0.0,0.0,0.0,1.0) )).valuesIterator.map(_.abs).max < 1E-5)
    assert( breeze.numerics.abs(s - DenseVector(5.464985704219043, 0.3659661906262578)).max < 1E-5)
    assert( breeze.numerics.abs(vt - DenseMatrix((-0.4045535848337569,-0.9145142956773045),
      (-0.9145142956773045,0.4045535848337569))).max < 1E-5)
  }

}
