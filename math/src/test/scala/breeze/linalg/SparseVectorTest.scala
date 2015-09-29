package breeze.linalg

import org.scalatest._
import org.scalatest.junit._
import org.junit.runner.RunWith
import breeze.math._
import org.scalacheck.Arbitrary
import breeze.stats.mean

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class SparseVectorTest extends FunSuite {

  val TOLERANCE = 1e-4
  def assertClose(a : Double, b : Double) =
    assert(math.abs(a - b) < TOLERANCE, a + " vs. " +  b)


  test("Min/Max") {
    val v = SparseVector(5)(0 -> 2, 2-> 3, 3-> 2)
    assert(Set(1,4) contains argmin(v), argmin(v))
    assert(argmax(v) === 2)
    assert(min(v) === 0)
    assert(max(v) === 3)
  }


  test("Mean") {
    assert(mean(SparseVector(0.0,1.0,2.0)) === 1.0)
    assert(mean(SparseVector(0.0,3.0)) === 1.5)
    assert(mean(SparseVector(3.0)) === 3.0)
  }

  test("MulInner") {
    val a = SparseVector(0.56390,0.36231,0.14601,0.60294,0.14535)
    val b = SparseVector(0.15951,0.83671,0.56002,0.57797,0.54450)
    val bd = DenseVector(0.15951,0.83671,0.56002,0.57797,0.54450)
    val bdSplit = DenseVector(0.0, 0.15951, 0.0, 0.83671,0.0, 0.56002, 0.0, 0.57797, 0.0, 0.54450)
    val bdd = bdSplit(1 to 9 by 2)
    assertClose(a dot b, .90249)
//    assertClose(a dot bd, .90249)
    assertClose(bd dot a, .90249)
    assertClose(bdd dot a, .90249)
  }

  test("Subtraction") {
    val a = SparseVector(0.56390,0.36231,0.14601,0.60294,0.14535)
    val ad = DenseVector(0.56390,0.36231,0.14601,0.60294,0.14535)
    val b = SparseVector(0.15951,0.83671,0.56002,0.57797,0.54450)
    val bd = DenseVector(0.15951,0.83671,0.56002,0.57797,0.54450)
    val bss = b - a
    val bdd = bd - ad
    b -= a
    bd -= a
    assertClose(norm(b, 2), norm(bd, 2))
    assertClose(norm(bdd, 2), norm(bd, 2))
    assertClose(norm(bss, 2), norm(bd, 2))
  }

  test("elementwise multiplication") {
    val sv = SparseVector.zeros[Int](4)
    sv(1) = 1
    sv(2) = 2

    val dv = DenseVector(1, 2, 0, 4)

    val res: SparseVector[Int] = sv :* dv

    assert(res === SparseVector(0,2,0,0))
    assert(res.activeSize == 1)

  }


  test("Norm") {
    val v = SparseVector(-0.4326, -1.6656, 0.1253, 0.2877, -1.1465)
    assertClose(norm(v, 1), 3.6577)
    assertClose(norm(v, 2), 2.0915)
    assertClose(norm(v, 3), 1.8405)
    assertClose(norm(v, 4), 1.7541)
    assertClose(norm(v, 5), 1.7146)
    assertClose(norm(v, 6), 1.6940)
    assertClose(norm(v, Double.PositiveInfinity), 1.6656)
  }

  test("SV ops work as Vector") {
    val a = SparseVector(1.0, 2.0, 3.0)
    val b = SparseVector(3.0, 4.0, 5.0)
    (a:Vector[Double]) += (b: Vector[Double])
    assert(a === SparseVector(4.0,6.0,8.0))
    assert((a: Vector[Double]).dot (b: Vector[Double]) === (a dot b))
    (a:Vector[Double]) *= (b: Vector[Double])
    assert(a === SparseVector(12.0,24.0,40.0))
  }


  test("Tabulate") {
    val m = SparseVector.tabulate(5)(i => i + 1)
    assert(m === SparseVector(1, 2, 3, 4, 5))
  }

  test("asCSCMatrix") {
    val a = SparseVector(1.0,2.0,3.0,4.0)
    val b = SparseVector.zeros[Double](5)
    val c = CSCMatrix.zeros[Double](1,5)

    // Test full
    assert(a.asCscRow === CSCMatrix((1.0,2.0,3.0,4.0)))
    // Test zero
    assert(b.asCscRow === c)
    // Test middle
    b(2) = 2.0; c(0,2) = 2.0
    assert(b.asCscRow === c)
    // Test end
    b(4) = 4.0; c(0,4) = 4.0
    assert(b.asCscRow === c)
    // Test beginning
    b(0) = 0.1; c(0,0) = 0.1
    assert(b.asCscRow === c)
  }

  test("MapPairs Double") {
    val a: SparseVector[Double] = SparseVector(1, 2, 3, 4, 5)
    val m: SparseVector[Double] = a.mapPairs( (i, x) => x + 1)
    assert(m === SparseVector(2.0, 3.0, 4.0, 5.0, 6.0))
  }

  test("MapActivePairs only touches non-zero entries: Double") {
    val a: SparseVector[Double] = SparseVector(5)()
    a(0) = 1
    a(2) = 3
    a(4) = 5
    val m: SparseVector[Double] = a.mapActivePairs( (i,x) => x+1)
    assert(m === SparseVector(2.0, 0.0, 4.0, 0.0, 6.0))
  }

  test("MapValues Double") {
    val a: SparseVector[Double] = SparseVector(1, 2, 3, 4, 5)
    val m: SparseVector[Double] = a.mapValues(_ + 1)
    assert(m === SparseVector(2.0, 3.0, 4.0, 5.0, 6.0))
  }

  test("MapActiveValues only touches non-zero entries: Double") {
    val a: SparseVector[Double] = SparseVector(5)()
    a(0) = 1
    a(2) = 3
    a(4) = 5
    val m: SparseVector[Double] = a.mapActiveValues(_+1)
    assert(m === SparseVector(2.0, 0.0, 4.0, 0.0, 6.0))
  }

  test("MapPairs Int") {
    val a: SparseVector[Int] = SparseVector(1, 2, 3, 4, 5)
    val m: SparseVector[Int] = a.mapPairs( (i, x) => x + 1)
    assert(m === SparseVector(2, 3, 4, 5, 6))
  }

  test("MapActivePairs only touches non-zero entries: Int") {
    val a: SparseVector[Int] = SparseVector(5)()
    a(0) = 1
    a(2) = 3
    a(4) = 5
    val m: SparseVector[Int] = a.mapActivePairs( (i,x) => x+1)
    assert(m === SparseVector(2, 0, 4, 0, 6))
  }

  test("MapValues Int") {
    val a: SparseVector[Int] = SparseVector(1, 2, 3, 4, 5)
    val m: SparseVector[Int] = a.mapValues(_ + 1)
    assert(m === SparseVector(2, 3, 4, 5, 6))
  }

  test("MapActiveValues only touches non-zero entries: Int") {
    val a: SparseVector[Int] = SparseVector(5)()
    a(0) = 1
    a(2) = 3
    a(4) = 5
    val m: SparseVector[Int] = a.mapActiveValues(_+1)
    assert(m === SparseVector(2, 0, 4, 0, 6))
  }

  test("MapPairs Float") {
    val a: SparseVector[Float] = SparseVector(1, 2, 3, 4, 5)
    val m: SparseVector[Float] = a.mapPairs( (i, x) => x + 1)
    assert(m === SparseVector(2f, 3f, 4f, 5f, 6f))
  }

  test("MapActivePairs only touches non-zero entries: Float") {
    val a: SparseVector[Float] = SparseVector(5)()
    a(0) = 1
    a(2) = 3
    a(4) = 5
    val m: SparseVector[Float] = a.mapActivePairs( (i,x) => x+1)
    assert(m === SparseVector(2f, 0f, 4f, 0f, 6f))
  }

  test("MapValues Float") {
    val a: SparseVector[Float] = SparseVector(1f, 2f, 3f, 4f, 5f)
    val m: SparseVector[Float] = a.mapValues(_ + 1f)
    assert(m === SparseVector(2f, 3f, 4f, 5f, 6f))
  }

  test("MapActiveValues only touches non-zero entries: Float") {
    val a: SparseVector[Float] = SparseVector(5)()
    a(0) = 1
    a(2) = 3
    a(4) = 5
    val m: SparseVector[Float] = a.mapActiveValues(_+1)
    assert(m === SparseVector(2f, 0f, 4f, 0f, 6f))
  }

  test("SparseVector * CSCMatrix Lifted OpMulMatrix & Transpose") {
    val sv = SparseVector.zeros[Int](4)
    sv(1) = 1
    sv(2) = 2

    val csc = CSCMatrix.zeros[Int](4,4)
    csc(1, 1) = 1
    csc(1, 2) = 2
    csc(2, 1) = 2
    csc(2, 2) = 4

    val svr = SparseVector.zeros[Int](4)
    svr(1) = 5
    svr(2) = 10
    val svrt = svr.t
    val svt = sv * sv.t
    assert(svt === csc)

    val svv = sv.t * csc
    assert(svv === svrt)

    sv(3) = 3
    csc(3,2) = 1
    csc(3,3) = 3
    svr(2) = 13
    svr(3) = 9
    val svvv = sv.t * csc
    assert(svvv === svr.t)

    sv(0) = 5
    csc(0,0) = 2
    csc(0,1) = 1
    svr(0) = 10
    svr(1) += 5
    val svvvv = sv.t * csc
    assert(svvvv === svr.t)


  }

  test("Transpose Complex") {
    val a = SparseVector.zeros[Complex](4)
    a(1) = Complex(1,1)
    a(2) = Complex(-2,-2)

    val expected = CSCMatrix.zeros[Complex](1, 4)
    expected(0, 1) = Complex(1,-1)
    expected(0, 2) = Complex(-2,2)

    assert(a.t === expected)
  }

  test("Generic SV ops") {
    // mostly for coverage
    val a = SparseVector("SSS")
    assert(a.copy === a)
    intercept[IndexOutOfBoundsException] {
      a(3) = ":("
      assert(false, "Shouldn't be here!")
    }
    assert(a(0) === "SSS")
    intercept[IndexOutOfBoundsException] {
      a(3)
      assert(false, "Shouldn't be here!")
    }
  }


  test("DV/SV ops") {
    val a = DenseVector(1, 2, 3)
    val b = SparseVector(3)((1,1))
    assert(a.dot(b) === 2)
    assert(a + b === DenseVector(1,3,3))
    assert(a :* b === DenseVector(0, 2, 0))

    axpy(4, b, a)
    assert( a === DenseVector(1, 6, 3))

  }

  test("SV/DV ops") {
    val a = DenseVector(1, 2, 3)
    val b = SparseVector(3)((1,1))
    assert(b.dot(a) === 2)
    assert(b + a === DenseVector(1,3,3))
    assert(b :* a === DenseVector(0, 2, 0))
    b += a
    assert(b === SparseVector(1,3,3))
  }

  test("DenseMatrix * SparseVector OpMulMatrix") {
    val x = SparseVector[Int](6)( 1 -> 2, 3 -> 4 )
    val xd = DenseVector[Int](0, 2, 0, 4, 0, 0)
    assert(x === xd)
    val m = DenseMatrix(
      ( 1, 2, 3,  4,  5,  6),
      ( 2, 4, 6,  8, 10, 12),
      ( 3, 6, 9, 12, 15, 18),
      (12, 1, 1,  0,  3,  4)
    )

    assert((m * x) ===
      m * xd)


  }

  test("#350: Dense +  SparseVector == Dense") {
    val v1 = DenseVector(0,0,0,0)
    val v2 = SparseVector(0,1,0,0)

    // do in two stages to ensure that telling the return type doesn't change type inference
    val r = v1 + v2 //type mismatch; found : breeze.linalg.Vector[Int] required: breeze.linalg.DenseVector[Int]
    val q = r:DenseVector[Int]
    assert(q == DenseVector(0,1,0,0))
  }

  test("#350: Sparse + DenseVector == Dense") {
    val v1 = DenseVector(0,0,0,0)
    val v2 = SparseVector(0,1,0,0)

    // do in two stages to ensure that telling the return type doesn't change type inference
    val r =  v2  + v1//type mismatch; found : breeze.linalg.Vector[Int] required: breeze.linalg.DenseVector[Int]
    val q = r:DenseVector[Int]
    assert(q == DenseVector(0,1,0,0))
  }

  test("#382: dividing a sparse vector") {
    val vec = SparseVector(5)(0 -> 0.0, 3 -> 60.0, 4 -> 80.0)
    val n = 60.0
    val answer1 = vec :/ n
    val answer2 = vec.toDenseVector :/ n
    assert(answer1.toDenseVector === answer2)
  }

  test("#446") {
    val x = SparseVector.zeros[Float](10)

    x.reserve(1)
    x(0) = 1
    x.asCscRow
  }

  test("#320 as CSCMatrix powers of two") {

    def foo(fill: Int) {
      val vb = new VectorBuilder[Int](421337)

      for (i ← 0 to fill) {
        vb.add(i,i)
      }

      val sv = vb.toSparseVector

      sv.asCscColumn
      sv.asCscRow
    }


    foo(1)
    foo(2)
    foo(4)
    foo(32)
    foo(64)
  }
}

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class SparseVectorOps_DoubleTest extends DoubleValuedTensorSpaceTestBase[SparseVector[Double], Int] {
 val space = SparseVector.space[Double]

  val N = 30
  implicit def genTriple: Arbitrary[(SparseVector[Double], SparseVector[Double], SparseVector[Double])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Double].map { _  % 1E100}
          xl <- Arbitrary.arbitrary[List[Int]]
          y <- Arbitrary.arbitrary[Double].map { _ % 1E100 }
          yl <- Arbitrary.arbitrary[List[Int]]
          z <- Arbitrary.arbitrary[Double].map { _ % 1E100 }
          zl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (SparseVector(N)( xl.map(i => (i % N).abs -> math.random * x):_*),
          SparseVector(N)( yl.map(i => (i % N).abs -> math.random * y):_* ),
          SparseVector(N)( zl.map(i => (i % N).abs -> math.random * z):_* ))
      }
    }
  }

  def genScalar: Arbitrary[Double] = Arbitrary(Arbitrary.arbitrary[Double].map{ _ % 1E10 })
}

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class SparseVectorOps_FloatTest extends TensorSpaceTestBase[SparseVector[Float], Int, Float] {
 val space = SparseVector.space[Float]

  override val TOL: Double = 1E-2
  val N = 30
  implicit def genTriple: Arbitrary[(SparseVector[Float], SparseVector[Float], SparseVector[Float])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Float].map { _  % 100 }
          xl <- Arbitrary.arbitrary[List[Int]]
          y <- Arbitrary.arbitrary[Float].map { _ % 100  }
          yl <- Arbitrary.arbitrary[List[Int]]
          z <- Arbitrary.arbitrary[Float].map { _ % 100 }
          zl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (SparseVector(N)( xl.map(i => (i % N).abs -> math.random.toFloat * x ):_*),
          SparseVector(N)( yl.map(i => (i % N).abs -> math.random.toFloat * y ):_* ),
          SparseVector(N)( zl.map(i => (i % N).abs -> math.random.toFloat * z ):_* ))
      }
    }
  }

  def genScalar: Arbitrary[Float] = Arbitrary(Arbitrary.arbitrary[Float].map{ _ % 1000 })

}

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class SparseVectorOps_IntTest extends TensorSpaceTestBase[SparseVector[Int], Int, Int] {
 val space = SparseVector.space[Int]

  val N = 100
  implicit def genTriple: Arbitrary[(SparseVector[Int], SparseVector[Int], SparseVector[Int])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Int].map { _  % 100 }
          xl <- Arbitrary.arbitrary[List[Int]]
          y <- Arbitrary.arbitrary[Int].map { _ % 100  }
          yl <- Arbitrary.arbitrary[List[Int]]
          z <- Arbitrary.arbitrary[Int].map { _ % 100 }
          zl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (SparseVector(N)( xl.map(i => (i % N).abs -> (math.random* x ).toInt ):_*),
          SparseVector(N)( yl.map(i => (i % N).abs -> (math.random * y).toInt ):_* ),
          SparseVector(N)( zl.map(i => (i % N).abs -> (math.random * z).toInt ):_* ))
      }
    }
  }

  def genScalar: Arbitrary[Int] = Arbitrary(Arbitrary.arbitrary[Int].map{ _ % 1000 })
}
