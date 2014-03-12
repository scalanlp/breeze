package breeze.linalg

import org.scalatest._
import org.scalatest.junit._
import org.junit.runner.RunWith
import breeze.math.{TensorSpaceTestBase, TensorSpace, DoubleValuedTensorSpaceTestBase}
import breeze.stats.mean
import org.scalacheck.Arbitrary

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class HashVectorTest extends FunSuite {

  val TOLERANCE = 1e-4
  def assertClose(a : Double, b : Double) =
    assert(math.abs(a - b) < TOLERANCE, a + " vs. " +  b)


  test("Min/Max") {
    val v = HashVector(2, 0, 3, 2, -1)
    assert(argmin(v) === 4)
    assert(argmax(v) === 2)
    assert(min(v) === -1)
    assert(max(v) === 3)
  }


  test("Mean") {
    assert(mean(HashVector(0.0,1.0,2.0)) === 1.0)
    assert(mean(HashVector(0.0,3.0)) === 1.5)
    assert(mean(HashVector(3.0)) === 3.0)
  }

  test("MulInner") {
    val a = HashVector(0.56390,0.36231,0.14601,0.60294,0.14535)
    val b = HashVector(0.15951,0.83671,0.56002,0.57797,0.54450)
    val bd = DenseVector(0.15951,0.83671,0.56002,0.57797,0.54450)
    val bdSplit = DenseVector(0.0, 0.15951, 0.0, 0.83671,0.0, 0.56002, 0.0, 0.57797, 0.0, 0.54450)
    val bdd = bdSplit(1 to 9 by 2)
    assertClose(a dot b, .90249)
//    assertClose(a dot bd, .90249)
    assertClose(bd dot a, .90249)
    assertClose(bdd dot a, .90249)
  }

  test("Subtraction") {
    val a = HashVector(0.56390,0.36231,0.14601,0.60294,0.14535)
    val ad = DenseVector(0.56390,0.36231,0.14601,0.60294,0.14535)
    val b = HashVector(0.15951,0.83671,0.56002,0.57797,0.54450)
    val bd = DenseVector(0.15951,0.83671,0.56002,0.57797,0.54450)
    val bss = b - a
    val bdd = bd - ad
    b -= a
    bd -= a
    assertClose(b.norm(2), bd.norm(2))
    assertClose(bdd.norm(2), bd.norm(2))
    assertClose(bss.norm(2), bd.norm(2))
  }


  test("Norm") {
    val v = HashVector(-0.4326, -1.6656, 0.1253, 0.2877, -1.1465)
    assertClose(v.norm(1), 3.6577)
    assertClose(v.norm(2), 2.0915)
    assertClose(v.norm(3), 1.8405)
    assertClose(v.norm(4), 1.7541)
    assertClose(v.norm(5), 1.7146)
    assertClose(v.norm(6), 1.6940)
    assertClose(v.norm(Double.PositiveInfinity), 1.6656)
  }

  test("SV ops work as Vector") {
    val a = HashVector(1.0, 2.0, 3.0)
    val b = HashVector(3.0, 4.0, 5.0)
    (a:Vector[Double]) += (b: Vector[Double])
    assert(a === HashVector(4.0,6.0,8.0))
    assert((a: Vector[Double]).dot (b: Vector[Double]) === (a dot b))
    (a:Vector[Double]) *= (b: Vector[Double])
    assert(a === HashVector(12.0,24.0,40.0))
  }


  test("Tabulate") {
    val m = HashVector.tabulate(5)(i => i + 1)
    assert(m === HashVector(1, 2, 3, 4, 5))
  }

  test("MapPairs Double") {
    val a: HashVector[Double] = HashVector(1, 2, 3, 4, 5)
    val m: HashVector[Double] = a.mapPairs( (i, x) => x + 1)
    assert(m === HashVector(2.0, 3.0, 4.0, 5.0, 6.0))
  }

  test("MapActivePairs only touches non-zero entries: Double") {
    val a: HashVector[Double] = HashVector(5)()
    a(0) = 1
    a(2) = 3
    a(4) = 5
    val m: HashVector[Double] = a.mapActivePairs( (i,x) => x+1)
    assert(m === HashVector(2.0, 0.0, 4.0, 0.0, 6.0))
  }

  test("MapValues Double") {
    val a: HashVector[Double] = HashVector(1, 2, 3, 4, 5)
    val m: HashVector[Double] = a.mapValues(_ + 1)
    assert(m === HashVector(2.0, 3.0, 4.0, 5.0, 6.0))
  }

  test("MapActiveValues only touches non-zero entries: Double") {
    val a: HashVector[Double] = HashVector(5)()
    a(0) = 1
    a(2) = 3
    a(4) = 5
    val m: HashVector[Double] = a.mapActiveValues(_+1)
    assert(m === HashVector(2.0, 0.0, 4.0, 0.0, 6.0))
  }

  test("MapPairs Int") {
    val a: HashVector[Int] = HashVector(1, 2, 3, 4, 5)
    val m: HashVector[Int] = a.mapPairs( (i, x) => x + 1)
    assert(m === HashVector(2, 3, 4, 5, 6))
  }

  test("MapActivePairs only touches non-zero entries: Int") {
    val a: HashVector[Int] = HashVector(5)()
    a(0) = 1
    a(2) = 3
    a(4) = 5
    val m: HashVector[Int] = a.mapActivePairs( (i,x) => x+1)
    assert(m === HashVector(2, 0, 4, 0, 6))
  }

  test("MapValues Int") {
    val a: HashVector[Int] = HashVector(1, 2, 3, 4, 5)
    val m: HashVector[Int] = a.mapValues(_ + 1)
    assert(m === HashVector(2, 3, 4, 5, 6))
  }

  test("MapActiveValues only touches non-zero entries: Int") {
    val a: HashVector[Int] = HashVector(5)()
    a(0) = 1
    a(2) = 3
    a(4) = 5
    val m: HashVector[Int] = a.mapActiveValues(_+1)
    assert(m === HashVector(2, 0, 4, 0, 6))
  }

  test("MapPairs Float") {
    val a: HashVector[Float] = HashVector(1, 2, 3, 4, 5)
    val m: HashVector[Float] = a.mapPairs( (i, x) => x + 1)
    assert(m === HashVector(2f, 3f, 4f, 5f, 6f))
  }

  test("MapActivePairs only touches non-zero entries: Float") {
    val a: HashVector[Float] = HashVector(5)()
    a(0) = 1
    a(2) = 3
    a(4) = 5
    val m: HashVector[Float] = a.mapActivePairs( (i,x) => x+1)
    assert(m === HashVector(2f, 0f, 4f, 0f, 6f))
  }

  test("MapValues Float") {
    val a: HashVector[Float] = HashVector(1f, 2f, 3f, 4f, 5f)
    val m: HashVector[Float] = a.mapValues(_ + 1f)
    assert(m === HashVector(2f, 3f, 4f, 5f, 6f))
  }

  test("MapActiveValues only touches non-zero entries: Float") {
    val a: HashVector[Float] = HashVector(5)()
    a(0) = 1
    a(2) = 3
    a(4) = 5
    val m: HashVector[Float] = a.mapActiveValues(_+1)
    assert(m === HashVector(2f, 0f, 4f, 0f, 6f))
  }

  test("Generic SV ops") {
    // mostly for coverage
    val a = HashVector("SSS")
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

  test("DV/HV ops") {
    val a = DenseVector(1, 2, 3)
    val b = HashVector(3)((1,1))
    assert(a.dot(b) === 2)
    assert(a + b === DenseVector(1,3,3))
    assert(a :* b === DenseVector(0, 2, 0))
  }

  test("HV/DV ops") {
    val a = DenseVector(1, 2, 3)
    val b = HashVector(3)((1,1))
    assert(b.dot(a) === 2)
    assert(b + a === DenseVector(1,3,3))
    assert(b :* a === DenseVector(0, 2, 0))
    b += a
    assert(b === HashVector(1,3,3))
  }

  test("HV/SV ops") {
    val a = SparseVector(3)( (1,2), (2,3))
    val b = HashVector(3)((1,1))
    assert(b.dot(a) === 2)
    assert(b + a === HashVector(0,3,3))
    assert(b :* a === HashVector(0, 2, 0))
    b += a
    assert(b === HashVector(0,3,3))
  }

  test("SV/HV ops") {
    val a = SparseVector(3)( (1,2), (2,3))
    val b = HashVector(3)((1,1))
    assert(a.dot(b) === 2)
    assert(a + b === SparseVector(0,3,3))
    assert(a :* b === SparseVector(0, 2, 0))
    a += b
    assert(a === SparseVector(0,3,3))
  }
}

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class HashVectorOps_DoubleTest extends DoubleValuedTensorSpaceTestBase[HashVector[Double], Int] {
 val space: TensorSpace[HashVector[Double], Int, Double] = implicitly

  val N = 30
  implicit def genTriple: Arbitrary[(HashVector[Double], HashVector[Double], HashVector[Double])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Double].map { _  % 1E100}
          xl <- Arbitrary.arbitrary[List[Int]]
          y <- Arbitrary.arbitrary[Double].map { _ % 1E100 }
          yl <- Arbitrary.arbitrary[List[Int]]
          z <- Arbitrary.arbitrary[Double].map { _ % 1E100 }
          zl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (HashVector(N)( xl.map(i => (i % N).abs -> math.random * x):_*),
          HashVector(N)( yl.map(i => (i % N).abs -> math.random * y):_* ),
          HashVector(N)( zl.map(i => (i % N).abs -> math.random * z):_* ))
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
class HashVectorOps_FloatTest extends TensorSpaceTestBase[HashVector[Float], Int, Float] {
 val space: TensorSpace[HashVector[Float], Int, Float] = implicitly

  override val TOL: Double = 1E-2
  val N = 30
  implicit def genTriple: Arbitrary[(HashVector[Float], HashVector[Float], HashVector[Float])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Float].map { _  % 100 }
          xl <- Arbitrary.arbitrary[List[Int]]
          y <- Arbitrary.arbitrary[Float].map { _ % 100  }
          yl <- Arbitrary.arbitrary[List[Int]]
          z <- Arbitrary.arbitrary[Float].map { _ % 100 }
          zl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (HashVector(N)( xl.map(i => (i % N).abs -> math.random.toFloat * x ):_*),
          HashVector(N)( yl.map(i => (i % N).abs -> math.random.toFloat * y ):_* ),
          HashVector(N)( zl.map(i => (i % N).abs -> math.random.toFloat * z ):_* ))
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
class HashVectorOps_IntTest extends TensorSpaceTestBase[HashVector[Int], Int, Int] {
 val space: TensorSpace[HashVector[Int], Int, Int] = implicitly

  val N = 100
  implicit def genTriple: Arbitrary[(HashVector[Int], HashVector[Int], HashVector[Int])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Int].map { _  % 100 }
          xl <- Arbitrary.arbitrary[List[Int]]
          y <- Arbitrary.arbitrary[Int].map { _ % 100  }
          yl <- Arbitrary.arbitrary[List[Int]]
          z <- Arbitrary.arbitrary[Int].map { _ % 100 }
          zl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (HashVector(N)( xl.map(i => (i % N).abs -> (math.random* x ).toInt ):_*),
          HashVector(N)( yl.map(i => (i % N).abs -> (math.random * y).toInt ):_* ),
          HashVector(N)( zl.map(i => (i % N).abs -> (math.random * z).toInt ):_* ))
      }
    }
  }

  def genScalar: Arbitrary[Int] = Arbitrary(Arbitrary.arbitrary[Int].map{ _ % 1000 })
}
