package breeze.linalg

import breeze.math._
import breeze.storage.Zero
import org.junit.runner.RunWith
import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._

import scala.reflect.ClassTag


/**
 *
 * @author dlwh
 */

@RunWith(classOf[JUnitRunner])
class VectorTest extends FunSuite {

  val dvTest = DenseVector(1,2,3,4)
  //val dmTest = DenseMatrix((1,2,3,4), (5,6,7,8))

  test("scan"){
    assert( dvTest.scanLeft(0)( (p1: Int, p2: Int) => p1 + p2 )  == DenseVector(0,1,3,6,10) )
    assert( dvTest.scanRight(0)( (p1: Int, p2: Int) => p1 + p2 )  == DenseVector(10,9,7,4,0) )
  }

  test("fold"){
    assert( dvTest.foldLeft(0)( (p1: Int, p2: Int) => 2 * p1  - p2 )  == - 26 )
    assert( dvTest.foldRight(0)( (p1: Int, p2: Int) => 2 * p1 - p2 )  == -4 )
  }

  test("reduce"){
    assert( dvTest.reduceLeft( (p1: Int, p2: Int) => 2 * p1  - p2 )  == - 10 )
    assert( dvTest.reduceRight( (p1: Int, p2: Int) => 2 * p1 - p2 )  == 0 )
  }

  test("unary !") {
    val b = Vector(true, false, false)
    assert(!b == Vector(false, true, true))
  }

  test("hashcode") {
    val v: DenseVector[Int] = DenseVector(1, 2, 0, 0, 3)
    val v2: SparseVector[Int] = SparseVector(5)((0 ->1), (1 -> 2), (4->3))
    assert(v === v2)
    assert(v.hashCode == v2.hashCode)
  }

  test("Min/Max") {
    val v: Vector[Int] = DenseVector(2, 0, 3, 2, -1)
    assert(argmin(v) === 4)
    assert(argmax(v) === 2)
    assert(min(v) === -1)
    assert(max(v) === 3)

    val v2: Vector[Int] = SparseVector(2, 0, 3, 2, -1)
    assert(argmin(v2) === 4)
    assert(argmax(v2) === 2)
    assert(min(v2) === -1)
    assert(max(v2) === 3)
  }

  test("assert operations of different size fail") {
    val a = Vector[Double](1D, 2D, 3D)
    val b = Vector[Double](1D, 2D, 3D, 4D)
    intercept[IllegalArgumentException] {
      a + b
    }
    intercept[IllegalArgumentException] {
      b + a
    }
  }

}


abstract class VectorPropertyTestBase[T: ClassTag: Zero: Semiring] extends TensorSpaceTestBase[Vector[T], Int, T] {
  def genScalar: Arbitrary[T]

  override implicit def genSingle: Arbitrary[Vector[T]] = Arbitrary {
    Gen.choose(1, 10).flatMap(RandomInstanceSupport.genVector(_, genScalar.arbitrary))
  }

  implicit def genTriple: Arbitrary[(Vector[T], Vector[T], Vector[T])] = Arbitrary {
    Gen.choose(1, 10).flatMap { n =>
      for {
        x <- RandomInstanceSupport.genVector(n, genScalar.arbitrary)
        y <- RandomInstanceSupport.genVector(n, genScalar.arbitrary)
        z <- RandomInstanceSupport.genVector(n, genScalar.arbitrary)
      } yield (x, y, z)
    }
  }
}

@RunWith(classOf[JUnitRunner])
class VectorOps_DoubleTest extends VectorPropertyTestBase[Double] with DoubleValuedTensorSpaceTestBase[Vector[Double], Int] {
  val space = Vector.space[Double]
  def genScalar: Arbitrary[Double] = RandomInstanceSupport.genReasonableDouble
}

@RunWith(classOf[JUnitRunner])
class VectorOps_FloatTest extends VectorPropertyTestBase[Float] {
  val space = Vector.space[Float]

  override val TOL: Double = 1E-2
  def genScalar: Arbitrary[Float] = RandomInstanceSupport.genReasonableFloat

}

@RunWith(classOf[JUnitRunner])
class VectorOps_IntTest extends VectorPropertyTestBase[Int] {
  val space = Vector.space[Int]
  def genScalar: Arbitrary[Int] = RandomInstanceSupport.genReasonableInt
}
