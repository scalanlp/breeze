package breeze.linalg

import breeze.math._
import org.junit.runner.RunWith
import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._


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

}


/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class VectorOps_DoubleTest extends DoubleValuedTensorSpaceTestBase[Vector[Double], Int] {
 val space = Vector.space[Double]


  val N = 30
  implicit def genTriple: Arbitrary[(Vector[Double], Vector[Double], Vector[Double])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Double].map { _  % 1E100}
          bx <- Arbitrary.arbitrary[Boolean]
          xl <- Arbitrary.arbitrary[List[Int]]
          y <- Arbitrary.arbitrary[Double].map { _ % 1E100 }
          by <- Arbitrary.arbitrary[Boolean]
          yl <- Arbitrary.arbitrary[List[Int]]
          z <- Arbitrary.arbitrary[Double].map { _ % 1E100 }
          bz <- Arbitrary.arbitrary[Boolean]
          zl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (if(bx) DenseVector.fill(N)(math.random * x) else SparseVector(N)( xl.map(i => (i % N).abs -> math.random * x ):_*),
         if(by) DenseVector.fill(N)(math.random * y) else SparseVector(N)( yl.map(i => (i % N).abs -> math.random * y ):_*),
          if(bz) DenseVector.fill(N)(math.random * z) else SparseVector(N)( zl.map(i => (i % N).abs -> math.random * z ):_*))
      }
    }
  }

  def genScalar: Arbitrary[Double] = Arbitrary(Arbitrary.arbitrary[Double].map{ _ % 1E10 })
}

@RunWith(classOf[JUnitRunner])
class VectorOps_FloatTest extends TensorSpaceTestBase[Vector[Float], Int, Float] {
 val space = Vector.space[Float]

  override val TOL: Double = 1E-2
  val N = 30
  implicit def genTriple: Arbitrary[(Vector[Float], Vector[Float], Vector[Float])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Float].map { _  % 1000f}
          bx <- Arbitrary.arbitrary[Boolean]
          xl <- Arbitrary.arbitrary[List[Int]]
          y <- Arbitrary.arbitrary[Float].map { _ % 1000f }
          by <- Arbitrary.arbitrary[Boolean]
          yl <- Arbitrary.arbitrary[List[Int]]
          z <- Arbitrary.arbitrary[Float].map { _ % 1000f }
          bz <- Arbitrary.arbitrary[Boolean]
          zl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (if(bx) DenseVector.fill(N)(math.random * x toFloat) else SparseVector(N)( xl.map(i => (i % N).abs -> (math.random * x toFloat)):_*),
         if(by) DenseVector.fill(N)(math.random * y toFloat) else SparseVector(N)( yl.map(i => (i % N).abs -> (math.random * y toFloat)):_*),
          if(bz) DenseVector.fill(N)(math.random * z toFloat) else SparseVector(N)( zl.map(i => (i % N).abs ->(math.random * z toFloat)):_*))
      }
    }
  }

  def genScalar: Arbitrary[Float] = Arbitrary(Arbitrary.arbitrary[Float].map{ _ % 1000f })
}

@RunWith(classOf[JUnitRunner])
class VectorOps_IntTest extends TensorSpaceTestBase[Vector[Int], Int, Int] {
 val space = Vector.space[Int]

  val N = 30
  implicit def genTriple: Arbitrary[(Vector[Int], Vector[Int], Vector[Int])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Int].map { _  % 1000}
          bx <- Arbitrary.arbitrary[Boolean]
          xl <- Arbitrary.arbitrary[List[Int]]
          y <- Arbitrary.arbitrary[Int].map { _ % 1000 }
          by <- Arbitrary.arbitrary[Boolean]
          yl <- Arbitrary.arbitrary[List[Int]]
          z <- Arbitrary.arbitrary[Int].map { _ % 1000 }
          bz <- Arbitrary.arbitrary[Boolean]
          zl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (if(bx) DenseVector.fill(N)(math.random * x toInt) else SparseVector(N)( xl.map(i => (i % N).abs -> (math.random * x toInt)):_*),
         if(by) DenseVector.fill(N)(math.random * y toInt) else SparseVector(N)( yl.map(i => (i % N).abs -> (math.random * y toInt)):_*),
          if(bz) DenseVector.fill(N)(math.random * z toInt) else SparseVector(N)( zl.map(i => (i % N).abs ->(math.random * z toInt)):_*))
      }
    }
  }

  def genScalar: Arbitrary[Int] = Arbitrary(Arbitrary.arbitrary[Int].map{ _ % 1000 })
}

@RunWith(classOf[JUnitRunner])
class VectorOps_ComplexTest extends TensorSpaceTestBase[Vector[Complex], Int, Complex] {
  val space = Vector.space[Complex]


  val N = 30
  implicit def genTriple: Arbitrary[(Vector[Complex], Vector[Complex], Vector[Complex])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Complex]
          bx <- Arbitrary.arbitrary[Boolean]
          xl <- Arbitrary.arbitrary[List[Int]]
          y <- Arbitrary.arbitrary[Complex]
          by <- Arbitrary.arbitrary[Boolean]
          yl <- Arbitrary.arbitrary[List[Int]]
          z <- Arbitrary.arbitrary[Complex]
          bz <- Arbitrary.arbitrary[Boolean]
          zl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (if(bx) DenseVector.fill(N)(math.random * x ) else SparseVector(N)( xl.map(i => (i % N).abs -> (math.random * x )):_*),
          if(by) DenseVector.fill(N)(math.random * y ) else SparseVector(N)( yl.map(i => (i % N).abs -> (math.random * y )):_*),
          if(bz) DenseVector.fill(N)(math.random * z ) else SparseVector(N)( zl.map(i => (i % N).abs ->(math.random * z )):_*))
      }
    }
  }

  implicit def genScalar: Arbitrary[Complex] = Arbitrary{for(r  <- Arbitrary.arbitrary[Double]; i <- Arbitrary.arbitrary[Double]) yield Complex(r % 100,i % 100)}
}