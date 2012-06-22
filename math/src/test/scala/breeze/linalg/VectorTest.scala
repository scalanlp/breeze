package breeze.linalg

import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith
import breeze.math.{DoubleValuedTensorSpaceTestBase, TensorSpace, TensorSpaceTestBase}


/**
 *
 * @author dlwh
 */

class VectorTest {

}


/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class VectorOps_DoubleTest extends DoubleValuedTensorSpaceTestBase[Vector[Double], Int] {
 val space: TensorSpace[Vector[Double], Int, Double] = implicitly

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