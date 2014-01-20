package breeze.linalg

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import breeze.math.{MutableVectorSpace, MutableVectorSpaceTestBase, TensorSpace, DoubleValuedTensorSpaceTestBase}
import org.scalacheck.{Prop, Arbitrary}
import org.scalatest.prop.Checkers
import breeze.numerics.closeTo

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class VectorBuilderTest extends FunSuite with Checkers {

  test("Basic VectorBuilder result tests") {
    val vb = VectorBuilder[Double](0.0, 1.0, 3.0)

    assert(vb.toSparseVector === SparseVector[Double](0.0, 1.0, 3.0))
    assert(vb.toHashVector === HashVector[Double](0.0, 1.0, 3.0))
    vb.add(0, 4.0)
    assert(vb.toSparseVector === SparseVector[Double](4.0, 1.0, 3.0))
    assert(vb.toHashVector === HashVector[Double](4.0, 1.0, 3.0))
    vb.add(0, 3.0)
    assert(vb.toSparseVector === SparseVector[Double](7.0, 1.0, 3.0))
    assert(vb.toHashVector === HashVector[Double](7.0, 1.0, 3.0))
  }

  implicit def genPair: Arbitrary[(VectorBuilder[Double], VectorBuilder[Double])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Double].map { _  % 1E3}
          xl <- Arbitrary.arbitrary[List[Int]]
          y <- Arbitrary.arbitrary[Double].map { _ % 1E3 }
          yl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (VectorBuilder(30)( xl.map(i => (i % 30).abs -> math.random * x):_*),
          VectorBuilder(30)( yl.map(i => (i % 30).abs -> math.random * y):_* ))

      }
    }
  }

  test("Dot product is consistent") {
    check(Prop.forAll{ (pair: (VectorBuilder[Double], VectorBuilder[Double])) =>
      val (vb1,vb2) = pair
      val (hv1, hv2) = (vb1.toHashVector, vb2.toHashVector)
       closeTo(vb1 dot hv2, hv1 dot vb2) && closeTo(vb1 dot hv2, hv1 dot hv2)
    })

  }

  test("+ for VB's and V's is consistent") {
    check(Prop.forAll{ (pair: (VectorBuilder[Double], VectorBuilder[Double])) =>
      val (vb1,vb2) = pair
      val (hv1, hv2) = (vb1.toHashVector, vb2.toHashVector)
      val sum1 = (vb1 + vb2).toHashVector
      val sum2 = (hv1 + hv2)
      hv1 += vb2
      hv2 += vb1
      (norm(hv1 - hv2) < 1E-4
        && norm(hv1 - sum1) < 1E-4
        && norm(hv1 - sum2) < 1E-4)
    })

  }


}

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class VectorBuilderOpsTest extends MutableVectorSpaceTestBase[VectorBuilder[Double], Double] {
 val space: MutableVectorSpace[VectorBuilder[Double], Double] = VectorBuilder.mvector_space_Double


  override val TOL: Double = 1E-4

  val N = 3
  implicit def genTriple: Arbitrary[(VectorBuilder[Double], VectorBuilder[Double], VectorBuilder[Double])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Double].map { _  % 1E3}
          xl <- Arbitrary.arbitrary[List[Int]]
          y <- Arbitrary.arbitrary[Double].map { _ % 1E3 }
          yl <- Arbitrary.arbitrary[List[Int]]
          z <- Arbitrary.arbitrary[Double].map { _ % 1E3 }
          zl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (VectorBuilder(N)( xl.take(4).map(i => (i % N).abs -> math.random * x):_*),
          VectorBuilder(N)( yl.take(4).map(i => (i % N).abs -> math.random * y):_* ),
          VectorBuilder(N)( zl.take(4).map(i => (i % N).abs -> math.random * z):_* ))
      }
    }
  }

  def genScalar: Arbitrary[Double] = Arbitrary(Arbitrary.arbitrary[Double].map{ _ % 1E3 })
}
