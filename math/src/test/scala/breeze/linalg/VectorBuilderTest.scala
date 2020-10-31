package breeze.linalg

import org.scalatest.FunSuite
import breeze.math._
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatestplus.scalacheck.Checkers
import breeze.numerics.closeTo

/**
 *
 * @author dlwh
 */
class VectorBuilderTest extends FunSuite with Checkers {
  test("VectorBuilder respects initialNonZero") {
    val vb = new VectorBuilder[Double](32, initialNonZero = 4)
    assert(vb.index.length === 4)
    assert(vb.data.length === 4)
  }

  test("toSparseVector on empty") {
    assert(VectorBuilder[Double]().toSparseVector === SparseVector[Double]())
  }

  test("toSparseVector on singleton") {
    assert(VectorBuilder[Double](1.0).toSparseVector === SparseVector[Double](1.0))
  }

  test("toSparseVector/toHashVector on multiple") {
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

  test("toSparseVector with data having unique keys") {
    val uniqueGen = Arbitrary {
      for {
        activeSize <- Gen.chooseNum[Int](20, 100)
        actives <- Gen.listOfN(activeSize, Gen.chooseNum[Int](0, 200))
      } yield actives.distinct
    }

    check(Prop.forAll(uniqueGen.arbitrary) { vs: List[Int] =>
      val vb = new VectorBuilder[Int](300)

      vs.foreach { v =>
        vb.add(v, v)
      }

      val result = vb.toSparseVector(keysAlreadyUnique = true)

      result.activeSize === vs.length &&
        result === SparseVector[Int](300)(vs.sorted.map(v => (v, v)):_*)
    })
  }

  test("toSparseVector with duplicated keys") {
    val inputGen = Arbitrary {
      for {
        activeSize <- Gen.chooseNum[Int](300, 3000)
        actives <- Gen.listOfN(activeSize, Gen.chooseNum[Int](0, 200))
      } yield actives
    }

    check(Prop.forAll(inputGen.arbitrary) { vs: List[Int] =>
      val vb = new VectorBuilder[Int](300)

      vs.foreach { v =>
        vb.add(v, v)
      }

      val result = vb.toSparseVector

      val expectedKVs = vs.groupBy(identity).map(p => p._1 -> p._2.sum).toList.sortBy(_._1)
      
      result.activeSize === vs.distinct.length &&
        result === SparseVector[Int](300)(expectedKVs:_*)
    })
  }

  implicit def genPair: Arbitrary[(VectorBuilder[Double], VectorBuilder[Double])] = {
    Arbitrary {
      for {
        x <- Arbitrary.arbitrary[Double].map { _ % 1E3 }
        xl <- Arbitrary.arbitrary[List[Int]]
        y <- Arbitrary.arbitrary[Double].map { _ % 1E3 }
        yl <- Arbitrary.arbitrary[List[Int]]
      } yield {
        (
          VectorBuilder(30)(xl.map(i => (i % 30).abs -> math.random * x): _*),
          VectorBuilder(30)(yl.map(i => (i % 30).abs -> math.random * y): _*))

      }
    }
  }

  test("dot is consistent") {
    check(Prop.forAll { (pair: (VectorBuilder[Double], VectorBuilder[Double])) =>
      val (vb1, vb2) = pair
      val (hv1, hv2) = (vb1.toHashVector, vb2.toHashVector)
      closeTo(vb1.dot(hv2), hv1.dot(vb2)) && closeTo(vb1.dot(hv2), hv1.dot(hv2))
    })
  }

  test("+ for VB's and V's is consistent") {
    check(Prop.forAll { (pair: (VectorBuilder[Double], VectorBuilder[Double])) =>
      val (vb1, vb2) = pair
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
class VectorBuilderOpsTest extends MutableModuleTestBase[VectorBuilder[Double], Double] {
  val space: MutableModule[VectorBuilder[Double], Double] = VectorBuilder.space[Double]

  override val TOL: Double = 1E-3

  val N = 3
  implicit def genTriple: Arbitrary[(VectorBuilder[Double], VectorBuilder[Double], VectorBuilder[Double])] = {
    Arbitrary {
      Gen.choose(1, 10).flatMap { n =>
        for {
          x <- RandomInstanceSupport.genVectorBuilder(n, genScalar.arbitrary)
          y <- RandomInstanceSupport.genVectorBuilder(n, genScalar.arbitrary)
          z <- RandomInstanceSupport.genVectorBuilder(n, genScalar.arbitrary)
        } yield (x, y, z)
      }
    }
  }

  def genScalar: Arbitrary[Double] = RandomInstanceSupport.genReasonableDouble

  override implicit def genSingle: Arbitrary[VectorBuilder[Double]] = Arbitrary {
    Gen.choose(1, 10).flatMap(RandomInstanceSupport.genVectorBuilder(_, genScalar.arbitrary))
  }
}
