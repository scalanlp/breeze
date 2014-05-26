package breeze.stats

import org.scalatest.{FunSuite, WordSpec}
import org.scalatest.Matchers
import scala.util.Random
import breeze.linalg.DenseVector
import breeze.math.Complex

class DescriptiveStatsTest extends WordSpec with Matchers {
  "DescriptiveStats" should {
    "percentile should not explode when p = 1" in {
      val a = List.fill(100)(1.0)
      DescriptiveStats.percentile(a,1.0) should be (1.0)
    }
    "variance should not explode when size of list is 1" in {
      val a = List(1.0)
      meanAndVariance(a) should be (MeanAndVariance(1.0,0,1))
    }
    "mean should give correct value" in {
      val a = List(1.0,2.0,3.0,4.0)
      mean(a) should be (2.5)
    }
    "covariance should not explode when size of list is 1" in {
      val a = List(1.0)
      val b = List(2.0)
      DescriptiveStats.meanAndCov(a,b) should be ((1.0,2.0,0))
    }
    "covariance should produce correct values" in {
      val a = List(1.0,2.0,3.0,4)
      val b = List(2.0,-3,4.0,5)
      DescriptiveStats.cov(a,b) should be (2+(2.0/3))
    }
  }
}

class DescriptiveStatsTest2 extends FunSuite {
  //Tests copied over from LinearAlgebraTests.scala

  test("complex mean") {
    import breeze.{math=>bmath}
    import breeze.math.Complex
    val data =  DenseVector[Complex]( (0.0 + 1.0 * bmath.i), (1.0 + 0.0 * bmath.i), (2.0 + 2.0 * bmath.i) )
    assert( mean(data) === (1.0 + 1.0 * bmath.i))
  }

  test("mean and variance") {
    val r = new Random(0)
    val data =  Array.fill(100000)(r.nextGaussian)
    val mav = meanAndVariance(data)
    val mav2 = meanAndVariance(data.iterator)
    assert(breeze.numerics.closeTo(mav.mean,0.0,1E-2), mav.mean + " should be 0")
    assert(breeze.numerics.closeTo(mav.variance,1.0,1E-2), mav.variance + " should be 1")
    assert(mav == mav2)
  }

  //  test("complex mean") {
  //    val data =  DenseVector[Complex]( (0.0 + 1.0 * bmath.i), (1.0 + 0.0 * bmath.i), (2.0 + 2.0 * bmath.i) )
  //    assert( mean(data) == (1.0 + 1.0 * bmath.i), "complex mean incorrect")
  //  }

  test("median") {
    val dataOdd =  DenseVector(0,1,2,3,400000)
    val dataOddDuplicate =  DenseVector(0,0,0,1,2,2,2,3,400000)
    val dataEven =  DenseVector(0f,1f,2f,100f)
    val dataEvenDuplicate =  DenseVector(100,200,200,300,400,500)
    val dataEvenDuplicate2 =  DenseVector(200,250,400,300,100,500, 550, 550, 550, 550)

    assert( median(dataOdd)==2, "median (odd length) should be 2 instead of "+ median(dataOdd))
    assert( median(dataOddDuplicate)==2)
    assert( median(dataEven)==1.5f, "median (even length) should be 1.5f instead of "+ median(dataOdd))
    assert( median(dataEvenDuplicate)==250)
    assert( median(dataEvenDuplicate2)==450)
  }
}

