package breeze.stats

import org.scalatest.{FunSuite, WordSpec}
import org.scalatest.Matchers
import scala.util.Random
import breeze.linalg.{SparseVector, DenseVector, DenseMatrix}
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
    "mean should give correct value for SparseVectors" in {
      val a = SparseVector.zeros[Double](1000)
      a(10) = 100.0
      mean(a) should be (0.1)
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
    "covmat should produce correct values for matrix" in {
      val d = DenseMatrix((1.0,2.0), (2.0, -3.0),(3.0, 4.0), (4.0, 5.0))
      val result: DenseMatrix[Double] = covmat(d)
      assert( math.abs( result(0,1) - 2.66666667) < 1e-7)
      assert( math.abs( result(1,0) - 2.66666667) < 1e-7)
      assert( math.abs( result(0,0) - 1.66666667) < 1e-7)
      assert( math.abs( result(1,1) - 12.66666667) < 1e-7)
    }

    "covmat should produce correct values for seq of vectors" in {
      val d = Seq(DenseVector(1.0,2.0), DenseVector(2.0, -3.0),DenseVector(3.0, 4.0), DenseVector(4.0, 5.0))
      val result: DenseMatrix[Double] = covmat(d)
      assert( math.abs( result(0,1) - 2.66666667) < 1e-7)
      assert( math.abs( result(1,0) - 2.66666667) < 1e-7)
      assert( math.abs( result(0,0) - 1.66666667) < 1e-7)
      assert( math.abs( result(1,1) - 12.66666667) < 1e-7)
    }

    "corrcoeff should produce correct values" in {
      val d = DenseMatrix((1.0,2.0), (2.0, -3.0),(3.0, 4.0), (4.0, 5.0))
      val result: DenseMatrix[Double] = corrcoeff(d)
      assert( math.abs( result(0,1) - 0.580381) < 1e-7)
      assert( math.abs( result(1,0) - 0.580381) < 1e-7)
      assert( math.abs( result(0,0) - 1.0) < 1e-7)
      assert( math.abs( result(1,1) - 1.0) < 1e-7)
    }
    "corrcoeff should produce correct values for list of vectors" in {
      val d = Seq(DenseVector(1.0,2.0), DenseVector(2.0, -3.0),DenseVector(3.0, 4.0), DenseVector(4.0, 5.0))
      val result: DenseMatrix[Double] = corrcoeff(d)
      assert( math.abs( result(0,1) - 0.580381) < 1e-7)
      assert( math.abs( result(1,0) - 0.580381) < 1e-7)
      assert( math.abs( result(0,0) - 1.0) < 1e-7)
      assert( math.abs( result(1,1) - 1.0) < 1e-7)
    }

    "mode should produce the correct values" in {
      val vector = DenseVector(1.0, 2.0, 3.0, 2.0, 3.0, 3.0)
      val result = mode(vector)
      assert(result.mode == 3.0)
      assert(result.frequency == 3)
    }
    "mode should return Double.NaN for an empty collection" in {
      val vector = DenseVector[Double]()
      val result = mode(vector)
      assert(result.mode.isNaN)
      assert(result.frequency == 0)
    }
    "digitize should return proper bins" in {
      val x = DenseVector[Double](-0.5, 0.5, 1.5, 2,0, 2.5)
      val bins = DenseVector[Double](0.0,1.0,2.0)
      val result = digitize(x, bins)
      val desiredResult = DenseVector[Int](0, 1, 2, 2, 0, 3)
      assert(result == desiredResult)
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

  test("mean and variance addition") {
    val r = new Random(0)
    val data =  Array.fill(100000)(r.nextGaussian)
    val data2 =  Array.fill(100000)(r.nextGaussian * 5 + 3)
    val mav = meanAndVariance(data)
    val mav2 = meanAndVariance(data2)
    val mavTotal = meanAndVariance(data ++ data2)
    val mavSum = mav + mav2
    assert(breeze.numerics.closeTo(mavTotal.mean, mavSum.mean, 1E-5))
    assert(breeze.numerics.closeTo(mavTotal.variance, mavSum.variance, 1E-5))
    assert(mavSum.count == mavTotal.count)
  }


  test("median") {
    val dataOdd =  DenseVector(0,1,2,3,400000)
    val dataOddDuplicate =  DenseVector(0,0,0,1,2,2,2,3,400000)
    val dataEven =  DenseVector(0f,1f,2f,100f)
    val dataEvenDuplicate =  DenseVector(100,200,200,300,400,500)
    val dataEvenDuplicate2 =  DenseVector(200,250,400,300,100,500, 550, 550, 550, 550)

    val dataOddSeq = Seq(0, 1, 2, 3, 400000)
    val dataOddDuplicateSeq =  Seq(0,0,0,1,2,2,2,3,400000)
    val dataEvenSeq =  Seq(0f,1f,2f,100f)
    val dataEvenDuplicateSeq =  Seq(100,200,200,300,400,500)
    val dataEvenDuplicate2Seq =  Seq(200,250,400,300,100,500, 550, 550, 550, 550)

    assert( median(dataOdd)==2, "median (odd length) should be 2 instead of "+ median(dataOdd))
    assert( median(dataOddDuplicate)==2, "median (odd length with duplicate) should be 2 instead of "
      + median(dataOddDuplicate))
    assert( median(dataEven)==1.5f, "median (even length) should be 1.5f instead of "+ median(dataEven))
    assert( median(dataEvenDuplicate)==250, "median (even length with duplicate) should be 250 instead of "
      + median(dataEvenDuplicate))
    assert( median(dataEvenDuplicate2)==450, "median (even length with duplicate) should be 450 instead of "
      + median(dataEvenDuplicate2))

    assert( median(dataOddSeq)==2, "median (odd length) should be 2 instead of "+ median(dataOddSeq))
    assert( median(dataOddDuplicateSeq)==2, "median (odd length with duplicate) should be 2 instead of "
      + median(dataOddDuplicateSeq))
    assert( median(dataEvenSeq)==1.5f, "median (even length) should be 1.5f instead of "+ median(dataEvenSeq))
    assert( median(dataEvenDuplicateSeq)==250, "median (even length with duplicate) should be 250 instead of "
      + median(dataEvenDuplicate))
    assert( median(dataEvenDuplicate2Seq)==450, "median (even length with duplicate) should be 450 instead of "
      + median(dataEvenDuplicate2))
  }
}
