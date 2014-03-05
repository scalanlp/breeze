package breeze.stats

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers



class DescriptiveStatsTest extends WordSpec with ShouldMatchers {
  "DescriptiveStats" should {
    "percentile should not explode when p = 1" in {
      val a = List.fill(100)(1.0)
      DescriptiveStats.percentile(a,1.0) should be (1.0)
    }
    "variance should not explode when size of list is 1" in {
      val a = List(1.0)
      DescriptiveStats.meanAndVariance(a) should be ((1.0,0,1))
    }
    "mean should give correct value" in {
      val a = List(1.0,2.0,3.0,4.0)
      DescriptiveStats.mean(a) should be (2.5)
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
