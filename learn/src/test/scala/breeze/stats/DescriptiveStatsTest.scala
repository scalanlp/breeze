package breeze.stats

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers



class DescriptiveStatsTest extends WordSpec with ShouldMatchers {
  "DescriptiveStats" should {
    "percentile should not explode when p = 1" in {
      val a = List.fill(100)(1.0)
      DescriptiveStats.percentile(a,1.) should be (1.0)
    }
    "variance should not explode when size of list is 1" in {
      val a = List(1.)
      DescriptiveStats.meanAndVariance(a) should be ((1.0,0))
    }
  }


}
