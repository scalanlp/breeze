package breeze.stats

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
class DescriptiveStatsTest extends WordSpec with ShouldMatchers{
"The Percentile" should {
  "not explode when p = 1" in {
    val a = List.fill(100)(1.)
    DescriptiveStats.percentile(a,1.) should be (1.)
  }
}

}
