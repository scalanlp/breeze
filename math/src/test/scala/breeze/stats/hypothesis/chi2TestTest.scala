package breeze.stats.hypothesis

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.scalacheck._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class chi2TestTest extends FunSuite with Matchers {
  val threshold = 0.01
  test("Chi2 test single comparisons"){
      chi2Test(14, 200, 20, 200).pVal should be (0.2820 +- threshold)
  }

  test("Chi2 test multiple comparisons pt 1"){
      chi2Test((14, 200), Seq((20, 200)))(0).pVal should be (0.2820 +- threshold)
  }

  test("Chi2 test multiple comparisons pt 2"){
      chi2Test((14, 200), Seq((20, 200), (20, 200)))(0).pVal should be (0.4845 +- threshold)
  }
  test("Chi2 test multiple comparisons pt 3"){
      chi2Test((14, 200), Seq((20, 200), (20, 200), (20, 200)))(0).pVal should be (0.6299 +- threshold)
  }

}
