package breeze.stats.hypothesis

import org.scalatest._
import org.scalatest.funsuite._
import matchers.should.Matchers._

class chi2TestTest extends AnyFunSuite {
  val threshold = 0.01
  test("Chi2 test single comparisons") {
    chi2Test(14, 200, 20, 200).pVal should be(0.2820 +- threshold)
  }

  test("Chi2 test multiple comparisons pt 1") {
    chi2Test((14, 200), Seq((20, 200)))(0).pVal should be(0.2820 +- threshold)
  }

  test("Chi2 test multiple comparisons pt 2") {
    chi2Test((14, 200), Seq((20, 200), (20, 200)))(0).pVal should be(0.4845 +- threshold)
  }
  test("Chi2 test multiple comparisons pt 3") {
    chi2Test((14, 200), Seq((20, 200), (20, 200), (20, 200)))(0).pVal should be(0.6299 +- threshold)
  }

}
