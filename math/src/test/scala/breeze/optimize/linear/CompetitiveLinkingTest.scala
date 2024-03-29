package breeze.optimize.linear

import org.scalatest._
import org.scalatest.funsuite._
import org.scalatestplus.scalacheck._

class CompetitiveLinkingTest extends AnyFunSuite with Checkers {
  test("sanity check") {
    val arr = Array(Seq(2.0, 4.0, 7.0, 9.0), Seq(3.0, 9.0, 5.0, 1.0), Seq(8.0, 2.0, 9.0, 7.0));
    val (matching, weight) = CompetitiveLinking.extractMatching(arr.map(_.toSeq));
    assert(weight === 5.0);
    assert(matching(0) === 0)
    assert(matching(1) === 3)
    assert(matching(2) === 1)
  }

}
