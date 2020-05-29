package breeze.util

import breeze.linalg.RandomInstanceSupport
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.{Arbitrary, Prop}

class RangeUtilsTest extends FunSuite with Checkers {
  implicit def arbRange: Arbitrary[Range] =
    Arbitrary {
      for {
        a <- RandomInstanceSupport.genReasonableInt.arbitrary.map(_.abs)
        gap <- RandomInstanceSupport.genReasonableInt.arbitrary.map(_.abs)
        step <- RandomInstanceSupport.genReasonableInt.arbitrary.map(_.abs + 1)
      } yield {
        a until (a + gap) by step
      }
    }

  def exhaustiveCheck(a: Range, b: Range) = {
    RangeUtils.overlaps(a, b) == a.toSet.intersect(b.toSet).nonEmpty
  }

  test("overlaps") {
    assert(exhaustiveCheck(4 until 9 by 7, 2 until 7 by 2))
    assert(exhaustiveCheck(4 until 9 by 12, 2 until 7 by 2))
    assert(exhaustiveCheck(5 until 13 by 10, 3 until 16 by 17))
    assert(exhaustiveCheck(682 until 791 by 173, 644 until 810 by 596))
    assert(exhaustiveCheck(700 until 791 by 261, 618 until 1617 by 2))
    assert(exhaustiveCheck(443 until 1305 by 176, 421 until 1260 by 729))
    assert(exhaustiveCheck(336 until 1083 by 542, 989 until 1320 by 716))
    // scalacheck's not being thorough enough by default
    for (i <- 0 until 10000) {
      check(Prop.forAll(exhaustiveCheck _))
    }
  }

  test("simple euclidean test") {
    val (x, y, d) = RangeUtils.extendedEuclideanAlgorithm(81, 57)
    assert(d == 3)
    assert(x == -7)
    assert(y == 10)
  }

}
