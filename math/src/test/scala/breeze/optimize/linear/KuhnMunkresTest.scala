package breeze.optimize.linear

import org.scalatest._
import org.scalatest.funsuite._
import org.scalatestplus.scalacheck._
import org.scalacheck._

class KuhnMunkresTest extends AnyFunSuite with Checkers {

  test("sanity check") {
    val arr = Array(Seq(2.0, 4.0, 7.0, 9.0), Seq(3.0, 9.0, 5.0, 1.0), Seq(8.0, 2.0, 9.0, 7.0));
    val (matching, weight) = KuhnMunkres.extractMatching(arr.map(_.toSeq));
    assert(weight === 5.0);
    assert(matching(0) === 0)
    assert(matching(1) === 3)
    assert(matching(2) === 1)
  }

  test("another test") {
    val arr =
      Array(Seq(14.0, 5.0, 8.0, 7.0), Seq(1.5, 12.0, 6.0, 5.0), Seq(7.0, 8.0, 3.0, 9.0), Seq(2.0, 4.0, 6.0, 10.0));
    val (matching, weight) = KuhnMunkres.extractMatching(arr.map(_.toSeq));
    assert(weight === 15.0);
    assert(matching(0) === 1)
    assert(matching(1) === 3)
    assert(matching(2) === 2)
    assert(matching(3) === 0)
  }

  test("square matrix test 1") {
    val arr = Array(Seq(400.0, 150.0, 400.0), Seq(400.0, 450.0, 600.0), Seq(300.0, 225.0, 300.0));

    val (matching, weight) = KuhnMunkres.extractMatching(arr);
    assert(weight === 850.0);
  }

  test("rectangular matrix test 1") {
    val arr = Array(Seq(400.0, 150.0, 400.0, 1.0), Seq(400.0, 450.0, 600.0, 2.0), Seq(300.0, 225.0, 300.0, 3.0));

    val (matching, weight) = KuhnMunkres.extractMatching(arr);
    assert(weight === 452.0);
  }

  test("square matrix test 2") {
    val arr = Array(Seq(10.0, 10.0, 8.0), Seq(9.0, 8.0, 1.0), Seq(9.0, 7.0, 4.0));

    val (matching, weight) = KuhnMunkres.extractMatching(arr);
    assert(weight === 18.0);
  }

  test("rectangular matrix test 2") {
    val arr = Array(Seq(10.0, 10.0, 8.0, 11.0), Seq(9.0, 8.0, 1.0, 1.0), Seq(9.0, 7.0, 4.0, 10.0));

    val (matching, weight) = KuhnMunkres.extractMatching(arr);
    assert(weight === 15.0);
  }

  test("rectangular matrix test 3") {
    val testMatrix = Seq(
      Seq(1.0, 0.0, 3.0),
      Seq(0.0, 1.0, 1.0)
    )

    val testMatrixTranspose = Seq(
      Seq(1.0, 0.0),
      Seq(0.0, 1.0),
      Seq(3.0, 1.0)
    )

    val (result1, score1) = KuhnMunkres.extractMatching(testMatrix)
    val (result2, score2) = KuhnMunkres.extractMatching(testMatrixTranspose)

    assert(result1 == Seq(1, 0))
    assert(result2 == Seq(1, 0, -1))
  }
}
