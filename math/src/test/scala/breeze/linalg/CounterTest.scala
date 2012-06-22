package breeze.linalg

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.prop.Checkers
import breeze.math.{TensorSpace, TensorSpaceTestBase}
import org.scalacheck.Arbitrary

/**
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class CounterTest extends FunSuite with Checkers {
  val TOLERANCE = 1e-4
  def assertClose(a : Double, b : Double) =
    assert(math.abs(a - b) < TOLERANCE)

  test("Addition") {
    assert(Counter("a"->1,"b"->2) + Counter("a"->3) === Counter("a"->4,"b"->2))
    assert(Counter("a"->3) + Counter("a"->1,"b"->2) === Counter("a"->4,"b"->2))
  }

  test("Subtraction") {
    assert(Counter("a"->1,"b"->2) - Counter("a"->3) === Counter("a" -> -2, "b" -> 2))
    assert(Counter("a"->3) - Counter("a"->1,"b"->2) === Counter("a" -> 2, "b" -> -2))
  }

  test("Multiplication") {
    assert(Counter("a"->1,"b"->2) :* Counter("a"->3) === Counter("a"->3))
    assert(Counter("a"->3) :* Counter("a"->1,"b"->2) === Counter("a"->3))
  }

  test("MulInner") {
    val a = Counter(1->0.56390,2->0.36231,3->0.14601,4->0.60294,5->0.14535)
    val b = Counter(1->0.15951,2->0.83671,3->0.56002,4->0.57797,5->0.54450)
    assertClose(a dot b, .90249)
  }

  test("Zero + non zero is nonzero") {
    val a = Counter[Int,Double](1->0.0)
    val b = Counter(1->0.15951)
    assert(a + b === b, (a + b).toString + " not equal " + b)
  }

  test("Mean") {
    assert(mean(Counter(0->0.0,1->3.0)) === 1.5)
  }

  test("assignment checks both domains") {
    val a = Counter[Int,Int]()
    val b = Counter[Int,Int](3->4)
    a := b
    assert(a === b)
  }
}


@RunWith(classOf[JUnitRunner])
class CounterOps_IntTest extends TensorSpaceTestBase[Counter[Int, Int], Int, Int] {
 val space: TensorSpace[Counter[Int, Int], Int, Int] = implicitly

  val N = 30
  def genTriple: Arbitrary[(Counter[Int, Int], Counter[Int, Int], Counter[Int, Int])] = {
    implicitly
  }

  implicit def genS: Arbitrary[Counter[Int, Int]] = {
    Arbitrary {
      for{l <- Arbitrary.arbitrary[List[Int]] } yield {
        Counter.count(l)
      }
    }
  }

  def genScalar: Arbitrary[Int] = Arbitrary(Arbitrary.arbitrary[Int].map{ _ % 1000 })
}
