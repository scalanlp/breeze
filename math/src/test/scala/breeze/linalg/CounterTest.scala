package breeze.linalg
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import breeze.math.TensorSpaceTestBase
import breeze.stats.mean
import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

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
    assert(Counter("a"->1,"b"->2) *:*Counter("a"->3) === Counter("a"->3))
    assert(Counter("a"->3) *:*Counter("a"->1,"b"->2) === Counter("a"->3))
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

  test("Counting") {
    val a = Counter.count("this", "is", "my", "sentence")
    val b = Counter.count(List("this", "is", "my", "sentence"):_*)
    val c = Counter.countTraversable(Iterator("this", "is", "my", "sentence"))
    assert(a === b)
    assert(a === c)
  }

  test("Counter inplace operations") {
    import breeze.numerics._
    val x = Counter(1 -> 1.0, 2 -> 2.0)
    log.inPlace(x)
    assert(x === Counter(1 -> log(1.0), 2 -> log(2.0)))
  }
}


@RunWith(classOf[JUnitRunner])
class CounterOps_IntTest extends TensorSpaceTestBase[Counter[Int, Int], Int, Int] {
 val space = Counter.space[Int,Int]

  val N = 30
  def genTriple: Arbitrary[(Counter[Int, Int], Counter[Int, Int], Counter[Int, Int])] = {
    implicitly
  }

  implicit def genSingle: Arbitrary[Counter[Int, Int]] = {
    Arbitrary {
      for{l <- Arbitrary.arbitrary[List[Int]] } yield {
        Counter.count(l:_*)
      }
    }
  }

  def genScalar: Arbitrary[Int] = RandomInstanceSupport.genReasonableInt
}
