package breeze.collection.mutable

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SparseArrayTest extends FunSuite with Checkers {
  test("Map") {
    val x = SparseArray(1,0,2,0,3,0,-1,-2,-3)
    x.compact()
    val y = x.map(_ + 1)
    assert(x.length === y.length)
    assert(x.activeSize === x.length - 3)
    assert(y.activeSize === 6, y)
    assert(y.toList === List(2,1,3,1,4,1,0,-1,-2))
  }

  test("Filter") {
    val x = SparseArray(1,0,2,0,3,0,-1,-2,-3)
    x.compact()
    assert(x.filter(_ % 2 == 1).toList === List(1,3))
    assert(x.filter(_ % 2 == 1).activeSize === 2)
    assert(x.filter(_ % 2 == 0).toList === List(0,2,0,0,-2))
    assert(x.filter(_ % 2 == 0).activeSize === 2, x.filter(_ % 2 == 0))
    assert(x.filter(_ > 0).toList === List(1,2,3))
    assert(x.filter(_ > 0).activeSize === 3)
    assert(x.filter(_ >= 0).toList === List(1,0,2,0,3,0))
    assert(x.filter(_ >= 0).activeSize === 3)

    val y = SparseArray(0,1,0,0,-1,-2,-3,-5)
    y.compact()
    assert(y.filter(_ > 0).toList === List(1))
    assert(y.filter(_ >= 0).toList === List(0,1,0,0))
  }
}

