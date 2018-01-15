package breeze.util

import org.scalatest.FunSuite

class SortingTest extends FunSuite {
  test("indirectSort") {
    val keys = Array(5, 4, 3, 2, 1)
    val elems = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    Sorting.indirectSort(keys, elems, 0, elems.length)
    assert(keys === Array(1, 2, 3, 4, 5))
    assert(elems === Array(5.0, 4.0, 3.0, 2.0, 1.0))
  }

  test("indirectSort with offset") {
    val keys = Array(5, 4, 3, 2, 1)
    val elems = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val off = 2
    Sorting.indirectSort(keys, elems, off, elems.length - off)
    assert(keys.take(off) === Array(5, 4))
    assert(keys.drop(off) === Array(1, 2, 3))
    assert(elems === Array(1.0, 2.0, 5.0, 4.0, 3.0))
  }

  test("indirectSort with offset and length") {
    val keys = Array(5, 4, 3, 2, 1)
    val elems = Array(1.0, 2.0, 3.0, 4.0, 5.0)
    val off = 2
    val len = 2
    Sorting.indirectSort(keys, elems, off, len)
    assert(keys.take(off) === Array(5, 4))
    assert(keys.slice(off, off + len) === Array(2, 3))
    assert(keys.last === 1)
    assert(elems === Array(1.0, 2.0, 4.0, 3.0, 5.0))
  }
}
