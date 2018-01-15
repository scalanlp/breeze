package breeze.util

import breeze.macros.expand

/**
 *
 * @author dlwh
 */
object Sorting {
  // Based on code by
  // /*                     __                                               *\
  //**     ________ ___   / /  ___     Scala API                            **
  //**    / __/ __// _ | / /  / _ |    (c) 2006-2009, Ross Judson           **
  //**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
  //** /____/\___/_/ |_/____/_/ | |                                         **
  //**                          |/                                          **
  //\*                                                                      */

  def indexSort(elems: Array[Int], off: Int, length: Int): Array[Int] = {
    val idx = ArrayUtil.range(0, length)
    indirectSort(elems.clone(), idx, off, length)
    idx
  }

  def indexSort(elems: Array[Long], off: Int, length: Int): Array[Int] = {
    val idx = ArrayUtil.range(0, length)
    indirectSort(elems.clone(), idx, off, length)
    idx
  }

  def indexSort(elems: Array[Float], off: Int, length: Int): Array[Int] = {
    val idx = ArrayUtil.range(0, length)
    indirectSort(elems.clone(), idx, off, length)
    idx
  }

  def indexSort(elems: Array[Double], off: Int, length: Int): Array[Int] = {
    val idx = ArrayUtil.range(0, length)
    indirectSort(elems.clone(), idx, 0, elems.length)
    idx
  }

  def indirectSort[@specialized(Int, Long, Float, Double) E](
    keys: Array[Int],
    elems: Array[E],
    off: Int,
    length: Int): Unit = indirectSort_Int(keys, elems, off, length)

  def indirectSort[@specialized(Int, Long, Float, Double) E](
    keys: Array[Long],
    elems: Array[E],
    off: Int,
    length: Int): Unit = indirectSort_Long(keys, elems, off, length)

  def indirectSort[@specialized(Int, Long, Float, Double) E](
    keys: Array[Float],
    elems: Array[E],
    off: Int,
    length: Int): Unit = indirectSort_Float(keys, elems, off, length)

  def indirectSort[@specialized(Int, Long, Float, Double) E](
    keys: Array[Double],
    elems: Array[E],
    off: Int,
    length: Int): Unit = indirectSort_Double(keys, elems, off, length)

  /**
   * Jointly sorts `keys`/`elems` in-place based on the items in the
   * `keys` array.
   *
   * If `off` is non-zero or `length` is less than the length of the arrays,
   * then only the subarrays specified by these arguments are modified. All
   * other items remain unchanged.
   *
   * The implementation is not stable.
   */
  @expand
  def indirectSort[@expand.args(Int, Long, Float, Double) K, @specialized(Int, Long, Float, Double) E](
    keys: Array[K],
    elems: Array[E],
    off: Int,
    length: Int): Unit = {
    require(keys.length == elems.length, "arrays must have the same length")
    def swap(a: Int, b: Int) {
      val t0 = keys(a)
      keys(a) = keys(b)
      keys(b) = t0
      val t1 = elems(a)
      elems(a) = elems(b)
      elems(b) = t1
    }
    def vecswap(_a: Int, _b: Int, n: Int) {
      var a = _a
      var b = _b
      var i = 0
      while (i < n) {
        swap(a, b)
        i += 1
        a += 1
        b += 1
      }
    }
    def med3(a: Int, b: Int, c: Int) = {
      if (keys(a) < keys(b)) {
        if (keys(b) < keys(c)) b else if (keys(a) < keys(c)) c else a
      } else {
        if (keys(b) > keys(c)) b else if (keys(a) > keys(c)) c else a
      }
    }
    def sort2(off: Int, length: Int) {
      // Insertion sort on smallest arrays
      if (length < 7) {
        var i = off
        while (i < length + off) {
          var j = i
          while (j>off && keys(j-1) > keys(j)) {
            swap(j, j-1)
            j -= 1
          }
          i += 1
        }
      } else {
        // Choose a partition element, v
        var m = off + (length >> 1)        // Small arrays, middle element
        if (length > 7) {
          var l = off
          var n = off + length - 1
          if (length > 40) {        // Big arrays, pseudomedian of 9
            val s = length / 8
            l = med3(l, l+s, l+2*s)
            m = med3(m-s, m, m+s)
            n = med3(n-2*s, n-s, n)
          }
          m = med3(l, m, n) // Mid-size, med of 3
        }
        val v = keys(m)

        // Establish Invariant: v* (<v)* (>v)* v*
        var a = off
        var b = a
        var c = off + length - 1
        var d = c
        var done = false
        while (!done) {
          while (b <= c && keys(b) <= v) {
            if (keys(b) == v) {
              swap(a, b)
              a += 1
            }
            b += 1
          }
          while (c >= b && keys(c) >= v) {
            if (keys(c) == v) {
              swap(c, d)
              d -= 1
            }
            c -= 1
          }
          if (b > c) {
            done = true
          } else {
            swap(b, c)
            c -= 1
            b += 1
          }
        }

        // Swap partition elements back to middle
        val n = off + length
        var s = math.min(a-off, b-a)
        vecswap(off, b-s, s)
        s = math.min(d-c, n-d-1)
        vecswap(b,   n-s, s)

        // Recursively sort non-partition-elements
        s = b - a
        if (s > 1)
          sort2(off, s)
        s = d - c
        if (s > 1)
          sort2(n-s, s)
      }
    }
    sort2(off, length)
  }
}
