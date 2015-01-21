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


  def indexSort(x: Array[Int], off: Int, len: Int, order: Array[Int]): Array[Int] = {
    indexSort_Int(x, off, len, order)
  }

  def indexSort(x: Array[Int], off: Int, len: Int, order: Array[Long]): Array[Int] = {
    indexSort_Long(x, off, len, order)
  }
  
  def indexSort(x: Array[Int], off: Int, len: Int, order: Array[Float]): Array[Int] = {
    indexSort_Float(x, off, len, order)
  }

  def indexSort(x: Array[Int], off: Int, len: Int, order: Array[Double]): Array[Int] = {
    indexSort_Double(x, off, len, order)
  }

  @expand
  def indexSort[@expand.args(Int, Long, Float, Double) T](x: Array[Int], off: Int, len: Int, order: Array[T]): Array[Int] = {
    def swap(a: Int, b: Int) {
      val t = x(a)
      x(a) = x(b)
      x(b) = t
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
      if (order(x(a)) < order(x(b))) {
        if (order(x(b)) < order(x(c))) b else if (order(x(a)) < order(x(c))) c else a
      } else {
        if (order(x(b)) > order(x(c))) b else if (order(x(a)) > order(x(c))) c else a
      }
    }
    def sort2(off: Int, len: Int) {
      // Insertion sort on smallest arrays
      if (len < 7) {
        var i = off
        while (i < len + off) {
          var j = i
          while (j>off && order(x(j-1)) > order(x(j))) {
            swap(j, j-1)
            j -= 1
          }
          i += 1
        }
      } else {
        // Choose a partition element, v
        var m = off + (len >> 1)        // Small arrays, middle element
        if (len > 7) {
          var l = off
          var n = off + len - 1
          if (len > 40) {        // Big arrays, pseudomedian of 9
            val s = len / 8
            l = med3(l, l+s, l+2*s)
            m = med3(m-s, m, m+s)
            n = med3(n-2*s, n-s, n)
          }
          m = med3(l, m, n) // Mid-size, med of 3
        }
        val v = order(x(m))

        // Establish Invariant: v* (<v)* (>v)* v*
        var a = off
        var b = a
        var c = off + len - 1
        var d = c
        var done = false
        while (!done) {
          while (b <= c && order(x(b)) <= v) {
            if (order(x(b)) == v) {
              swap(a, b)
              a += 1
            }
            b += 1
          }
          while (c >= b && order(x(c)) >= v) {
            if (order(x(c)) == v) {
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
        val n = off + len
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
    sort2(off, len)

    x
  }


}
