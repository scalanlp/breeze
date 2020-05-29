package breeze.util

object RangeUtils {

  def overlaps(a: Range, b: Range): Boolean = {
    // easy case: completely disjoint
    if (a.isEmpty || b.isEmpty || a.start > b.end || b.start > a.end) {
      false
    } else if (a.step == 1 && b.step == 1) {
      true
    } else {
      // we want to find an index where they overlap
      // i.e. we want to find integers x, y s.t.
      // x * a.step + a.start == y * b.step + b.start
      // <=> x * a.step + y * (-b.step) == b.start - a.start
      // i.e. a linear diophantine equation
      // cf https://link.springer.com/chapter/10.1007/11792086_14
      // https://en.wikipedia.org/wiki/B%C3%A9zout%27s_identity
      // since y is an arbitrary integer, we invert the sign, which allows us to guarantee positivity, which
      // makes bookkeeping easier.
      val (x0_, y0_, gcd) = extendedEuclideanAlgorithm(a.step, b.step)
      // from here, we have:
      assert(a.step * x0_ + b.step * y0_ == gcd)
      val target = b.start - a.start
      val q = target / gcd
      val x0 = x0_ * q
      val y0 = y0_ * q
      val s = a.step / gcd
      val t = b.step / gcd
      assert(s * x0_ + t * y0_ == 1)
      if (target % gcd != 0) {
        // no solution
        false
      } else {
        // from here, we have
        // a.step * x0 - b.step * y0 == target
        // <==>
        // x0 * a.step + a.start == y0 * b.step + b.start
        assert(x0 * a.step + a.start == -y0 * b.step + b.start)
        // all solutions (x,y) will have the form
        // (x0 + k * t, y0 + m * s), for arbitrary integer m

        // debugging code to check this:
//        for ( k <- -10 to 10) {
//          val x = (x0 + k * t)
//          val y = (y0 - k * s)
//          assert (x * s + y * t == q)
//          assert(x * a.step + a.start == -y * b.step + b.start)
//        }

        // now the question is whether there are x and y that are "in bounds".
        // i.e. x \in [0, a.length) and -y \in [0, b.length) (note inverted sign on y).
        // Make the bounds inclusive and flip the sign:
        // x \in [0, a.length - 1] and y \in [-b.length+1, 0]
        // applying lemma 1 from https://link.springer.com/content/pdf/10.1007%2F11792086.pdf we get:
        // max((0 - x0)/t, (y0 - 0)/s) <= m <= min( (a.length - 1 - x0)/t, (y0 + b.length -1) /s)
        val x0d = x0.toDouble
        val y0d = y0.toDouble
        val minK = math.max(-x0d / t, y0d / s)
        val maxK = math.min((a.length - 1 - x0d) / t, (b.length - 1 + y0d) / s)
        // need to check there's an integer in this range
        math.ceil(minK) <= math.floor(maxK)
      }
    }
  }

  // returns (s, t, d) s.t. a * s + b * t == (d = gcd(a, b))
  // https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
  def extendedEuclideanAlgorithm(a: Int, b: Int): (Int, Int, Int) = {
    var s = 0L
    var old_s = 1L
    var t = 1L
    var old_t = 0L
    var r = b.toLong
    var old_r = a.toLong

    while (r != 0) {
      val quotient = old_r / r
      // (old_r, r) := (r, old_r - quotient * r)
      var temp = r
      r = old_r % r
      old_r = temp
      // (old_s, s) := (s, old_s - quotient * s)
      temp = s
      s = old_s - quotient * s
      old_s = temp
      // (old_t, t) := (t, old_t - quotient * t)
      temp = t
      t = old_t - quotient * t
      old_t = temp
    }

    (old_s.toInt, old_t.toInt, old_r.toInt)
  }
}
