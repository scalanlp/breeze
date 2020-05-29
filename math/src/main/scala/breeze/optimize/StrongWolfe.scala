package breeze.optimize

import breeze.util.SerializableLogging

abstract class CubicLineSearch extends SerializableLogging with MinimizingLineSearch {
  import scala.math._

  case class Bracket(
      t: Double, // 1d line search parameter
      dd: Double, // Directional Derivative at t
      fval: Double // Function value at t
  )

  /*
   * Invoke line search, returning stepsize
   */
  def minimize(f: DiffFunction[Double], init: Double = 1.0): Double
  /*
   * Cubic interpolation to find the minimum inside the bracket l and r.
   * Uses the fval and gradient at the left and right side, which gives
   * the four bits of information required to interpolate a cubic.
   * This is additionally "safe-guarded" whereby steps too close to
   * either side of the interval will not be selected.
   */
  def interp(l: Bracket, r: Bracket) = {
    // See N&W p57 actual for an explanation of the math
    val d1 = l.dd + r.dd - 3 * (l.fval - r.fval) / (l.t - r.t)
    val d2 = sqrt(d1 * d1 - l.dd * r.dd)
    val multipler = r.t - l.t
    val t = r.t - multipler * (r.dd + d2 - d1) / (r.dd - l.dd + 2 * d2)

    // If t is too close to either end bracket, move it closer to the middle

    val lbound = l.t + 0.1 * (r.t - l.t)
    val ubound = l.t + 0.9 * (r.t - l.t)
    t match {
      case _ if t < lbound =>
        logger.debug("Cubic " + t + " below LHS limit: " + lbound)
        lbound
      case _ if t > ubound =>
        logger.debug("Cubic " + t + " above RHS limit: " + ubound)
        ubound
      case _ => t
    }
  }
}

/*
 * This line search will attempt steps larger than step length one,
 * unlike back-tracking line searches. It also comes with strong convergence
 * properties. It selects step lengths using cubic interpolation, which
 * works better than other approaches in my experience.
 * Based on Nocedal & Wright.
 */
class StrongWolfeLineSearch(maxZoomIter: Int, maxLineSearchIter: Int) extends CubicLineSearch {
  import scala.math._

  val c1 = 1e-4
  val c2 = 0.9

  def minimize(f: DiffFunction[Double], init: Double = 1.0): Double = {
    minimizeWithBound(f, init, bound = Double.PositiveInfinity)
  }

  /**
   * Performs a line search on the function f with bound, returning a point satisfying
   * the Strong Wolfe conditions OR satisfying sufficient decrease condition and hit bound.
   * Based on the line search detailed in Nocedal & Wright Numerical Optimization p58.
   * BUT add some modification for bound checking.
   */
  def minimizeWithBound(f: DiffFunction[Double], init: Double = 1.0, bound: Double = 1.0): Double = {

    require(init <= bound, "init value should <= bound")

    def phi(t: Double): Bracket = {
      val (pval, pdd) = f.calculate(t)
      Bracket(t = t, dd = pdd, fval = pval)
    }

    var t = init // Search's current multiple of pk
    var low = phi(0.0)
    val fval = low.fval
    val dd = low.dd

    if (dd > 0) {
      throw new FirstOrderException("Line search invoked with non-descent direction: " + dd)
    }

    /**
     * Assuming a point satisfying the strong wolfe conditions exists within
     * the passed interval, this method finds it by iteratively refining the
     * interval. Nocedal & Wright give the following invariants for zoom's loop:
     *
     *  - The interval bounded by low.t and hi.t contains a point satisfying the
     *    strong Wolfe conditions.
     *  - Among all points evaluated so far that satisfy the "sufficient decrease"
     *    condition, low.t is the one with the smallest fval.
     *  - hi.t is chosen so that low.dd * (hi.t - low.t) < 0.
     */
    def zoom(linit: Bracket, rinit: Bracket): Double = {

      var low = linit
      var hi = rinit

      for (i <- 0 until maxZoomIter) {
        // Interp assumes left less than right in t value, so flip if needed
        val t = if (low.t > hi.t) interp(hi, low) else interp(low, hi)

        // Evaluate objective at t, and build bracket
        val c = phi(t)
        //logger.debug("ZOOM:\n c: " + c + " \n l: " + low + " \nr: " + hi)
        logger.info(
          "Line search t: " + t + " fval: " + c.fval +
            " rhs: " + (fval + c1 * c.t * dd) + " cdd: " + c.dd
        )

        if (t.isNaN) throw new FirstOrderException(s"Line search zoom failed")

        ///////////////
        /// Update left or right bracket, or both

        if (c.fval > fval + c1 * c.t * dd || c.fval >= low.fval) {
          // "Sufficient decrease" condition not satisfied by c. Shrink interval at right
          hi = c
          logger.debug("hi=c")
        } else {

          // Zoom exit condition is the "curvature" condition
          // Essentially that the directional derivative is large enough
          if (abs(c.dd) <= c2 * abs(dd)) {
            return c.t
          }

          // If the signs don't coincide, flip left to right before updating l to c
          if (c.dd * (hi.t - low.t) >= 0) {
            logger.debug("flipping")
            hi = low
          }

          logger.debug("low=c")
          // If curvature condition not satisfied, move the left hand side of the
          // interval further away from t=0.
          low = c
        }
      }

      throw new FirstOrderException(s"Line search zoom failed")
    }

    ///////////////////////////////////////////////////////////////////

    for (i <- 0 until maxLineSearchIter) {
      val c = phi(t)

      // If phi has a bounded domain, inf or nan usually indicates we took
      // too large a step.
      if (java.lang.Double.isInfinite(c.fval) || java.lang.Double.isNaN(c.fval)) {
        t /= 2.0
        logger.error("Encountered bad values in function evaluation. Decreasing step size to " + t)
      } else {

        // Zoom if "sufficient decrease" condition is not satisfied
        if (
          (c.fval > fval + c1 * t * dd) ||
          (c.fval >= low.fval && i > 0)
        ) {
          logger.debug("Line search t: " + t + " fval: " + c.fval + " cdd: " + c.dd)
          return zoom(low, c)
        }

        // We don't need to zoom at all
        // if the strong wolfe condition is satisfied already.
        if (abs(c.dd) <= c2 * abs(dd)) {
          return c.t
        }

        // If c.dd is positive, we zoom on the inverted interval.
        // Occurs if we skipped over the nearest local minimum
        // over to the next one.
        if (c.dd >= 0) {
          logger.debug(
            "Line search t: " + t + " fval: " + c.fval +
              " rhs: " + (fval + c1 * t * dd) + " cdd: " + c.dd
          )
          return zoom(c, low)
        }

        low = c
        if (t == bound) {
          logger.debug(
            "Reach bound, satisfy sufficent decrease condition," +
              " but not curvature condition satisfied."
          )
          return bound
        } else {
          t *= 1.5
          if (t > bound) {
            t = bound
          }
          logger.debug("Sufficent Decrease condition but not curvature condition satisfied. Increased t to: " + t)
        }
      }
    }

    throw new FirstOrderException("Line search failed")
  }

}
