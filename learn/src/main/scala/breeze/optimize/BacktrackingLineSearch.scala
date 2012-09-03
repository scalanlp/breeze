package breeze.optimize

/**
 * Implements a basic line search that returns iterates where
 * each step is smaller than the last.
 * f(alpha)... f(.2 * alpha) ...
 * @author dlwh
 */
class BacktrackingLineSearch(cScale: Double = 0.5, initAlpha: Double = 1.0, maxIter: Int = 20) extends ApproximateLineSearch {
  def iterations(f: (Double) => Double): Iterator[State] = {
    val alphas = Iterator.iterate(initAlpha)(_ * cScale) take (maxIter);
    val r = alphas.map { alpha =>
      State(alpha,f(alpha))
    }
    r
  }
}