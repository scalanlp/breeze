package breeze.optimize

/**
 * Implements the Backtracking Linesearch like that in LBFGS-C (which is (c) 2007-2010 Naoaki Okazaki under BSD)
 *
 * Basic idea is that we need to find an alpha that is sufficiently smaller than f(0),
 * and also possibly requiring that the slope of f decrease by the right amount (wolfe conditions)
 *
 * @author dlwh
 */
class BacktrackingLineSearch(
    initfval: Double,
    maxIterations: Int = 20,
    shrinkStep: Double = 0.5,
    growStep: Double = 2.1,
    cArmijo: Double = 1E-4,
    cWolfe: Double = 0.9,
    minAlpha: Double = 1E-10,
    maxAlpha: Double = 1E10,
    enforceWolfeConditions: Boolean = true,
    enforceStrongWolfeConditions: Boolean = true)
    extends ApproximateLineSearch {
  require(shrinkStep * growStep != 1.0, "Can't do a line search with growStep * shrinkStep == 1.0")
  require(cArmijo < 0.5)
  require(cArmijo > 0.0)
  require(cWolfe > cArmijo)
  require(cWolfe < 1.0)
  def iterations(f: DiffFunction[Double], init: Double = 1.0): Iterator[State] = {
    val (f0, df0) = f.calculate(0.0)
    val initfderiv = f.calculate(init)._2
    //val (initfval, initfderiv) = f.calculate(init)
    Iterator
      .iterate((State(init, initfval, initfderiv), false, 0)) {
        case (state @ State(alpha, fval, fderiv), _, iter) =>
          val multiplier = if (fval > f0 + alpha * df0 * cArmijo) {
            shrinkStep
          } else if (enforceWolfeConditions && (fderiv < cWolfe * df0)) {
            growStep
          } else if (enforceStrongWolfeConditions && (fderiv > -cWolfe * df0)) {
            shrinkStep
          } else {
            1.0
          }
          if (multiplier == 1.0) {
            (state, true, iter)
          } else {
            val newAlpha = alpha * multiplier
            if (iter >= maxIterations) {
              throw new LineSearchFailed(0.0, 0.0)
            } else if (newAlpha < minAlpha) {
              throw new StepSizeUnderflow()
            } else if (newAlpha > maxAlpha) {
              throw new StepSizeOverflow()
            }
            val (fvalnew, fderivnew) = f.calculate(newAlpha)
            (State(newAlpha, fvalnew, fderivnew), false, iter + 1)
          }
      }
      .takeWhile(triple => !triple._2 && (triple._3 < maxIterations))
      .map(_._1)
  }
}
