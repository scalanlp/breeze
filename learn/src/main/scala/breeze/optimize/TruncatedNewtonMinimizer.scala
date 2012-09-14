package breeze.optimize

import breeze.math.{MutableCoordinateSpace, MutableInnerProductSpace}
import breeze.linalg.operators.{OpMulMatrix, BinaryOp}
import breeze.util.Implicits._
import linear.ConjugateGradient
import breeze.util.logging.{ConsoleLogging, ConfiguredLogging}


/**
 * Implements a TruncatedNewton Trust region method (like Tron).
 * Also implements "Hessian Free learning". We have a few extra tricks though... :)
 *
 * @author dlwh
 */
class TruncatedNewtonMinimizer[T, H](maxIterations: Int = -1,
                                     tolerance: Double = 1E-6,
                                     l2Regularization: Double = 0.0)
                                    (implicit vs: MutableCoordinateSpace[T, Double],
                                     mult: BinaryOp[H, T, OpMulMatrix, T]) extends Minimizer[T, SecondOrderFunction[T, H]] with ConsoleLogging {

  def minimize(f: SecondOrderFunction[T, H], initial: T): T = iterations(f, initial).takeUpToWhere(_.converged).last.x

  import vs._
  case class State(iter: Int,
                   initialGNorm: Double,
                   delta: Double,
                   x: T,
                   fval: Double,
                   grad: T,
                   h: H) {
    def converged = (iter >= maxIterations && maxIterations > 0) || norm(grad) <= tolerance * initialGNorm
  }

  private def initialState(f: SecondOrderFunction[T, H], initial: T): State = {
    val (v, grad, h) = f.calculate2(initial)
    val initDelta = norm(grad)
    State(0, initDelta, initDelta, initial, v, grad, h)
  }

  // from tron
  // Parameters for updating the iterates.
  private val eta0 = 1e-4
  private val eta1 = 0.25
  private val eta2 = 0.75

  // Parameters for updating the trust region size delta.
  private val sigma1 = 0.25
  private val sigma2 = 0.5
  private val sigma3 = 4.0


  def iterations(f: SecondOrderFunction[T, H], initial: T):Iterator[State] = {
    Iterator.iterate(initialState(f, initial)){ (state: State) =>
      import state._
      val cg = new ConjugateGradient[T, H](maxNormValue = delta, tolerance = math.min(0.1, .01 * norm(grad)))
      // todo see if we can use something other than zeros as an initializer?
      val (step, residual) = cg.minimizeAndReturnResidual(-grad,  h, zeros(grad))
      val x_new = x + step

      val gs = grad dot step
      val predictedReduction = -0.5 * (gs - (step dot residual))

      val (newv, newg, newh) = f.calculate2(x_new)

      val actualReduction = fval - newv

      val stepNorm = norm(step)
      var newDelta = if(iter == 1) delta min stepNorm else delta

      val alpha = if(actualReduction <= gs) sigma3 else sigma1 max (-0.5 * (gs / (actualReduction - gs)))

      newDelta = {
        if (actualReduction < eta0 * predictedReduction)
          math.min(math.max(alpha, sigma1) * stepNorm, sigma2 * delta)
        else if (actualReduction < eta1 * predictedReduction)
          math.max(sigma1 * delta, math.min(alpha * stepNorm, sigma2 * delta))
        else if (actualReduction < eta2 * predictedReduction)
          math.max(sigma1 * delta, math.min(alpha * stepNorm, sigma3 * delta))
        else
          math.max(delta, math.min(alpha * stepNorm, sigma3 * delta))
      }

      if (actualReduction > eta0 * predictedReduction) {
        log.info("Accept %d %.2f %f %f".format(iter, delta, newv, norm(newg)))
        State(iter + 1, initialGNorm, newDelta, x_new, newv, newg, newh)
      } else {
        log.info("Reject %d %.2f".format(iter, delta))
        state.copy(iter + 1, delta = newDelta)
      }

    }

  }

}
