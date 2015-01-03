package breeze.optimize

import breeze.linalg.norm
import breeze.linalg.operators.OpMulMatrix
import breeze.math.MutableVectorField
import breeze.optimize.linear.ConjugateGradient
import breeze.util.Implicits._
import breeze.util.SerializableLogging


/**
 * Implements a TruncatedNewton Trust region method (like Tron).
 * Also implements "Hessian Free learning". We have a few extra tricks though... :)
 *
 * @author dlwh
 */
class TruncatedNewtonMinimizer[T, H](maxIterations: Int = -1,
                                     tolerance: Double = 1E-6,
                                     l2Regularization: Double = 0,
                                     m: Int = 0)
                                    (implicit space: MutableVectorField[T, Double],
                                     mult: OpMulMatrix.Impl2[H, T, T]) extends Minimizer[T, SecondOrderFunction[T, H]] with SerializableLogging {

  def minimize(f: SecondOrderFunction[T, H], initial: T): T = iterations(f, initial).takeUpToWhere(_.converged).last.x




  import space._
  case class State(iter: Int,
                   initialGNorm: Double,
                   delta: Double,
                   x: T,
                   fval: Double,
                   grad: T,
                   h: H,
                   adjFval: Double,
                   adjGrad: T,
                   stop: Boolean,
                   accept: Boolean, history: History) {
    def converged = (iter >= maxIterations && maxIterations > 0 && accept == true) || norm(adjGrad) <= tolerance * initialGNorm || stop
  }

  private def initialState(f: SecondOrderFunction[T, H], initial: T): State = {
    val (v, grad, h) = f.calculate2(initial)
    val adjgrad = grad + initial * l2Regularization
    val initDelta = norm(adjgrad)
    val adjfval = v + 0.5 * l2Regularization * (initial dot initial),
    val f_too_small = if (adjfval < -1.0e+32) true else false
    State(0, initDelta, initDelta,
      initial, v, grad, h, adjfval,
      adjgrad, f_too_small, true, initialHistory(f, initial))
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
      val cg = new ConjugateGradient[T, H](maxNormValue = delta,
        tolerance = .1 * norm(adjGrad),
        maxIterations = 400,
        normSquaredPenalty = l2Regularization)
      // todo see if we can use something other than zeros as an initializer?
      val initStep = chooseDescentDirection(state)
      val (step, residual) = cg.minimizeAndReturnResidual(-adjGrad,  h, initStep)
      val x_new = x + step

      val gs = adjGrad dot step
      val predictedReduction = -0.5 * (gs - (step dot residual))

      val (newv, newg, newh) = f.calculate2(x_new)

      val adjNewG = newg + x_new * l2Regularization
      val adjNewV = newv + 0.5 * l2Regularization * (x_new dot x_new)

      val actualReduction = adjFval - adjNewV

      val stepNorm = norm(step)
      var newDelta = if(iter == 1) delta min (stepNorm*3) else delta

      val alpha = if(-actualReduction <= gs) sigma3 else sigma1 max (-0.5 * (gs / (-actualReduction - gs)))

      newDelta = {
        if (actualReduction < eta0 * predictedReduction)
          math.min(math.max(alpha, sigma1) * stepNorm, sigma2 * newDelta)
        else if (actualReduction < eta1 * predictedReduction)
          math.max(sigma1 * newDelta, math.min(alpha * stepNorm, sigma2 * newDelta))
        else if (actualReduction < eta2 * predictedReduction)
          math.max(sigma1 * newDelta, math.min(alpha * stepNorm, sigma3 * newDelta))
        else
          math.max(newDelta, math.min(10 * stepNorm, sigma3 * newDelta))
      }

      if (actualReduction > eta0 * predictedReduction) {
        logger.info("Accept %d d=%.2E newv=%.4E newG=%.4E resNorm=%.2E pred=%.2E actual=%.2E".format(iter, delta, adjNewV, norm(adjNewG), norm(residual), predictedReduction, actualReduction))
        val stop_cond = if (adjNewV < -1.0e+32 ||
                             (math.abs(actualReduction) <= math.abs(adjNewV) * 1.0e-12
                               && math.abs(predictedReduction) <= math.abs(adjNewV) * 1.0e-12)) true else false
        val newHistory = updateHistory(x_new, adjNewG, adjNewV, state)
        val this_iter = if (state.accept = true) iter + 1 else iter
        State(this_iter, initialGNorm, newDelta, x_new, newv, newg, newh, adjNewV, adjNewG, stop_cond, true, newHistory)
      } else {
        val this_iter = if (state.accept = true) iter + 1 else iter
        val stop_cond = if (adjFval < -1.0e+32 ||
                             (math.abs(actualReduction) <= math.abs(adjFval) * 1.0e-12 && math.abs(predictedReduction) <= math.abs(adjFval) * 1.0e-12)) true else false
        logger.info("Reject %d d=%.2f resNorm=%.2f pred=%.2f actual=%.2f".format(iter, delta, norm(residual), predictedReduction, actualReduction))
        state.copy(this_iter, delta = newDelta, stop = stop_cond, accept = false)
      }

    }

  }

  // lbfgs stuff for preconditioning
    // LBFGS history
  case class History(memStep: IndexedSeq[T] = IndexedSeq.empty,
                     memGradDelta: IndexedSeq[T] = IndexedSeq.empty)

  protected def initialHistory(f: DiffFunction[T], x: T):History = new History()
  protected def chooseDescentDirection(state: State):T = {
    val grad = state.adjGrad
    val memStep = state.history.memStep
    val memGradDelta = state.history.memGradDelta
    val diag = if(memStep.size > 0) {
      computeDiagScale(memStep.head,memGradDelta.head)
    } else {
      1.0 / norm(grad)
    }

    val dir:T = copy(grad)
    val as = new Array[Double](m)
    val rho = new Array[Double](m)

    for(i <- (memStep.length-1) to 0 by -1) {
      rho(i) = (memStep(i) dot memGradDelta(i))
      as(i) = (memStep(i) dot dir)/rho(i)
      if(as(i).isNaN) {
        throw new NaNHistory
      }
      dir -= memGradDelta(i) * as(i)
    }

    dir *= diag

    for(i <- 0 until memStep.length) {
      val beta = (memGradDelta(i) dot dir)/rho(i)
      dir += memStep(i) * (as(i) - beta)
    }

    dir *= -1.0
    dir
  }


  private def computeDiagScale(prevStep: T, prevGradStep: T):Double = {
    val sy = prevStep dot prevGradStep
    val yy = prevGradStep dot prevGradStep
    if(sy < 0 || sy.isNaN) throw new NaNHistory
    sy/yy
  }

  protected def updateHistory(newX: T, newGrad: T, newVal: Double, oldState: State): History = {
    val gradDelta : T = (newGrad :- oldState.adjGrad)
    val step:T = (newX - oldState.x)

    val memStep = (step +: oldState.history.memStep) take m
    val memGradDelta = (gradDelta +: oldState.history.memGradDelta) take m


    new History(memStep,memGradDelta)
  }

}
