package breeze.optimize

import breeze.math.{NormedVectorSpace, MutableCoordinateSpace}
import breeze.util.Implicits._
import com.typesafe.scalalogging.log4j.Logging

/**
 *
 * @param minImprovementWindow How many iterations to improve function by at least improvementTol
 * @author dlwh
 */
abstract class FirstOrderMinimizer[T,-DF<:StochasticDiffFunction[T]](maxIter: Int = -1,
                                                                     tolerance: Double=1E-6,
                                                                     improvementTol: Double=1E-3,
                                                                     val minImprovementWindow: Int = 10,
                                                                     val numberOfImprovementFailures: Int = 1)(implicit vspace: NormedVectorSpace[T, Double]) extends Minimizer[T,DF] with Logging {

  type History
  case class State(x: T,
                   value: Double, grad: T,
                   adjustedValue: Double, adjustedGradient: T,
                   iter: Int,
                   initialAdjVal: Double,
                   history: History,
                   fVals: IndexedSeq[Double] = Vector(Double.PositiveInfinity),
                   numImprovementFailures: Int = 0,
                   searchFailed: Boolean = false) {
  }

  protected def initialHistory(f: DF, init: T): History
  protected def adjust(newX: T, newGrad: T, newVal: Double):(Double,T) = (newVal,newGrad)
  protected def chooseDescentDirection(state: State, f: DF):T
  protected def determineStepSize(state: State, f: DF, direction: T):Double
  protected def takeStep(state: State, dir: T, stepSize:Double):T
  protected def updateHistory(newX: T, newGrad: T, newVal: Double, f: DF, oldState: State):History

  protected def updateFValWindow(oldState: State, newAdjVal: Double):IndexedSeq[Double] = {
    val interm = oldState.fVals :+ newAdjVal
    if(interm.length > minImprovementWindow) interm.drop(1)
    else interm
  }

  protected def initialState(f: DF, init: T) = {
    val x = init
    val history = initialHistory(f,init)
    val (value, grad) = calculateObjective(f, x, history)
    val (adjValue,adjGrad) = adjust(x,grad,value)
    State(x,value,grad,adjValue,adjGrad,0,adjValue,history)
  }


  protected def calculateObjective(f: DF, x: T, history: History): (Double, T) = {
     f.calculate(x)
  }

  def iterations(f: DF,init: T): Iterator[State] = {
    var failedOnce = false
    val it = Iterator.iterate(initialState(f,init)) { state => try {
        val dir = chooseDescentDirection(state, f)
        val stepSize = determineStepSize(state, f, dir)
        logger.info(f"Step Size: $stepSize%.4g")
        val x = takeStep(state,dir,stepSize)
        val (value,grad) = calculateObjective(f, x, state.history)
        val (adjValue,adjGrad) = adjust(x,grad,value)
        val oneOffImprovement = (state.adjustedValue - adjValue)/(state.adjustedValue.abs max adjValue.abs max 1E-6 * state.initialAdjVal.abs)
        logger.info(f"Val and Grad Norm: $adjValue%.6g (rel: $oneOffImprovement%.3g) ${vspace.norm(adjGrad)}%.6g")
        val history = updateHistory(x,grad,value, f, state)
        val newAverage = updateFValWindow(state, adjValue)
        failedOnce = false
        var s = State(x,value,grad,adjValue,adjGrad,state.iter + 1, state.initialAdjVal, history, newAverage, 0)
        val improvementFailure = (state.fVals.length >= minImprovementWindow && state.fVals.nonEmpty && state.fVals.last > state.fVals.head * (1-improvementTol))
        if(improvementFailure)
          s = s.copy(fVals = IndexedSeq.empty, numImprovementFailures = state.numImprovementFailures + 1)
        s
      } catch {
        case x: FirstOrderException if !failedOnce =>
          failedOnce = true
          logger.error("Failure! Resetting history: " + x)
          state.copy(history = initialHistory(f, state.x))
        case x: FirstOrderException =>
          logger.error("Failure again! Giving up and returning. Maybe the objective is just poorly behaved?")
          state.copy(searchFailed = true)
      }
    }.takeUpToWhere(iteratingShouldStop)
    it: Iterator[State]
  }
  def iteratingShouldStop(state: State) = {
    ((state.iter >= maxIter && maxIter >= 0)
      || (!state.fVals.isEmpty && (state.adjustedValue - state.fVals.max).abs <= tolerance)
      || (state.numImprovementFailures >= numberOfImprovementFailures)
      || (vspace.norm(state.adjustedGradient) <= math.max(tolerance * state.adjustedValue.abs,1E-8))
      || state.searchFailed)
  }

  def minimize(f: DF, init: T): T = {
    iterations(f, init).last.x
  }

}

sealed class FirstOrderException(msg: String="") extends RuntimeException(msg)
class NaNHistory extends FirstOrderException
class StepSizeUnderflow extends FirstOrderException
class StepSizeOverflow extends FirstOrderException
class LineSearchFailed(gradNorm: Double, dirNorm: Double) extends FirstOrderException("Grad norm: %.4f Dir Norm: %.4f".format(gradNorm, dirNorm))

object FirstOrderMinimizer {

  /**
   * OptParams is a Configuration-compatible case class that can be used to select optimization
   * routines at runtime.
   *
   * Configurations:
   * 1) useStochastic=false,useL1=false: LBFGS with L2 regularization
   * 2) useStochastic=false,useL1=true: OWLQN with L1 regularization
   * 3) useStochastic=true,useL1=false: AdaptiveGradientDescent with L2 regularization
   * 3) useStochastic=true,useL1=true: AdaptiveGradientDescent with L1 regularization
   *
   *
   * @param batchSize size of batches to use if useStochastic and you give a BatchDiffFunction
   * @param regularization regularization constant to use.
   * @param alpha rate of change to use, only applies to SGD.
   * @param maxIterations, how many iterations to do.
   * @param useL1 if true, use L1 regularization. Otherwise, use L2.
   * @param tolerance convergence tolerance, looking at both average improvement and the norm of the gradient.
   * @param useStochastic if false, use LBFGS or OWLQN. If true, use some variant of Stochastic Gradient Descent.
   */
  case class OptParams(batchSize:Int = 512,
                       regularization: Double = 0.0,
                       alpha: Double = 0.5,
                       maxIterations:Int = 1000,
                       useL1: Boolean = false,
                       tolerance:Double = 1E-5,
                       useStochastic: Boolean= false) {
    def minimize[T](f: BatchDiffFunction[T], init: T)(implicit arith: MutableCoordinateSpace[T, Double]): T = {
      this.iterations(f, init).last.x
    }

    def minimize[T](f: DiffFunction[T], init: T)(implicit arith: MutableCoordinateSpace[T, Double]): T = {
      this.iterations(f, init).last.x
    }

    def iterations[T](f: BatchDiffFunction[T], init: T)(implicit arith: MutableCoordinateSpace[T, Double]): Iterator[FirstOrderMinimizer[T, BatchDiffFunction[T]]#State] = {
      val it = if(useStochastic) {
         this.iterations(f.withRandomBatches(batchSize), init)(arith)
      } else {
        iterations(f:DiffFunction[T], init)
      }

      it.asInstanceOf[Iterator[FirstOrderMinimizer[T, BatchDiffFunction[T]]#State]]
    }

    def iterations[T](f: StochasticDiffFunction[T], init:T)(implicit arith: MutableCoordinateSpace[T, Double]):Iterator[FirstOrderMinimizer[T, StochasticDiffFunction[T]]#State] = {
      val r = if(useL1) {
        new AdaptiveGradientDescent.L1Regularization[T](regularization, eta=alpha, maxIter = maxIterations)(arith)
      } else { // L2
        new AdaptiveGradientDescent.L2Regularization[T](regularization, alpha,  maxIterations)(arith)
      }
      r.iterations(f,init)
    }

    def iterations[T](f: DiffFunction[T], init:T)(implicit vspace: MutableCoordinateSpace[T, Double]): Iterator[LBFGS[T]#State] = {
       if(useL1) new OWLQN[T](maxIterations, 5, regularization, tolerance)(vspace).iterations(f,init)
      else (new LBFGS[T](maxIterations, 5, tolerance=tolerance)(vspace)).iterations(DiffFunction.withL2Regularization(f,regularization),init)
    }
  }
}
