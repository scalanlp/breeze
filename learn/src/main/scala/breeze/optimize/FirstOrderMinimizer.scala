package breeze.optimize

import breeze.util.logging.{ConsoleLogging, Logged}
import breeze.math.{NormedVectorSpace, MutableCoordinateSpace}

/**
 * 
 * @author dlwh
 */

abstract class FirstOrderMinimizer[T,-DF<:StochasticDiffFunction[T]](maxIter: Int = -1, tolerance: Double=1E-5)(implicit vspace: NormedVectorSpace[T, Double]) extends Minimizer[T,DF] with Logged {

  type History
  case class State(x: T,
                   value: Double, grad: T,
                   adjustedValue: Double, adjustedGradient: T,
                   iter: Int,
                   initialAdjVal: Double,
                   history: History) {
  }

  protected def initialHistory(f: DF, init: T): History
  protected def adjust(newX: T, newGrad: T, newVal: Double):(Double,T) = (newVal,newGrad)
  protected def chooseDescentDirection(state: State):T
  protected def determineStepSize(state: State, f: DF, direction: T):Double
  protected def takeStep(state: State, dir: T, stepSize:Double):T
  protected def updateHistory(newX: T, newGrad: T, newVal: Double, oldState: State):History

  protected def initialState(f: DF, init: T) = {
    val x = init
    val (value,grad) = f.calculate(x)
    val (adjValue,adjGrad) = adjust(x,grad,value)
    val history = initialHistory(f,init)
    State(x,value,grad,adjValue,adjGrad,0,adjValue,history)
  }

  def iterations(f: DF,init: T): Iterator[State] = {
    var failedOnce = false
    val it = Iterator.iterate(initialState(f,init)) { state => try {
        val dir = chooseDescentDirection(state)
        val stepSize = determineStepSize(state, f, dir)
        log.info("Step Size:" + stepSize)
        val x = takeStep(state,dir,stepSize)
        val (value,grad) = f.calculate(x)
        log.info("Val and Grad Norm:" + value + " " + vspace.norm(grad))
        val (adjValue,adjGrad) = adjust(x,grad,value)
        log.info("Adj Val and Grad Norm:" + adjValue + " " + vspace.norm(adjGrad))
        val history = updateHistory(x,grad,value,state)
        failedOnce = false
        State(x,value,grad,adjValue,adjGrad,state.iter + 1, state.initialAdjVal, history)
    } catch {
      case x: FirstOrderException if !failedOnce =>
        failedOnce = true
        log.error("Failure! Resetting history: " + x)
        state.copy(history = initialHistory(f,state.x))
    }
    }
    it:Iterator[State]
  }

  def minimize(f: DF, init: T):T = {
    var initialFVal: Option[Double] = None;
    iterations(f,init).find (state =>
      (state.iter >= maxIter && maxIter >= 0)
        || (vspace.norm(state.adjustedGradient) <= math.max(tolerance * state.initialAdjVal,1E-8))
    ).get.x
  }
}

sealed class FirstOrderException(msg: String="") extends RuntimeException(msg)
class NaNHistory extends FirstOrderException
class StepSizeUnderflow extends FirstOrderException
class LineSearchFailed(gradNorm: Double, dirNorm: Double) extends FirstOrderException("Grad norm: %.4f Dir Norm: %.4f".format(gradNorm, dirNorm))

object FirstOrderMinimizer {
  case class OptParams(batchSize:Int = 512,
                       regularization: Double = 1.0,
                       alpha: Double = 0.5,
                       maxIterations:Int = 1000,
                       useL1: Boolean = false,
                       tolerance:Double = 1E-4,
                       useStochastic: Boolean= false) {
    def minimize[T](f: BatchDiffFunction[T], init: T)(implicit arith: MutableCoordinateSpace[T, Double]) = {
      this.iterations(f, init).find{state =>
            ((state.iter >= maxIterations && maxIterations >= 0)
              || arith.norm(state.adjustedGradient) <= math.max(tolerance * state.initialAdjVal.abs,1E-8))
          }.get.x
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
      val r = if(regularization == 0.0) {
        new StochasticGradientDescent.SimpleSGD[T](alpha, maxIterations)(arith) {
        }
      } else if(useL1) {
        new AdaptiveGradientDescent.L1Regularization[T](regularization, eta=alpha, maxIter = maxIterations)(arith) {
        }
      } else { // L2
        new StochasticGradientDescent[T](alpha,  maxIterations)(arith) with AdaptiveGradientDescent.L2Regularization[T] {
          override val lambda = regularization
        }
      }
      r.iterations(f,init)
    }

    def iterations[T](f: DiffFunction[T], init:T)(implicit vspace: MutableCoordinateSpace[T, Double]): Iterator[LBFGS[T]#State] = {
       if(useL1) new OWLQN[T](maxIterations, 5, regularization)(vspace).iterations(f,init)
      else (new LBFGS[T](maxIterations, 5)(vspace)).iterations(DiffFunction.withL2Regularization(f,regularization),init)
    }
  }
}
