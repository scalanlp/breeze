package breeze.optimize

import breeze.linalg.norm
import breeze.math.{MutableCoordinateField, MutableFiniteCoordinateField, NormedModule}
import breeze.optimize.FirstOrderMinimizer.ConvergenceReason
import breeze.stats.distributions.{RandBasis, ThreadLocalRandomGenerator}
import breeze.util.Implicits._
import breeze.util.SerializableLogging
import org.apache.commons.math3.random.MersenneTwister

/**
 *
 * @param minImprovementWindow How many iterations to improve function by at least improvementTol
 * @author dlwh
 */
abstract class FirstOrderMinimizer[T, DF<:StochasticDiffFunction[T]](maxIter: Int = -1,
                                                                     tolerance: Double=1E-6,
                                                                     improvementTol: Double=1E-3,
                                                                     val minImprovementWindow: Int = 10,
                                                                     val numberOfImprovementFailures: Int = 1)(implicit space: NormedModule[T, Double]) extends Minimizer[T,DF] with SerializableLogging {

  import space.normImpl

  /**
    * Any history the derived minimization function needs to do its updates. typically an approximation
   * to the second derivative/hessian matrix.
    */
  type History

  /**
   * Tracks the information about the optimizer, including the current point, its value, gradient, and then any history.
   * Also includes information for checking convergence.
   * @param x the current point being considered
   * @param value f(x)
   * @param grad f.gradientAt(x)
   * @param adjustedValue  f(x) + r(x), where r is any regularization added to the objective. For LBFGS, this is f(x).
   * @param adjustedGradient f'(x) + r'(x), where r is any regularization added to the objective. For LBFGS, this is f'(x).
   * @param iter what iteration number we are on.
   * @param initialAdjVal f(x_0) + r(x_0), used for checking convergence
   * @param history any information needed by the optimizer to do updates.
   * @param fVals the sequence of the last minImprovementWindow values, used for checking if the "value" isn't improving
   * @param numImprovementFailures the number of times in a row the objective hasn't improved, mostly for SGD
   * @param searchFailed did the line search fail?
   */
  case class State(x: T,
                   value: Double, grad: T,
                   adjustedValue: Double, adjustedGradient: T,
                   iter: Int,
                   initialAdjVal: Double,
                   history: History,
                   fVals: IndexedSeq[Double] = Vector(Double.PositiveInfinity),
                   numImprovementFailures: Int = 0,
                   searchFailed: Boolean = false) {

    def convergedReason:Option[ConvergenceReason] = {
      if (iter >= maxIter && maxIter >= 0)
        Some(FirstOrderMinimizer.MaxIterations)
      else if (!fVals.isEmpty && (adjustedValue - fVals.max).abs <= tolerance * initialAdjVal)
        Some(FirstOrderMinimizer.FunctionValuesConverged)
      else if (numImprovementFailures >= numberOfImprovementFailures)
        Some(FirstOrderMinimizer.ObjectiveNotImproving)
      else if (norm(adjustedGradient) <= math.max(tolerance * adjustedValue.abs, 1E-8))
        Some(FirstOrderMinimizer.GradientConverged)
      else if (searchFailed)
        Some(FirstOrderMinimizer.SearchFailed)
      else
        None
    }

    /** True if the optimizer thinks it's done. */
    def converged = convergedReason.nonEmpty

    /**
     * true if the function value hasn't changed for several iterations or if the gradient's norm is near 0
     * @return
     */
    def actuallyConverged = (
      convergedReason == Some(FirstOrderMinimizer.FunctionValuesConverged)
      || convergedReason == Some(FirstOrderMinimizer.GradientConverged)
    )
  }


  protected def initialHistory(f: DF, init: T): History
  protected def adjustFunction(f: DF): DF = f
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
    val adjustedFun = adjustFunction(f)
    val it = Iterator.iterate(initialState(adjustedFun,init)) { state => try {
        val dir = chooseDescentDirection(state, adjustedFun)
        val stepSize = determineStepSize(state, adjustedFun, dir)
        logger.info(f"Step Size: $stepSize%.4g")
        val x = takeStep(state,dir,stepSize)
        val (value,grad) = calculateObjective(adjustedFun, x, state.history)
        val (adjValue,adjGrad) = adjust(x,grad,value)
        val oneOffImprovement = (state.adjustedValue - adjValue)/(state.adjustedValue.abs max adjValue.abs max 1E-6 * state.initialAdjVal.abs)
        logger.info(f"Val and Grad Norm: $adjValue%.6g (rel: $oneOffImprovement%.3g) ${norm(adjGrad)}%.6g")
        val history = updateHistory(x,grad,value, adjustedFun, state)
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
          state.copy(history = initialHistory(adjustedFun, state.x))
        case x: FirstOrderException =>
          logger.error("Failure again! Giving up and returning. Maybe the objective is just poorly behaved?")
          state.copy(searchFailed = true)
      }
    }.takeUpToWhere(_.converged)
    it: Iterator[State]
  }

  def minimize(f: DF, init: T): T = {
    minimizeAndReturnState(f, init).x
  }


  def minimizeAndReturnState(f: DF, init: T):State = {
    iterations(f, init).last
  }
}

sealed class FirstOrderException(msg: String="") extends RuntimeException(msg)
class NaNHistory extends FirstOrderException
class StepSizeUnderflow extends FirstOrderException
class StepSizeOverflow extends FirstOrderException
class LineSearchFailed(gradNorm: Double, dirNorm: Double) extends FirstOrderException("Grad norm: %.4f Dir Norm: %.4f".format(gradNorm, dirNorm))

object FirstOrderMinimizer {


  sealed trait ConvergenceReason {
    def reason: String
  }
  case object MaxIterations extends ConvergenceReason {
    override def reason: String = "max iterations reached"
  }
  case object FunctionValuesConverged extends ConvergenceReason {
    override def reason: String = "function values converged"
  }
  case object GradientConverged extends ConvergenceReason {
    override def reason: String = "gradient converged"
  }
  case object SearchFailed extends ConvergenceReason {
    override def reason: String = "line search failed!"
  }
  case object ObjectiveNotImproving extends ConvergenceReason {
    override def reason: String = "objective is not improving"
  }



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
                       useStochastic: Boolean= false,
                       randomSeed: Int = 0) {
    private implicit val random = new RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister(randomSeed)))

    @deprecated("Use breeze.optimize.minimize(f, init, params) instead.", "0.10")
    def minimize[T](f: BatchDiffFunction[T], init: T)(implicit space: MutableFiniteCoordinateField[T, _, Double]): T = {
      this.iterations(f, init).last.x
    }

    @deprecated("Use breeze.optimize.minimize(f, init, params) instead.", "0.10")
    def minimize[T](f: DiffFunction[T], init: T)(implicit space: MutableCoordinateField[T, Double]): T = {
      this.iterations(f, init).last.x
    }

    @deprecated("Use breeze.optimize.iterations(f, init, params) instead.", "0.10")
    def iterations[T](f: BatchDiffFunction[T], init: T)(implicit space: MutableFiniteCoordinateField[T, _, Double]): Iterator[FirstOrderMinimizer[T, BatchDiffFunction[T]]#State] = {
      val it = if(useStochastic) {
         this.iterations(f.withRandomBatches(batchSize), init)(space)
      } else {
        iterations(f:DiffFunction[T], init)
      }

      it.asInstanceOf[Iterator[FirstOrderMinimizer[T, BatchDiffFunction[T]]#State]]
    }

    @deprecated("Use breeze.optimize.iterations(f, init, params) instead.", "0.10")
    def iterations[T](f: StochasticDiffFunction[T], init:T)(implicit space: MutableFiniteCoordinateField[T, _, Double]):Iterator[FirstOrderMinimizer[T, StochasticDiffFunction[T]]#State] = {
      val r = if(useL1) {
        new AdaptiveGradientDescent.L1Regularization[T](regularization, eta=alpha, maxIter = maxIterations)(space, random)
      } else { // L2
        new AdaptiveGradientDescent.L2Regularization[T](regularization, alpha,  maxIterations)(space, random)
      }
      r.iterations(f,init)
    }

    @deprecated("Use breeze.optimize.iterations(f, init, params) instead.", "0.10")
    def iterations[T](f: DiffFunction[T], init:T)(implicit space: MutableCoordinateField[T, Double]): Iterator[LBFGS[T]#State] = {
       if(useL1) new OWLQN[T](maxIterations, 5, regularization, tolerance)(space).iterations(f,init)
      else (new LBFGS[T](maxIterations, 5, tolerance=tolerance)(space)).iterations(DiffFunction.withL2Regularization(f,regularization),init)
    }
  }
}
