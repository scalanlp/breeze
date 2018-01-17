package breeze.optimize

import breeze.linalg.norm
import breeze.math.{MutableEnumeratedCoordinateField, MutableFiniteCoordinateField, NormedModule}
import breeze.optimize.FirstOrderMinimizer.ConvergenceReason
import breeze.stats.distributions.{RandBasis, ThreadLocalRandomGenerator}
import breeze.util.Implicits._
import breeze.util.SerializableLogging
import org.apache.commons.math3.random.MersenneTwister
import FirstOrderMinimizer.ConvergenceCheck

/**
 *
 * @author dlwh
 */
abstract class FirstOrderMinimizer[T, DF <: StochasticDiffFunction[T]](val convergenceCheck: ConvergenceCheck[T])(
    implicit space: NormedModule[T, Double])
    extends Minimizer[T, DF]
    with SerializableLogging {

  def this(maxIter: Int = -1, tolerance: Double = 1E-6, fvalMemory: Int = 100, relativeTolerance: Boolean = true)(
      implicit space: NormedModule[T, Double]) =
    this(FirstOrderMinimizer.defaultConvergenceCheck[T](maxIter, tolerance, relativeTolerance, fvalMemory))

  /**
   * Any history the derived minimization function needs to do its updates. typically an approximation
   * to the second derivative/hessian matrix.
   */
  type History
  type State = FirstOrderMinimizer.State[T, convergenceCheck.Info, History]

  import space.normImpl

  protected def initialHistory(f: DF, init: T): History
  protected def adjustFunction(f: DF): DF = f
  protected def adjust(newX: T, newGrad: T, newVal: Double): (Double, T) = (newVal, newGrad)
  protected def chooseDescentDirection(state: State, f: DF): T
  protected def determineStepSize(state: State, f: DF, direction: T): Double
  protected def takeStep(state: State, dir: T, stepSize: Double): T
  protected def updateHistory(newX: T, newGrad: T, newVal: Double, f: DF, oldState: State): History

  protected def initialState(f: DF, init: T): State = {
    val x = init
    val history = initialHistory(f, init)
    val (value, grad) = calculateObjective(f, x, history)
    val (adjValue, adjGrad) = adjust(x, grad, value)
    FirstOrderMinimizer.State(x, value, grad, adjValue, adjGrad, 0, adjValue, history, convergenceCheck.initialInfo)
  }

  protected def calculateObjective(f: DF, x: T, history: History): (Double, T) = {
    f.calculate(x)
  }

  def infiniteIterations(f: DF, state: State): Iterator[State] = {
    var failedOnce = false
    val adjustedFun = adjustFunction(f)

    Iterator.iterate(state) { state =>
      try {
        val dir = chooseDescentDirection(state, adjustedFun)
        val stepSize = determineStepSize(state, adjustedFun, dir)
        logger.info(f"Step Size: $stepSize%.4g")
        val x = takeStep(state, dir, stepSize)
        val (value, grad) = calculateObjective(adjustedFun, x, state.history)
        val (adjValue, adjGrad) = adjust(x, grad, value)
        val oneOffImprovement = (state.adjustedValue - adjValue) / (state.adjustedValue.abs
          .max(adjValue.abs)
          .max(1E-6 * state.initialAdjVal.abs))
        logger.info(f"Val and Grad Norm: $adjValue%.6g (rel: $oneOffImprovement%.3g) ${norm(adjGrad)}%.6g")
        val history = updateHistory(x, grad, value, adjustedFun, state)
        val newCInfo = convergenceCheck.update(x, grad, value, state, state.convergenceInfo)
        failedOnce = false
        FirstOrderMinimizer.State(
          x,
          value,
          grad,
          adjValue,
          adjGrad,
          state.iter + 1,
          state.initialAdjVal,
          history,
          newCInfo)
      } catch {
        case x: FirstOrderException if !failedOnce =>
          failedOnce = true
          logger.error("Failure! Resetting history: " + x)
          state.copy(history = initialHistory(adjustedFun, state.x))
        case x: FirstOrderException =>
          logger.error("Failure again! Giving up and returning. Maybe the objective is just poorly behaved?")
          state.copy(searchFailed = true)
      }
    }
  }

  def iterations(f: DF, init: T): Iterator[State] = {
    val adjustedFun = adjustFunction(f)
    infiniteIterations(f, initialState(adjustedFun, init)).takeUpToWhere { s =>
      convergenceCheck.apply(s, s.convergenceInfo) match {
        case Some(converged) =>
          logger.info(s"Converged because ${converged.reason}")
          s.convergenceReason = Some(converged)
          true
        case None =>
          false
      }
    }
  }

  def minimize(f: DF, init: T): T = {
    minimizeAndReturnState(f, init).x
  }

  def minimizeAndReturnState(f: DF, init: T): State = {
    iterations(f, init).last
  }
}

sealed class FirstOrderException(msg: String = "") extends RuntimeException(msg)
class NaNHistory extends FirstOrderException
class StepSizeUnderflow extends FirstOrderException
class StepSizeOverflow extends FirstOrderException
class LineSearchFailed(gradNorm: Double, dirNorm: Double)
    extends FirstOrderException("Grad norm: %.4f Dir Norm: %.4f".format(gradNorm, dirNorm))

object FirstOrderMinimizer {

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
   * @param searchFailed did the line search fail?
   * @param convergenceReason the convergence reason
   */
  case class State[+T, +ConvergenceInfo, +History](
      x: T,
      value: Double,
      grad: T,
      adjustedValue: Double,
      adjustedGradient: T,
      iter: Int,
      initialAdjVal: Double,
      history: History,
      convergenceInfo: ConvergenceInfo,
      searchFailed: Boolean = false,
      var convergenceReason: Option[ConvergenceReason] = None) {}

  trait ConvergenceCheck[T] {
    type Info
    def initialInfo: Info
    def apply(state: State[T, _, _], info: Info): Option[ConvergenceReason]
    def update(newX: T, newGrad: T, newVal: Double, oldState: State[T, _, _], oldInfo: Info): Info
    def ||(otherCheck: ConvergenceCheck[T]): ConvergenceCheck[T] = orElse(otherCheck)

    def orElse(other: ConvergenceCheck[T]): ConvergenceCheck[T] = {
      SequenceConvergenceCheck(asChecks ++ other.asChecks)
    }

    protected def asChecks: IndexedSeq[ConvergenceCheck[T]] = IndexedSeq(this)
  }

  object ConvergenceCheck {
    implicit def fromPartialFunction[T](pf: PartialFunction[State[T, _, _], ConvergenceReason]): ConvergenceCheck[T] =
      new ConvergenceCheck[T] {
        override type Info = Unit

        def update(newX: T, newGrad: T, newVal: Double, oldState: State[T, _, _], oldInfo: Info): Info = oldInfo

        override def apply(state: State[T, _, _], info: Info): Option[ConvergenceReason] = pf.lift(state)

        override def initialInfo: Info = ()
      }
  }

  case class SequenceConvergenceCheck[T](checks: IndexedSeq[ConvergenceCheck[T]]) extends ConvergenceCheck[T] {
    type Info = IndexedSeq[ConvergenceCheck[T]#Info]

    override def initialInfo: IndexedSeq[ConvergenceCheck[T]#Info] = checks.map(_.initialInfo)

    override def update(newX: T, newGrad: T, newVal: Double, oldState: State[T, _, _], oldInfo: Info): Info = {
      require(oldInfo.length == checks.length)
      checks.zip(oldInfo).map { case (c, i) => c.update(newX, newGrad, newVal, oldState, i.asInstanceOf[c.Info]) }
    }

    override def apply(state: State[T, _, _], info: IndexedSeq[ConvergenceCheck[T]#Info]): Option[ConvergenceReason] = {
      checks.zip(info).iterator.flatMap { case (c, i) => c(state, i.asInstanceOf[c.Info]) }.toStream.headOption
    }
  }

  trait ConvergenceReason {
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

  case object MonitorFunctionNotImproving extends ConvergenceReason {
    override def reason: String = "monitor function is not improving"
  }

  case object ProjectedStepConverged extends ConvergenceReason {
    override def reason: String = "projected step converged"
  }

  def maxIterationsReached[T](maxIter: Int): ConvergenceCheck[T] = ConvergenceCheck.fromPartialFunction {
    case s: State[_, _, _] if (s.iter >= maxIter && maxIter >= 0) =>
      MaxIterations
  }

  def functionValuesConverged[T](
      tolerance: Double = 1E-9,
      relative: Boolean = true,
      historyLength: Int = 10): ConvergenceCheck[T] = {
    new FunctionValuesConverged[T](tolerance, relative, historyLength)
  }

  case class FunctionValuesConverged[T](tolerance: Double, relative: Boolean, historyLength: Int)
      extends ConvergenceCheck[T] {
    override type Info = IndexedSeq[Double]

    override def update(newX: T, newGrad: T, newVal: Double, oldState: State[T, _, _], oldInfo: Info): Info = {
      (oldInfo :+ newVal).takeRight(historyLength)
    }

    override def apply(state: State[T, _, _], info: IndexedSeq[Double]): Option[ConvergenceReason] = {
      if (info.length >= 2 && (state.adjustedValue - info.max).abs <= tolerance * (if (relative) state.initialAdjVal.abs
                                                                                   else 1.0)) {
        Some(FunctionValuesConverged)
      } else {
        None
      }
    }

    override def initialInfo: Info = IndexedSeq(Double.PositiveInfinity)
  }

  def gradientConverged[T](tolerance: Double, relative: Boolean = true)(
      implicit space: NormedModule[T, Double]): ConvergenceCheck[T] = {
    import space.normImpl
    ConvergenceCheck.fromPartialFunction[T] {
      case s: State[T, _, _]
          if (norm(s.adjustedGradient) <= math.max(tolerance * (if (relative) s.adjustedValue else 1.0), 1E-8)) =>
        GradientConverged
    }
  }

  def searchFailed[T]: ConvergenceCheck[T] = ConvergenceCheck.fromPartialFunction {
    case s: State[_, _, _] if (s.searchFailed) =>
      SearchFailed
  }

  /**
   * Runs the function, and if it fails to decreased by at least improvementRequirement numFailures times in a row,
   * then we abort
   * @param f
   * @param numFailures
   * @param evalFrequency how often we run the evaluation
   * @tparam T
   */
  def monitorFunctionValues[T](
      f: T => Double,
      numFailures: Int = 5,
      improvementRequirement: Double = 1E-2,
      evalFrequency: Int = 10): ConvergenceCheck[T] =
    new MonitorFunctionValuesCheck(f, numFailures, improvementRequirement, evalFrequency)

  case class MonitorFunctionValuesCheck[T](
      f: T => Double,
      numFailures: Int,
      improvementRequirement: Double,
      evalFrequency: Int)
      extends ConvergenceCheck[T]
      with SerializableLogging {
    case class Info(bestValue: Double, numFailures: Int)

    override def update(newX: T, newGrad: T, newVal: Double, oldState: State[T, _, _], oldInfo: Info): Info = {
      if (oldState.iter % evalFrequency == 0) {
        val newValue = f(newX)
        if (newValue <= oldInfo.bestValue * (1 - improvementRequirement)) {
          logger.info(f"External function improved: current ${newValue}%.3f old: ${oldInfo.bestValue}%.3f")
          Info(numFailures = 0, bestValue = newValue)
        } else {
          logger.info(
            f"External function failed to improve sufficiently! current ${newValue}%.3f old: ${oldInfo.bestValue}%.3f")
          oldInfo.copy(numFailures = oldInfo.numFailures + 1)
        }
      } else {
        oldInfo
      }
    }

    override def apply(state: State[T, _, _], info: Info): Option[ConvergenceReason] = {
      if (info.numFailures >= numFailures) {
        Some(MonitorFunctionNotImproving)
      } else {
        None
      }
    }

    override def initialInfo: Info = Info(Double.PositiveInfinity, 0)
  }

  def defaultConvergenceCheck[T](maxIter: Int, tolerance: Double, relative: Boolean = true, fvalMemory: Int = 20)(
      implicit space: NormedModule[T, Double]): ConvergenceCheck[T] =
    (
      maxIterationsReached[T](maxIter) ||
        functionValuesConverged(tolerance, relative, fvalMemory) ||
        gradientConverged[T](tolerance, relative) ||
        searchFailed
    )

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
  case class OptParams(
      batchSize: Int = 512,
      regularization: Double = 0.0,
      alpha: Double = 0.5,
      maxIterations: Int = 1000,
      useL1: Boolean = false,
      tolerance: Double = 1E-5,
      useStochastic: Boolean = false,
      randomSeed: Int = 0) {
    private implicit val random = new RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister(randomSeed)))

    @deprecated("Use breeze.optimize.minimize(f, init, params) instead.", "0.10")
    def minimize[T](f: BatchDiffFunction[T], init: T)(implicit space: MutableFiniteCoordinateField[T, _, Double]): T = {
      this.iterations(f, init).last.x
    }

    @deprecated("Use breeze.optimize.minimize(f, init, params) instead.", "0.10")
    def minimize[T](f: DiffFunction[T], init: T)(implicit space: MutableEnumeratedCoordinateField[T, _, Double]): T = {
      this.iterations(f, init).last.x
    }

    @deprecated("Use breeze.optimize.iterations(f, init, params) instead.", "0.10")
    def iterations[T](f: BatchDiffFunction[T], init: T)(implicit space: MutableFiniteCoordinateField[T, _, Double])
      : Iterator[FirstOrderMinimizer[T, BatchDiffFunction[T]]#State] = {
      val it = if (useStochastic) {
        this.iterations(f.withRandomBatches(batchSize), init)(space)
      } else {
        iterations(f: DiffFunction[T], init)
      }

      it.asInstanceOf[Iterator[FirstOrderMinimizer[T, BatchDiffFunction[T]]#State]]
    }

    @deprecated("Use breeze.optimize.iterations(f, init, params) instead.", "0.10")
    def iterations[T](f: StochasticDiffFunction[T], init: T)(implicit space: MutableFiniteCoordinateField[T, _, Double])
      : Iterator[FirstOrderMinimizer[T, StochasticDiffFunction[T]]#State] = {
      val r = if (useL1) {
        new AdaptiveGradientDescent.L1Regularization[T](regularization, eta = alpha, maxIter = maxIterations)(
          space,
          random)
      } else { // L2
        new AdaptiveGradientDescent.L2Regularization[T](regularization, alpha, maxIterations)(space, random)
      }
      r.iterations(f, init)
    }

    @deprecated("Use breeze.optimize.iterations(f, init, params) instead.", "0.10")
    def iterations[T, K](f: DiffFunction[T], init: T)(
        implicit space: MutableEnumeratedCoordinateField[T, K, Double]): Iterator[LBFGS[T]#State] = {
      if (useL1) new OWLQN[K, T](maxIterations, 5, regularization, tolerance)(space).iterations(f, init)
      else
        (new LBFGS[T](maxIterations, 5, tolerance = tolerance)(space))
          .iterations(DiffFunction.withL2Regularization(f, regularization), init)
    }
  }
}
