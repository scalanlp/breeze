package breeze.optimize

import breeze.math.{InnerProductModule, MutableInnerProductModule, MutableVectorField}
import breeze.linalg.{*, clip, norm}
import breeze.util.SerializableLogging


/**
 * SPG is a Spectral Projected Gradient minimizer; it minimizes a differentiable
 * function subject to the optimum being in some set, given by the projection operator  projection
 * @tparam T vector type
 * @param optTol termination criterion: tolerance for norm of projected gradient
 * @param gamma  sufficient decrease parameter
 * @param M number of history entries for linesearch
 * @param alphaMax longest step
 * @param alphaMin shortest step
 * @param maxNumIt maximum number of iterations
 * @param initFeas is the initial guess feasible, or should it be projected?
 * @param maxSrchIt maximum number of line search attempts
 * @param projection projection operations
 */
class SpectralProjectedGradient[T](
  val projection: T => T = { (t: T) => t },
  tolerance: Double = 1e-6,
  suffDec: Double = 1e-4,
  minImprovementWindow: Int = 30,
  alphaMax: Double = 1e10,
  alphaMin: Double = 1e-10,
  fMemory: Int = 10,
  maxIter: Int = -1,
  val initFeas: Boolean = false,
  val maxSrchIt: Int = 30)(implicit space: MutableVectorField[T, Double]) extends FirstOrderMinimizer[T, DiffFunction[T]](minImprovementWindow = minImprovementWindow, maxIter = maxIter, tolerance = tolerance, improvementTol = tolerance) with Projecting[T] with SerializableLogging {
  import space._
  case class History(alphaBB: Double, fvals: IndexedSeq[Double])

  override protected def initialHistory(f: DiffFunction[T], init: T): History = {
    History(0.1, IndexedSeq.empty)
  }

  override protected def updateHistory(newX: T, newGrad: T, newVal: Double, f: DiffFunction[T], oldState: State): History = {
    val s = newX - oldState.x
    val y = newGrad - oldState.grad
    var alpha = (y dot y) / (s dot y)
    if(alpha.isNaN) {
      alpha = 1.0
    }
    History(alpha, (newVal +: oldState.history.fvals).take(fMemory))
  }


  override protected def takeStep(state: State, dir: T, stepSize: Double): T = {
    val qq = projection(state.x + dir * stepSize)
    assert(projection(qq) == qq)
    qq
  }

  override protected def chooseDescentDirection(state: State, f: DiffFunction[T]): T = {
    projection(state.x - state.grad * state.history.alphaBB) - state.x
  }

  override protected def determineStepSize(state: State, f: DiffFunction[T], direction: T): Double = {
    val fb = if(state.history.fvals.isEmpty) state.value else state.value max state.history.fvals.max
    val searchFun = functionFromSearchDirection(f, state.x, direction, projection)
    var normGradInDir: Double = state.grad dot direction
    if(normGradInDir > 0) {
      direction *= -1.0
      normGradInDir *= -1.0
    }
    var alpha = 1.0
    var fVal = searchFun(alpha)
    while (fVal > fb + alpha * (normGradInDir * suffDec) && alpha > 1E-10) {
      alpha *= 0.5
      fVal = searchFun(alpha)
    }

    if(alpha < 1E-10) {
      throw new LineSearchFailed(normGradInDir, norm(direction))
    }

    alpha
  }

  // because of the projection, we have to do our own verstion
  private def functionFromSearchDirection[T, I](f: DiffFunction[T], x: T, direction: T, project: T=>T)(implicit prod: InnerProductModule[T, Double]):DiffFunction[Double] = new DiffFunction[Double] {
    import prod._

    /** calculates the value at a point */
    override def valueAt(alpha: Double): Double = f.valueAt(project(x + direction * alpha))

    /** calculates the gradient at a point */
    override def gradientAt(alpha: Double): Double = f.gradientAt(project(x + direction * alpha)) dot direction

    /** Calculates both the value and the gradient at a point */
    def calculate(alpha: Double): (Double, Double) = {
      val (ff, grad) = f.calculate(x + direction * alpha)
      ff -> (grad dot direction)
    }
  }
}
