package breeze.optimize

/*
 Copyright 2015 David Hall, Daniel Ramage, Debasish Das

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

import breeze.math.{InnerProductModule, MutableVectorField}
import breeze.linalg.norm
import breeze.util.SerializableLogging

/**
 * SPG is a Spectral Projected Gradient minimizer; it minimizes a differentiable
 * function subject to the optimum being in some set, given by the projection operator projection
 * @tparam T vector type
 * @param tolerance termination criterion: tolerance for norm of projected gradient
 * @param suffDec  sufficient decrease parameter
 * @param bbMemory number of history entries for linesearch
 * @param alphaMax longest step
 * @param alphaMin shortest step
 * @param maxIter maximum number of iterations
 * @param maxSrcht maximum number of iterations inside line search
 * @param initFeas is the initial guess feasible, or should it be projected?
 * @param projection projection operations
 * @param curvilinear if curvilinear true, do the projection inside line search in place of doing it in chooseDescentDirection
 */
class SpectralProjectedGradient[T](val projection: T => T = { (t: T) =>
  t
}, tolerance: Double = 1e-6, suffDec: Double = 1e-4, fvalMemory: Int = 30, alphaMax: Double = 1e10, alphaMin: Double = 1e-10, bbMemory: Int = 10, maxIter: Int = -1, val initFeas: Boolean = false, val curvilinear: Boolean = false, val bbType: Int = 1, val maxSrcht: Int = 30)(
    implicit space: MutableVectorField[T, Double])
    extends FirstOrderMinimizer[T, DiffFunction[T]](fvalMemory = fvalMemory, maxIter = maxIter, tolerance = tolerance)
    with Projecting[T]
    with SerializableLogging {
  import space._
  case class History(alphaBB: Double, fvals: IndexedSeq[Double])

  override protected def initialHistory(f: DiffFunction[T], init: T): History = {
    History(0.1, IndexedSeq.empty)
  }

  /**
   * From Mark Schmidt's Matlab code
   * if bbType == 1
   *  alpha = (s'*s)/(s'*y);
   * else
   *  alpha = (s'*y)/(y'*y);
   */
  protected def bbAlpha(s: T, y: T): Double = {
    var alpha =
      if (bbType == 1)(s.dot(s)) / (s.dot(y))
      else (s.dot(y)) / (y.dot(y))
    if (alpha <= alphaMin || alpha > alphaMax) alpha = 1.0
    if (alpha.isNaN) alpha = 1.0
    alpha
  }

  override protected def updateHistory(
      newX: T,
      newGrad: T,
      newVal: Double,
      f: DiffFunction[T],
      oldState: State): History = {
    val s = newX - oldState.x
    val y = newGrad - oldState.grad
    History(bbAlpha(s, y), (newVal +: oldState.history.fvals).take(bbMemory))
  }

  override protected def takeStep(state: State, dir: T, stepSize: Double): T = {
    val qq = projection(state.x + dir * stepSize)
    assert(projection(qq) == qq)
    qq
  }

  override protected def chooseDescentDirection(state: State, f: DiffFunction[T]): T = {
    if (curvilinear) state.x - state.grad * state.history.alphaBB
    else projection(state.x - state.grad * state.history.alphaBB) - state.x
  }

  override protected def determineStepSize(state: State, f: DiffFunction[T], direction: T): Double = {
    val fb = if (state.history.fvals.isEmpty) state.value else state.value.max(state.history.fvals.max)
    val normGradInDir = state.grad.dot(direction)

    var gamma =
      if (state.iter == 0) scala.math.min(1.0, 1.0 / norm(state.grad))
      else 1.0

    val searchFun =
      if (curvilinear) functionFromSearchDirection(f, state.x, direction, projection)
      else LineSearch.functionFromSearchDirection(f, state.x, direction)

    //TO DO :
    // 1. Add cubic interpolation and see it's performance. Bisection did not work for L1 projection
    val search = new BacktrackingLineSearch(fb, maxIterations = maxSrcht)
    gamma = search.minimize(searchFun, gamma)

    if (gamma < 1e-10) {
      throw new LineSearchFailed(normGradInDir, norm(direction))
    }

    gamma
  }

  // because of the projection, we have to do our own verstion
  private def functionFromSearchDirection[T, I](f: DiffFunction[T], x: T, direction: T, project: T => T)(
      implicit prod: InnerProductModule[T, Double]): DiffFunction[Double] = new DiffFunction[Double] {
    import prod._

    /** calculates the value at a point */
    override def valueAt(alpha: Double): Double = f.valueAt(project(x + direction * alpha))

    /** calculates the gradient at a point */
    override def gradientAt(alpha: Double): Double = f.gradientAt(project(x + direction * alpha)).dot(direction)

    /** Calculates both the value and the gradient at a point */
    def calculate(alpha: Double): (Double, Double) = {
      val (ff, grad) = f.calculate(x + direction * alpha)
      ff -> (grad.dot(direction))
    }
  }
}
