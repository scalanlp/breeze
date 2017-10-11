package breeze.optimize.proximal

/*
 Copyright 2015 Debasish Das

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

import breeze.linalg.{min, DenseVector, norm}
import breeze.math.{InnerProductModule, MutableVectorField}
import breeze.numerics.pow
import breeze.optimize._
import breeze.util.{LazyLogger, SerializableLogging}

/**
  * Spectral Proximal Gradient is an extension to Spectral Projected Gradient minimizer for Proximal Operators;
  * it minimizes a composite function subject to the optimum being in some set, given by the projection operator projection
  * @tparam T vector type
  * @param tolerance termination criterion: tolerance for norm of projected gradient
  * @param suffDec  sufficient decrease parameter
  * @param bbMemory number of history entries for linesearch
  * @param alphaMax longest step
  * @param alphaMin shortest step
  * @param maxIter maximum number of iterations
  * @param maxSrcht maximum number of iterations inside line search
  * @param initFeas is the initial guess feasible, or should it be projected?
  * @param proximal proximal operations
  */
class SpectralProximalGradient(
   val proximal: Proximal,
   tolerance: Double = 1e-6,
   suffDec: Double = 1e-4,
   minImprovementWindow: Int = 30,
   alphaMax: Double = 1e10,
   alphaMin: Double = 1e-10,
   bbMemory: Int = 10,
   maxIter: Int = -1,
   val initFeas: Boolean = false,
   val bbType: Int = 2,
   val maxSrcht: Int = 30) extends FirstOrderMinimizer[DenseVector[Double], DiffFunction[DenseVector[Double]]](minImprovementWindow = minImprovementWindow, maxIter = maxIter, tolerance = tolerance, improvementTol = tolerance) with SerializableLogging {

  import SpectralProximalGradient.BDV

  case class History(alphaBB: Double, fvals: IndexedSeq[Double])


  override protected def initialHistory(f: DiffFunction[BDV], init: BDV): History = {
    History(0.1, IndexedSeq.empty)
  }

  override protected def adjust(newX: BDV, newGrad: BDV, newVal: Double): (Double, BDV) = {
    val xprox = (newX - newGrad).asInstanceOf[BDV]
    proximal.prox(xprox)
    (newVal + proximal.valueAt(xprox), xprox - newX)
  }

  /**
   * From Mark Schmidt's Matlab code
   * if bbType == 1
   * alpha = (s'*s)/(s'*y);
   * else
   * alpha = (s'*y)/(y'*y);
   */
  protected def bbAlpha(newGrad: BDV, s: BDV, y: BDV): Double = {
    var alpha =
      if (bbType == 1) (s dot s) / (s dot y)
      else (s dot y) / (y dot y)
    if (alpha <= alphaMin || alpha > alphaMax) alpha = min(1.0, 1.0 / norm(newGrad))
    if (alpha.isNaN) alpha = 1.0
    alpha
  }

  override protected def updateHistory(newX: BDV, newGrad: BDV, newVal: Double, f: DiffFunction[BDV], oldState: State): History = {
    val s = newX - oldState.x
    val y = newGrad - oldState.grad
    val compositeVal = newVal + proximal.valueAt(newX)
    println(s"History ${oldState.iter} fval $newVal compositeVal $compositeVal")
    History(bbAlpha(newGrad, s, y), (compositeVal +: oldState.history.fvals).take(bbMemory))
  }

  override protected def takeStep(state: State, dir: BDV, stepSize: Double): BDV = {
    val xprox = state.x + dir * stepSize
    proximal.prox(xprox.asInstanceOf[BDV], stepSize)
    xprox
    //val qq = projection(state.x + dir * stepSize)
    //assert(projection(qq) == qq)
    //qq
  }

  override protected def chooseDescentDirection(state: State, f: DiffFunction[BDV]): BDV = {
    val xprox = state.x - state.grad * state.history.alphaBB
    proximal.prox(xprox.asInstanceOf[BDV], state.history.alphaBB)
    xprox - state.x
    //projection(state.x - state.grad * state.history.alphaBB) - state.x
  }

  override protected def determineStepSize(state: State, f: DiffFunction[BDV], direction: BDV): Double = {
    val compositef = state.value + proximal.valueAt(state.x)
    val fb = if (state.history.fvals.isEmpty) compositef else compositef max state.history.fvals.max
    val normGradInDir = state.grad dot direction

    var gamma =
      if (state.iter == 0) min(1.0, 1.0 / norm(state.grad))
      else 1.0

    val searchFun = functionFromSearchDirection(f, state.x, direction, proximal)
    //TO DO :
    // 1. Add cubic interpolation and see it's performance. Bisection did not work for L1 projection
    val search = new BacktrackingLineSearch(fb, maxIterations = maxSrcht)
    gamma = search.minimize(searchFun, gamma)

    if (gamma < 1e-10) {
      throw new LineSearchFailed(normGradInDir, norm(direction))
    }
    gamma
  }

  // because of the projection, we have to do our own version
  private def functionFromSearchDirection(f: DiffFunction[BDV], x: BDV, direction: BDV, proximal: Proximal)(implicit prod: InnerProductModule[BDV, Double]): DiffFunction[Double] = new DiffFunction[Double] {

    import prod._

    /** calculates the value at a point */
    override def valueAt(alpha: Double): Double = {
      val xprox = x + direction * alpha
      val primalf = f.valueAt(xprox)
      val compositef = primalf + proximal.valueAt(xprox)
      compositef
    }

    /** calculates the gradient at a point */
    override def gradientAt(alpha: Double): Double = {
      f.gradientAt(x + direction * alpha) dot direction
    }

    /** Calculates both the value and the gradient at a point */
    def calculate(alpha: Double): (Double, Double) = {
      val xprox = x + direction * alpha
      val (ff, grad) = f.calculate(xprox)
      val compositef = ff + proximal.valueAt(xprox)
      compositef -> (grad dot direction)
    }
  }
}

object SpectralProximalGradient {
  type BDV = DenseVector[Double]

  def curvtrack(f: DiffFunction[BDV], proximal: Proximal,
                                          x: BDV,
                                          grad: BDV,
                                          direction: BDV,
                                          t: Double,
                                          fOld: Double,
                                          compyOld: Double,
                                          gamma: Double,
                                          maxSrchIt: Int,
                                          testOpt: Boolean,
                                          logger: LazyLogger) = {
    var accepted = false
    var srchit = 0
    var lambda = t

    do {
      val y = x + direction * lambda
      val fy = f.calculate(y)._1

      val gy = proximal.valueAt(y.asInstanceOf[DenseVector[Double]])
      proximal.prox(y.asInstanceOf[DenseVector[Double]], lambda)

      val compy = fy + gy

      val desc = 0.5 * pow(norm(y - x), 2)

      if (testOpt && srchit > 0) {
        logger.debug(f"SpectralProximalGradient: SrchIt $srchit%4d: f $compy%-10.4f t $lambda%-10.4f\n")
      }

      if (compy < compyOld + gamma * lambda * desc) {
        accepted = true
      } else if (srchit >= maxSrchIt) {
        accepted = true
      } else {
        lambda *= 0.5
        srchit = srchit + 1
      }
    } while (!accepted)

    if (srchit >= maxSrchIt) {
      logger.info("SpectralProximalGradient: Line search cannot make further progress")
      throw new LineSearchFailed(norm(grad), norm(direction))
    }
    lambda
  }
}
