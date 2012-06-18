package breeze.optimize

/*
 Copyright 2009 David Hall, Daniel Ramage
 
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

import breeze.util.logging._
import breeze.math.MutableCoordinateSpace
import breeze.linalg._


/**
 * Port of LBFGS to Scala.
 * 
 * Special note for LBFGS:
 *  If you use it in published work, you must cite one of:
 *     * J. Nocedal. Updating  Quasi-Newton  Matrices  with  Limited  Storage
 *    (1980), Mathematics of Computation 35, pp. 773-782.
 *  * D.C. Liu and J. Nocedal. On the  Limited  mem  Method  for  Large
 *    Scale  Optimization  (1989),  Mathematical  Programming  B,  45,  3,
 *    pp. 503-528.
 *  * 
 * 
 * @param maxIter: maximum number of iterations, or &lt;= 0 for unlimited
 * @param m: The memory of the search. 3 to 7 is usually sufficient.
 */
class LBFGS[T](maxIter: Int = -1, m: Int=5)
              (implicit vspace: MutableCoordinateSpace[T, Double]) extends FirstOrderMinimizer[T,DiffFunction[T]](maxIter) with ConfiguredLogging {

  import vspace._
  require(m > 0)

  case class History(private[LBFGS] val memStep: IndexedSeq[T] = IndexedSeq.empty,
                     private[LBFGS] val memGradDelta: IndexedSeq[T] = IndexedSeq.empty,
                     private[LBFGS] val memRho: IndexedSeq[Double] = IndexedSeq.empty)


  protected def takeStep(state: State, dir: T, stepSize: Double) = state.x + dir * stepSize
  protected def initialHistory(f: DiffFunction[T], x: T):History = new History()
  protected def chooseDescentDirection(state: State):T = {
    val grad = state.grad
    val memStep = state.history.memStep
    val memGradDelta = state.history.memGradDelta
    val memRho = state.history.memRho
    val diag = if(memStep.size > 0) {
      computeDiag(state.iter,grad,memStep.last,memGradDelta.last)
    } else {
      zeros(grad) + 1.
    }

    val dir:T = copy(grad)
    val as = new Array[Double](m)

    for(i <- (memStep.length-1) to 0 by -1) {
      as(i) = (memStep(i) dot dir)/memRho(i)
      if(as(i).isNaN) {
        throw new NaNHistory
      }
      assert(!as(i).isInfinite, memRho(i) -> norm(grad,2))
      dir -= memGradDelta(i) * as(i)
    }

    dir :*= diag

    for(i <- 0 until memStep.length) {
      val beta = (memGradDelta(i) dot dir)/memRho(i)
      dir += memStep(i) * (as(i) - beta)
    }

    dir *= -1.0
    dir
  }

  protected def updateHistory(newX: T, newGrad: T, newVal: Double, oldState: State): History = {
    val gradDelta : T = (newGrad :- oldState.grad)
    val step:T = (newX - oldState.x)

    var memStep = oldState.history.memStep :+ step
    var memGradDelta = oldState.history.memGradDelta :+ gradDelta
    var memRho = oldState.history.memRho :+ (step dot gradDelta)

    if(memStep.length > m) {
      memStep = memStep.drop(1)
      memRho = memRho.drop(1)
      memGradDelta = memGradDelta.drop(1)
    }

    new History(memStep,memGradDelta,memRho)
  }

  private def computeDiag(iter: Int, grad: T, prevStep: T, prevGrad: T):T = {
    if(iter == 0) {
      zeros(grad) + 1.
    } else {
      val sy = prevStep dot prevGrad
      val yy = prevGrad dot prevGrad
      val syyy = if(sy < 0 || sy.isNaN) {
        throw new NaNHistory
      } else {
        sy/yy
      }
     ((zeros(grad) + 1.)* sy/yy)
    }
  }
   
  /**
   * Given a direction, perform a line search to find 
   * a direction to descend. At the moment, this just executes
   * backtracking, so it does not fulfill the wolfe conditions.
   * 
   * @param f: The objective
   * @param dir: The step direction
   * @param x: The location
   * @return (stepSize, newValue)
   */

  protected def determineStepSize(state: State, f: DiffFunction[T], dir: T) = {
    val iter = state.iter
    val x = state.x
    val grad = state.grad

    val normGradInDir = {
      val possibleNorm = dir dot grad
      if (possibleNorm > 0) { // hill climbing is not what we want. Bad LBFGS.
        log.warn("Direction of positive gradient chosen!")
        log.warn("Direction is:" + possibleNorm)
        // Reverse the direction, clearly it's a bad idea to go up
        dir *= -1.0
        dir dot grad
      } else {
        possibleNorm
      }
    }

    def ff(alpha: Double) = f.valueAt(x + dir * alpha)
    val search = new BacktrackingLineSearch(cScale = if(iter < 1) 0.01 else 0.5, initAlpha = 1.0)
    val iterates = search.iterations(ff)
    val targetState = iterates.find { case search.State(alpha,v) =>
      // sufficient descent
      val r = v < state.value + alpha * 0.0001 * normGradInDir
      if(!r) log.info(".")
      r

    }
    val search.State(alpha,currentVal) = targetState.getOrElse(throw new LineSearchFailed)

    if(alpha * norm(grad,Double.PositiveInfinity) < 1E-10)
      throw new StepSizeUnderflow
    alpha
  }

}

object LBFGS {
  def main(args: Array[String]) {
    val lbfgs = new LBFGS[Counter[Int,Double]](5,4) with ConsoleLogging

    def optimizeThis(init: Counter[Int,Double]) = {
      val f = new DiffFunction[Counter[Int,Double]] {
        def calculate(x: Counter[Int,Double]) = {
          (math.pow(norm((x - 3.0),2),2),(x * 2.0) - 6.0)
        }
      }

      val result = lbfgs.minimize(f,init)
    }

    optimizeThis(Counter(1->0.,2->0.,3->0.,4->0.,5->0.))
  }
}

