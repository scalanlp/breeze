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

import breeze.math.{MutableInnerProductSpace, MutableCoordinateSpace}
import breeze.linalg._
import com.typesafe.scalalogging.log4j.Logging


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
 * @param maxIter: maximum number of iterations, or <= 0 for unlimited
 * @param m: The memory of the search. 3 to 7 is usually sufficient.
 */
class LBFGS[T](maxIter: Int = -1, m: Int=10, tolerance: Double=1E-9)
              (implicit vspace: MutableInnerProductSpace[T, Double]) extends FirstOrderMinimizer[T,DiffFunction[T]](maxIter, tolerance) with Logging {

  import vspace._
  require(m > 0)

  case class History(private[LBFGS] val memStep: IndexedSeq[T] = IndexedSeq.empty,
                     private[LBFGS] val memGradDelta: IndexedSeq[T] = IndexedSeq.empty)


  protected def takeStep(state: State, dir: T, stepSize: Double) = state.x + dir * stepSize
  protected def initialHistory(f: DiffFunction[T], x: T):History = new History()
  protected def chooseDescentDirection(state: State):T = {
    val grad = state.grad
    val memStep = state.history.memStep
    val memGradDelta = state.history.memGradDelta
    val diag = if(memStep.size > 0) {
      computeDiagScale(memStep.head,memGradDelta.head)
    } else {
      1.0
    }

    val dir:T = copy(grad)
    val as = new Array[Double](m)
    val rho = new Array[Double](m)

    for(i <- 0 until memStep.length) {
      rho(i) = (memStep(i) dot memGradDelta(i))
      as(i) = (memStep(i) dot dir)/rho(i)
      if(as(i).isNaN) {
        throw new NaNHistory
      }
      axpy(-as(i), memGradDelta(i), dir)
    }

    dir *= diag

    for(i <- (memStep.length - 1) to 0 by (-1)) {
      val beta = (memGradDelta(i) dot dir)/rho(i)
      axpy(as(i) - beta, memStep(i), dir)
    }

    dir *= -1.0
    dir
  }

  protected def updateHistory(newX: T, newGrad: T, newVal: Double, oldState: State): History = {
    val gradDelta : T = (newGrad :- oldState.grad)
    val step:T = (newX - oldState.x)

    val memStep = (step +: oldState.history.memStep) take m
    val memGradDelta = (gradDelta +: oldState.history.memGradDelta) take m


    new History(memStep,memGradDelta)
  }

  private def computeDiagScale(prevStep: T, prevGradStep: T):Double = {
    val sy = prevStep dot prevGradStep
    val yy = prevGradStep dot prevGradStep
    if(sy < 0 || sy.isNaN) throw new NaNHistory
    sy/yy
  }
   
  /**
   * Given a direction, perform a line search to find 
   * a direction to descend. At the moment, this just executes
   * backtracking, so it does not fulfill the wolfe conditions.
   *
   * @param state the current state
   * @param f The objective
   * @param dir The step direction
   * @return stepSize
   */
  protected def determineStepSize(state: State, f: DiffFunction[T], dir: T) = {
    val iter = state.iter
    val x = state.x
    val grad = state.grad

    val normGradInDir = {
      val possibleNorm = dir dot grad
      if (possibleNorm > 0) { // hill climbing is not what we want. Bad LBFGS.
        logger.warn("Direction of positive gradient chosen!")
        logger.warn("Direction is:" + possibleNorm)
        // Reverse the direction, clearly it's a bad idea to go up
        dir *= -1.0
        dir dot grad
      } else {
        possibleNorm
      }
    }

    val ff = LineSearch.functionFromSearchDirection(f, x, dir)
    val search = new StrongWolfeLineSearch(maxZoomIter = 10, maxLineSearchIter = 10) // TODO: Need good default values here.
    val alpha = search.minimize(ff, if(state.iter == 0.0) 1.0/norm(dir) else 1.0)

    if(alpha * norm(grad) < 1E-10)
      throw new StepSizeUnderflow
    alpha
  }

}

object LBFGS {
  def main(args: Array[String]) {
    val lbfgs = new LBFGS[Counter[Int,Double]](5,4)

    def optimizeThis(init: Counter[Int,Double]) = {
      val f = new DiffFunction[Counter[Int,Double]] {
        def calculate(x: Counter[Int,Double]) = {
          (math.pow(norm((x - 3.0),2),2),(x * 2.0) - 6.0)
        }
      }

      val result = lbfgs.minimize(f,init)
      println(result)
    }

    optimizeThis(Counter(1->0.0,2->0.0,3->0.0,4->0.0,5->0.0))
  }
}

