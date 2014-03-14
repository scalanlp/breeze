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

import breeze.math.MutableInnerProductSpace
import breeze.linalg._
import breeze.linalg.operators.OpMulMatrix
import breeze.util.SerializableLogging


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
              (implicit space: MutableInnerProductSpace[T, Double]) extends FirstOrderMinimizer[T,DiffFunction[T]](maxIter, tolerance) with SerializableLogging {

  import space._
  require(m > 0)

  type History = LBFGS.ApproximateInverseHessian[T]


  override protected def adjustFunction(f: DiffFunction[T]): DiffFunction[T] = f.cached

  protected def takeStep(state: State, dir: T, stepSize: Double) = state.x + dir * stepSize
  protected def initialHistory(f: DiffFunction[T], x: T):History = new LBFGS.ApproximateInverseHessian(m)
  protected def chooseDescentDirection(state: State, fn: DiffFunction[T]):T = {
    state.history * state.grad
  }

  protected def updateHistory(newX: T, newGrad: T, newVal: Double,  f: DiffFunction[T], oldState: State): History = {
    oldState.history.updated(newX - oldState.x, newGrad :- oldState.grad)
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
    val x = state.x
    val grad = state.grad

    val ff = LineSearch.functionFromSearchDirection(f, x, dir)
    val search = new StrongWolfeLineSearch(maxZoomIter = 10, maxLineSearchIter = 10) // TODO: Need good default values here.
    val alpha = search.minimize(ff, if(state.iter == 0.0) 1.0/norm(dir) else 1.0)

    if(alpha * norm(grad) < 1E-10)
      throw new StepSizeUnderflow
    alpha
  }

}

object LBFGS {
  case class ApproximateInverseHessian[T](m: Int,
                                          private[LBFGS] val memStep: IndexedSeq[T] = IndexedSeq.empty,
                                          private[LBFGS] val memGradDelta: IndexedSeq[T] = IndexedSeq.empty)
                                         (implicit vspace: MutableInnerProductSpace[T, Double]) extends NumericOps[ApproximateInverseHessian[T]] {

    import vspace._

    def repr: ApproximateInverseHessian[T] = this

    def updated(step: T, gradDelta: T) = {
      val memStep = (step +: this.memStep) take m
      val memGradDelta = (gradDelta +: this.memGradDelta) take m

      new ApproximateInverseHessian(m, memStep,memGradDelta)
    }


    def historyLength = memStep.length

    def *(grad: T) = {
     val diag = if(historyLength > 0) {
       val prevStep = memStep.head
       val prevGradStep = memGradDelta.head
       val sy = prevStep dot prevGradStep
       val yy = prevGradStep dot prevGradStep
       if(sy < 0 || sy.isNaN) throw new NaNHistory
       sy/yy
     } else {
       1.0
     }

     val dir = vspace.copy(grad)
     val as = new Array[Double](m)
     val rho = new Array[Double](m)

     for(i <- 0 until historyLength) {
       rho(i) = (memStep(i) dot memGradDelta(i))
       as(i) = (memStep(i) dot dir)/rho(i)
       if(as(i).isNaN) {
         throw new NaNHistory
       }
       axpy(-as(i), memGradDelta(i), dir)
     }

     dir *= diag

     for(i <- (historyLength - 1) to 0 by (-1)) {
       val beta = (memGradDelta(i) dot dir)/rho(i)
       axpy(as(i) - beta, memStep(i), dir)
     }

     dir *= -1.0
     dir
    }
  }


  implicit def multiplyInverseHessian[T](implicit vspace: MutableInnerProductSpace[T, Double]):OpMulMatrix.Impl2[ApproximateInverseHessian[T], T, T] = {
    new OpMulMatrix.Impl2[ApproximateInverseHessian[T], T, T] {
      def apply(a: ApproximateInverseHessian[T], b: T): T = a * b
    }

  }
}
