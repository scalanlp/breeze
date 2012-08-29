package breeze.optimize

import breeze.util._
import logging.ConfiguredLogging
import breeze.stats.distributions._
import breeze.math.MutableCoordinateSpace


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

/** 
* Minimizes a function using stochastic gradient descent
*
* @author dlwh
*/
abstract class StochasticGradientDescent[T](val defaultStepSize: Double,
                                            val maxIter: Int,
                                            tolerance: Double=1E-5,
                                            improvementTol: Double=1E-4,
                                            minImprovementWindow: Int = 50)
                                            (implicit protected val vspace: MutableCoordinateSpace[T, Double])
  extends FirstOrderMinimizer[T,StochasticDiffFunction[T]](maxIter, tolerance, improvementTol, minImprovementWindow, 2) with ConfiguredLogging {

  import vspace._

  // Optional hooks with reasonable defaults

  /**
   * Projects the vector x onto whatever ball is needed. Can also incorporate regularization, or whatever.
   *
   * Default just takes a step
   */
  protected def takeStep(state: State, dir: T, stepSize: Double) = state.x + dir * stepSize
  protected def chooseDescentDirection(state: State) = state.grad * -1.0


  override protected def updateFValWindow(oldState: State, newAdjVal: Double) = {
    if(oldState.fVals.isEmpty) IndexedSeq(newAdjVal)
    else {
      // weighted average. less sensitive to outliers
      val interm = oldState.fVals :+ ((oldState.fVals.last * 3 + newAdjVal)/4.0)
      if(interm.length > minImprovementWindow) interm.drop(1)
      else interm
    }
  }

  /**
   * Choose a step size scale for this iteration.
   *
   * Default is eta / math.pow(state.iter + 1,2.0 / 3.0)
   */
  def determineStepSize(state: State, f: StochasticDiffFunction[T], dir: T) = {
    defaultStepSize / math.pow(state.iter + 1, 2.0 / 3.0)
  }


}

object StochasticGradientDescent {
  def apply[T](initialStepSize: Double=4, maxIter: Int=100)(implicit vs: MutableCoordinateSpace[T, Double]) :StochasticGradientDescent[T]  = {
    new SimpleSGD(initialStepSize,maxIter)
  }

  class SimpleSGD[T](eta: Double=4,
                     maxIter: Int=100)
                    (implicit vs: MutableCoordinateSpace[T, Double]) extends StochasticGradientDescent[T](eta,maxIter) {
    type History = Unit
    def initialHistory(f: StochasticDiffFunction[T],init: T)= ()
    def updateHistory(newX: T, newGrad: T, newValue: Double, oldState: State) = ()
  }

}
