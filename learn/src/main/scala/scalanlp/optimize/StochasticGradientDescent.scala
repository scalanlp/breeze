package scalanlp.optimize;

import scalala._;
import generic.collection.{CanMapValues, CanCreateZerosLike}
import operators._
import bundles.MutableInnerProductSpace
import scalanlp.util._;
import scalanlp.stats.sampling._
import scalala.generic.math.{CanSqrt, CanNorm}
import scalala.library.Library.norm;
import scalala.tensor.mutable;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
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
abstract class StochasticGradientDescent[T](val eta: Double,
                                            val maxIter: Int,
                                            val batchSize: Int)
                                            (implicit protected val vspace: MutableInnerProductSpace[Double,T], protected val canNorm: CanNorm[T])
  extends FirstOrderMinimizer[T,BatchDiffFunction[T]]
                                      with GradientNormConvergence[T]
                                      with Logged {

  import vspace._;

  type History;
  def initialHistory(f: BatchDiffFunction[T], init: T):History;
  def updateHistory(oldState: State, newX: T, curValue: Double, curGrad: T):History
  protected def adjustGradient(grad: T, x: T): T = grad
  protected def adjustValue(value: Double, x: T): Double = value;

  def iterations(f: BatchDiffFunction[T], init: T):Iterator[State] = {
    val it = Iterator.iterate(initialState(f,init)) { state =>
      val oldX = state.x
      val iter = state.iter;
      val sample = chooseBatch(f,state);

      val (value,grad: T) = f.calculate(oldX,sample);
      log(Log.INFO)("SGD gradient norm: " + norm(grad,2));
      log(Log.INFO)("SGD value: " + value);
      val stepSize = chooseStepSize(state.copy(value=value,grad=grad));
      val newX = projectVector(state, oldX, grad, stepSize);
      val newState = State(newX, value, grad, adjustValue(value,newX), adjustGradient(grad,newX), iter + 1, updateHistory(state,newX,value,grad))
      newState
    };

    it.drop(1).takeWhile { state => (state.iter < maxIter || maxIter < 0) && !checkConvergence(state.value,state.grad)}
  }


  // Optional hooks with reasonable defaults

  /**
   * Chooses the initial state
   */
  def initialState(f: BatchDiffFunction[T], init: T): State = {
    State(init,Double.PositiveInfinity,zeros(init), Double.PositiveInfinity, zeros(init), 0, initialHistory(f,init));
  }


  /**
  * Selects a sample of the data to evaluate on. By default, it does
  * repeated sweeps.
  */
  def chooseBatch(f: BatchDiffFunction[T], state: State) = {
    val offset = (batchSize * state.iter) % f.fullRange.size;
    val batch = (offset until (offset + batchSize)) map (i =>f.fullRange(i%f.fullRange.size));
    batch;
  }

  /**
   * Projects the vector onto whatever ball is needed. Can also incorporate regularization, or whatever.
   *
   * Default just takes a step
   */
  def projectVector(state: State, oldX: T, gradient: T, stepSize: Double):T = oldX - gradient * stepSize;

  /**
   * Choose a step size scale for this iteration.
   *
   * Default is eta * math.sqrt(state.iter + 1)
   */
  def chooseStepSize(state: State) = {
    eta / math.sqrt(state.iter + 1)
  }


}

object StochasticGradientDescent {
  def apply[T](eta: Double=4, maxIter: Int=100, batchSize: Int = 50)(implicit vs: MutableInnerProductSpace[Double,T], canNorm: CanNorm[T]) :StochasticGradientDescent[T]  = {
      new SimpleSGD(eta,maxIter,batchSize);
  }

  class SimpleSGD[T] (eta: Double=4,
                      maxIter: Int=100,
                      batchSize: Int = 50)(implicit vs: MutableInnerProductSpace[Double,T], canNorm: CanNorm[T]) extends StochasticGradientDescent[T](eta,maxIter,batchSize) {
      type History = Unit
      def initialHistory(f: BatchDiffFunction[T],init: T)= ()
      def updateHistory(oldState: State,newX: T,curValue: Double,curGrad: T) = ()
  }

}
