package scalanlp.optimize;

import scala.collection.mutable.{_};
import scalala.Scalala._;
import scalala.tensor._;
import scalala.tensor.operators._;
import TensorShapes._;

import scalanlp.util._;
import scalanlp.stats.sampling._;

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
abstract class StochasticGradientDescent[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]](val eta: Double,
    val maxIter: Int,
    val batchSize: Int)(implicit protected val arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col])
																	    extends FirstOrderMinimizer[K,T,BatchDiffFunction[K,T]]
                                      with GradientNormConvergence[K,T]
                                      with Logged {

  type History;
  def initialHistory(f: BatchDiffFunction[K,T], init: T):History;
  def updateHistory(oldState: State, newX: T, curValue: Double, curGrad: T):History
  def adjustGradient(grad:T, x: T):T = grad

  def iterations(f: BatchDiffFunction[K,T], init: T):Iterator[State] = {
    val it = Iterator.iterate(initialState(f,init)) { case state @ State(oldX, oldV, _, _, iter, _) =>
      val sample = chooseBatch(f,state);

      val (value,grad: T) = f.calculate(oldX,sample);
      log(Log.INFO)("SGD gradient norm: " + norm(grad,2));
      assert(grad.forall(v => !v._2.isInfinite && !v._2.isNaN));
      log(Log.INFO)("SGD value: " + value);
      val stepSize = chooseStepSize(state.copy(value=value,grad=grad));
      val newX = projectVector(state, oldX, grad, stepSize);
      assert(newX.forall(v => !v._2.isInfinite && !v._2.isNaN));
      val newState = State(newX, value, grad, adjustGradient(grad,newX), iter + 1, updateHistory(state,newX,value,grad))
      newState
    };

    it.drop(1).takeWhile { case State(x,v,g,_,i,_) => i < maxIter && !checkConvergence(v,g)}
  }


  // Optional hooks with reasonable defaults

  /**
   * Chooses the initial state
   */
  def initialState(f: BatchDiffFunction[K,T], init: T): State = {
    State(init,Double.PositiveInfinity,init.like, init.like, 0, initialHistory(f,init));
  }


  /**
  * Selects a sample of the data to evaluate on. By default, it does
  * repeated sweeps.
  */
  def chooseBatch(f: BatchDiffFunction[K,T], state: State) = {
    val offset = (batchSize * state.iter) % f.fullRange.size;
    val batch = (offset until (offset + batchSize)) map (i =>f.fullRange(i%f.fullRange.size));
    batch;
  }

  /**
   * Projects the vector onto whatever ball is needed. Can also incorporate regularization, or whatever.
   *
   * Default just takes a step
   */
  def projectVector(state: State, oldX: T, gradient: T, stepSize: Double):T = oldX - gradient * stepSize value;

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
  def apply[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]](eta: Double=4, maxIter: Int=100, batchSize: Int = 50)
    (implicit arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col]):StochasticGradientDescent[K,T]  = {
      new SimpleSGD(eta,maxIter,batchSize);
  }

  class SimpleSGD[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]]
      (eta: Double=4, maxIter: Int=100, batchSize: Int = 50)(implicit arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col])
          extends StochasticGradientDescent[K,T](eta,maxIter,batchSize) {
      type History = Unit
      def initialHistory(f: BatchDiffFunction[K,T],init: T)= ()
      def updateHistory(oldState: State,newX: T,curValue: Double,curGrad: T) = ()
  }

}
