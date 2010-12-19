package scalanlp.optimize;

import scalala._;
import generic.collection.{CanMapValues, CanCreateZerosLike}
import operators._
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
                                            (implicit protected val view: T=>MutableNumericOps[T],
                                             protected val view2: T=>HasValuesMonadic[T,Double],
                                             protected val canMapValues: CanMapValues[T,Double,Double,T],
                                             protected val canNorm: CanNorm[T],
                                             protected val canAddScalar: BinaryOp[T,Double,OpAdd,T],
                                             protected val canAdd: BinaryOp[T,T,OpAdd,T],
                                             protected val canSub: BinaryOp[T,T,OpSub,T],
                                             protected val canMulScalar: BinaryOp[T,Double,OpMul,T],
                                             protected val canMulPiece: BinaryOp[T,T,OpMul,T],
                                             protected val canDivScalar: BinaryOp[T,Double,OpDiv,T],
                                             protected val canDivPiece: BinaryOp[T,T,OpDiv,T],
                                             protected val zeros: CanCreateZerosLike[T,T])
  extends FirstOrderMinimizer[T,BatchDiffFunction[T]]
                                      with GradientNormConvergence[T]
                                      with Logged {

  type History;
  def initialHistory(f: BatchDiffFunction[T], init: T):History;
  def updateHistory(oldState: State, newX: T, curValue: Double, curGrad: T):History
  def adjustGradient(grad:T, x: T):T = grad

  def iterations(f: BatchDiffFunction[T], init: T):Iterator[State] = {
    val it = Iterator.iterate(initialState(f,init)) { case state @ State(oldX, oldV, _, _, iter, _, _) =>
      val sample = chooseBatch(f,state);

      val (value,grad: T) = f.calculate(oldX,sample);
      log(Log.INFO)("SGD gradient norm: " + norm(grad,2));
//      assert(grad.forall(v => !v._2.isInfinite && !v._2.isNaN));
      log(Log.INFO)("SGD value: " + value);
      val stepSize = chooseStepSize(state.copy(value=value,grad=grad));
      val newX = projectVector(state, oldX, grad, stepSize);
      val newState = State(newX, value, grad, adjustGradient(grad,newX), iter + 1, updateHistory(state,newX,value,grad))
//      assert(newX.forall(v => !v._2.isInfinite && !v._2.isNaN));
      newState
    };

    it.drop(1).takeWhile { case State(x,v,g,_,i,_, _) => (i < maxIter || maxIter < 0) && !checkConvergence(v,g)}
  }


  // Optional hooks with reasonable defaults

  /**
   * Chooses the initial state
   */
  def initialState(f: BatchDiffFunction[T], init: T): State = {
    State(init,Double.PositiveInfinity,zeros(init), zeros(init), 0, initialHistory(f,init));
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
  def apply[T](eta: Double=4, maxIter: Int=100, batchSize: Int = 50) (implicit view: T=>MutableNumericOps[T],
                                           view2: T=>HasValuesMonadic[T,Double],
                                           canNormT: CanNorm[T],
                                           canMapValues: CanMapValues[T,Double,Double,T],
                                           canAddScalar: BinaryOp[T,Double,OpAdd,T],
                                           canAdd: BinaryOp[T,T,OpAdd,T],
                                           canSub: BinaryOp[T,T,OpSub,T],
                                           canMulScalar: BinaryOp[T,Double,OpMul,T],
                                           canDivScalar: BinaryOp[T,Double,OpDiv,T],
                                           canMulPiece: BinaryOp[T,T,OpMul,T],
                                           canDivPiece: BinaryOp[T,T,OpDiv,T],
                                           zeros: CanCreateZerosLike[T,T]):StochasticGradientDescent[T]  = {
      new SimpleSGD(eta,maxIter,batchSize);
  }

  class SimpleSGD[T] (eta: Double=4,
                      maxIter: Int=100,
                      batchSize: Int = 50)(implicit view: T=>MutableNumericOps[T],
                                           view2: T=>HasValuesMonadic[T,Double],
                                           canNorm: CanNorm[T],
                                           canMapValues: CanMapValues[T,Double,Double,T],
                                           canAddScalar: BinaryOp[T,Double,OpAdd,T],
                                           canAdd: BinaryOp[T,T,OpAdd,T],
                                           canSub: BinaryOp[T,T,OpSub,T],
                                           canMulScalar: BinaryOp[T,Double,OpMul,T],
                                           canMulPiece: BinaryOp[T,T,OpMul,T],
                                           canDivPiece: BinaryOp[T,T,OpDiv,T],
                                           canDivScalar: BinaryOp[T,Double,OpDiv,T],
                                           canSqrt: CanSqrt[T,T],
                                           zeros: CanCreateZerosLike[T,T]) extends StochasticGradientDescent[T](eta,maxIter,batchSize) {
      type History = Unit
      def initialHistory(f: BatchDiffFunction[T],init: T)= ()
      def updateHistory(oldState: State,newX: T,curValue: Double,curGrad: T) = ()
  }

}
