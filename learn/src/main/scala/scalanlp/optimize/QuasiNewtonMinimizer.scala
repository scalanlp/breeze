package scalanlp.optimize

import scalanlp.util.Logged
import scalanlp.util.Log._
import scalanlp.optimize.QuasiNewtonMinimizer._;
import scalala.tensor._
import scalala.Scalala._
import scalala.tensor.operators._;
import TensorShapes._;

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
 * A base trait for any quasi-newton optimizers.
 * @author dlwh
 */
trait QuasiNewtonMinimizer[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]]
        extends Minimizer[T,DiffFunction[K,T]] with Logged with CheckedConvergence[K,T] {

  protected val maxIter: Int = -1;

  def minimize(f: DiffFunction[K,T], init: T):T = {
    val steps = iterations(f,init);
    val convSteps = for( cur@State(x,v,grad, adjGradient, iter,history)  <- steps) yield {
      log(INFO)("Iteration: " + iter);
      log(INFO)("Current v:" + v);
      log(INFO)("Current grad norm:" + norm(adjGradient,2));
      (cur,checkConvergence(v,adjGradient));
    }

    convSteps.dropWhile(state => !state._2 && (state._1.iter < maxIter || maxIter < 0)).next._1.x;
  }

  type History;
  case class State protected[QuasiNewtonMinimizer](x: T, value: Double,
                                                   grad: T,
                                                   adjustedGradient: T,iter: Int,
                                                   history: History);

  protected def initialState(f: DiffFunction[K,T], init: T):State = {
    val (v,grad) = f.calculate(init);
    new State(init,v,grad,adjustGradient(grad,init),0,initialHistory(grad));
  }

  protected def initialHistory(grad: T):History;
  /** Changes the gradient for each iteration before search is started. */
  protected def adjustGradient(grad: T, x: T): T;
  protected def chooseDescentDirection(grad: T, state: State):T;
  protected def chooseStepSize(f: DiffFunction[K,T], dir: T, grad: T, state: State):(Double,Double);
  protected def updateHistory(oldState: State, newGrad: T, newVal: Double, step: T): History;

  def iterations(f: DiffFunction[K,T],init: T): Iterator[State] = Iterator.iterate(initialState(f,init)) {state =>
     val x : T = state.x.copy;
     val grad = state.grad;
     val v = state.value;
     val iter = state.iter;
     val adjGrad = state.adjustedGradient

     try {
       val dir = chooseDescentDirection(adjGrad, state);
       val (stepScale,newVal) = chooseStepSize(f, dir, adjGrad, state);
       log(INFO)("Scale:" +  stepScale);
       dir *= stepScale;
       x += dir;
       if (norm(dir,2) <= 1E-20) {
         throw new StepSizeUnderflow;
       }

       val newGrad = f.gradientAt(x);

       val newHistory = updateHistory(state, newGrad, newVal, dir);

       new State(x,newVal,newGrad,adjustGradient(newGrad,x),iter+1,newHistory);

     } catch {
       case _:StepSizeUnderflow =>
        log(ERROR)("Step size underflow! Clearing history.");
        state.copy(history=initialHistory(state.grad), iter = iter+1)
       case _: QNException =>
         log(ERROR)("Something in the history is giving NaN's, clearing it!");
         state.copy(history=initialHistory(state.grad), iter=iter+1);
     }

   }

}

object QuasiNewtonMinimizer {
  class QNException extends RuntimeException;
  class NaNHistory extends QNException;
  class StepSizeUnderflow extends QNException;
}