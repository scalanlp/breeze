package scalanlp.optimize

import scalanlp.util.Logged
import scalanlp.util.Log._
import scalanlp.optimize.QuasiNewtonMinimizer._
import scalala.generic.math.CanNorm
import scalala.library.Library.norm
import scalala.operators._
;

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
trait QuasiNewtonMinimizer[T] extends FirstOrderMinimizer[T,DiffFunction[T]] with Logged with CheckedConvergence[T] {

  protected val maxIter: Int = -1;
  protected implicit val view: T => NumericOps[T];
  protected implicit val opAdd: BinaryOp[T,T,OpAdd,T];
  protected implicit val opMulScalar: BinaryOp[T,Double,OpMul,T];
  protected implicit val canNorm: CanNorm[T];

  protected def initialState(f: DiffFunction[T], init: T):State = {
    val (v,grad) = f.calculate(init);
    new State(init,v,grad,adjustGradient(grad,init),0,initialHistory(grad));
  }

  protected def initialHistory(grad: T):History;
  /** Changes the gradient for each iteration before search is started. */
  protected def adjustGradient(grad: T, x: T): T;
  protected def chooseDescentDirection(grad: T, state: State):T;
  protected def chooseStepSize(f: DiffFunction[T], dir: T, grad: T, state: State):(Double,Double);
  protected def updateHistory(oldState: State, newGrad: T, newVal: Double, step: T): History;
  protected def project(newX: T, grad: T, state: State) = newX;


  def iterations(f: DiffFunction[T],init: T): Iterator[State] = {
    val it =  Iterator.iterate(initialState(f,init)) {state =>
     val x : T = state.x;
     val grad = state.grad;
     val v = state.value;
     val iter = state.iter;
     val adjGrad = state.adjustedGradient

     try {
       val dir = chooseDescentDirection(adjGrad, state);
       val (stepScale,newVal) = chooseStepSize(f, dir, adjGrad, state);
       log(INFO)("Scale:" +  stepScale);
       val step = dir * stepScale
       val newX = x + step;
       if (norm(step,2) <= 1E-20) {
         throw new StepSizeUnderflow;
       }

       val newGrad = f.gradientAt(newX);

       val newHistory = updateHistory(state, newGrad, newVal, dir);

       new State(newX,newVal,newGrad,adjustGradient(newGrad,newX),iter+1,newHistory);

     } catch {
       case _:StepSizeUnderflow =>
        log(ERROR)("Step size underflow! Clearing history.");
        state.copy(history=initialHistory(state.grad), iter = iter+1)
       case _: QNException =>
         log(ERROR)("Something in the history is giving NaN's, clearing it!");
         state.copy(history=initialHistory(state.grad), iter=iter+1);
     }
    }

    // These methods, unlike SGD, update gradient and the like after taking a step, so you should include the first step that converges.
    it.sliding(2).takeWhile{ case Seq(state,state2) =>
      (state.iter < maxIter || maxIter < 0) && !checkConvergence(state.value,state.adjustedGradient) && state.failures < 5
    }.map(_(1));

   }

}

object QuasiNewtonMinimizer {
  class QNException extends RuntimeException;
  class NaNHistory extends QNException;
  class StepSizeUnderflow extends QNException;
  class LineSearchFailed extends QNException;
}