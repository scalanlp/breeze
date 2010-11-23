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
class StochasticGradientDescent[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]](val alpha: Double,
    val maxIter: Int,
    batchSize: Int)(implicit arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col]) 
																	    extends Minimizer[T,BatchDiffFunction[K,T]] 
                                      with GradientNormConvergence[K,T]
                                      with Logged {


  case class State protected[StochasticGradientDescent](x: T, value: Double,
                                                   grad: T,
                                                   iter: Int);

  def iterations(f: BatchDiffFunction[K,T], init: T):Iterator[State] = {
    val it = Iterator.iterate(State(init,Double.NegativeInfinity,init.like,0)) { case State(oldX, oldV, _, iter) =>
      val sample = selectSample(f,iter);

      val (value,grad) = f.calculate(oldX,sample);
      log(Log.INFO)("SGD gradient: " + norm(grad,2));
      assert(grad.forall(!_._2.isInfinite));
      log(Log.INFO)("SGD value: " + value);
      val guess = update(oldX,grad,iter);
      //log(Log.INFO)("SGD update: " + guess.mkString("[",",","]"));
      val newState = State(guess, value, grad, iter + 1)
      newState
    };

    it.drop(1).takeWhile { case State(x,v,g,i) => i < maxIter && !checkConvergence(v,g)}
  }

  /**
  * Runs SGD on f, for maxIter.
  */
  def minimize(f: BatchDiffFunction[K,T], init: T) = {
    iterations(f,init).reduceLeft((a,b) => b).x;
  }
  
  /**
  * Applies the update given the gradient. By default, it executes:
  *
  * guess - temp * grad;
  */
  def update(guess: T, grad: T, iter: Int):T = {
    guess - grad * (alpha/(alpha + iter)) value;
  }

  /**
  * Selects a sample of the data to evalute on. By default, it selects
  * a random sample without replacement.
  */
  def selectSample(f: BatchDiffFunction[K,T], iter: Int) : IndexedSeq[Int] = {
    Rand.permutation(f.fullRange.size).draw.map(f.fullRange).take(batchSize);
  }
}
