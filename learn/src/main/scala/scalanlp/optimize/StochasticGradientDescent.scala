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

  /**
  * Runs SGD on f, for mxIter. It ignores tol.
  */
  def minimize(f: BatchDiffFunction[K,T], init: T) = {
    var guess = init;

    log(Log.INFO)("SGD starting");
    log(Log.INFO)("SGD init: " + init);
    log(Log.INFO)("SGD maxIter: " + maxIter);

    var i = 0;
    var converged = false;
    while(!converged && (i < maxIter || maxIter <= 0)) {
      log(Log.INFO)("SGD iteration: " + i);
      val sample = selectSample(f,i);

      val (value,grad) = f.calculate(guess,sample);
      log(Log.INFO)("SGD gradient: " + norm(grad,2));
      assert(grad.forall(!_._2.isInfinite));
      log(Log.INFO)("SGD value: " + value);
      guess = update(guess,grad,i);
      assert(guess.forall(!_._2.isInfinite));
      //log(Log.INFO)("SGD update: " + guess.mkString("[",",","]"));

      i+=1;
      converged = checkConvergence(value,grad);
    }
    guess;
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
  def selectSample(f: BatchDiffFunction[K,T], iter: Int) : Seq[Int] = {
    Rand.permutation(f.fullRange.size).draw.map(f.fullRange).take(batchSize);
  }
}
