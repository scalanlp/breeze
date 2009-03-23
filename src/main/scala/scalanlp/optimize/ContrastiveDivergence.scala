package scalanlp.optimize

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

import stats.sampling._;
import math.Arrays._;

import util._;
import Log._;

/**
* Implements Contrastive Divergence for maximizing the probability of parameters.
*
* see www.robots.ox.ac.uk/~ojw/files/NotesOnCD.pdf 
*
* @param X the type of the data
* @param trans: transition kernel from X to a new X, given parameters Array[Double
* @param deriv: derivative of the energy function f, where f = log p(x), up to some constant.
* @param learningRate: to slow the gradient descent, or whatever.
* @author dlwh
*/
class ContrastiveDivergenceOptimizer[X](trans: Array[Double]=>X=>Rand[X],
        deriv: Array[Double]=>X=>Array[Double],
        learningRate: Double) extends Logged {
  /**
  * Run CD to convergence on the given data with initial parameters.
  */
  def maximize(data: Seq[X], init: Array[Double]) = {
    var cur: Array[Double] = init;
    var next: Array[Double] = null;
    var i = 0;
    do {
      log(INFO)("Starting iteration: " +i);
      log(INFO)("Current v:" + cur.mkString("[",",","]"));
      next = step(data,cur);
      val temp = cur;
      cur = next;
      next = temp;
      i += 1;
    } while(!converged(cur,next))
    cur
  }

  /**
  * True if the 2 norm of the difference is sufficiently small
  */
  def converged(currentTheta: Array[Double], nextTheta: Array[Double]) = {
    val diff = scaleAdd(currentTheta,-1,nextTheta);
    val dot = dotProduct(diff,diff);
    dot < (1E-4 * 1E-4)
  }

  /**
  * Take a single step using CD.
  */
  def step(data: Seq[X], theta: Array[Double]) = {
    val perturbedData = data map (trans(theta)) map (_.draw);
    val thetaDeriv = deriv(theta);
    val normalGrad = (data map thetaDeriv).foldLeft(new Array[Double](theta.length)){ (x,y) => 
      scaleAdd(x,1.0/data.length,y,x)
    } 
    val perturbedGrad = (perturbedData map thetaDeriv).foldLeft(new Array[Double](theta.length)){ (x,y) => 
      scaleAdd(x,1.0/data.length,y,x)
    }
    val result = new Array[Double](theta.length);
    var i = 0;
    while(i < result.length) {
      result(i) = theta(i) + learningRate * (normalGrad(i) - perturbedGrad(i));
      i += 1
    }

    result;
  }
}


object TestCD {
  def main(arg: Array[String]) {
    val data = (new Gaussian(3,1).samples take 1000).collect;
    def trans(mean: Array[Double]) = { (x:Double) =>
      new Gaussian(mean(0),1)
    }
    def deriv(theta: Array[Double]) = { (x:Double) => 
      Array((x-theta(0)))
    }
    val opt = new ContrastiveDivergenceOptimizer[Double](trans _ ,deriv _ ,0.01) with ConsoleLogging;
    
    opt.maximize(data,Array(-100.0));
  }
  
}
