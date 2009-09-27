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
import scalala.tensor.Vector;
import scalala.library.Vectors._;
import scalala.library.Implicits._;

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
class ContrastiveDivergenceOptimizer[X](trans: Vector=>X=>Rand[X],
        deriv: Vector=>X=>Vector,
        learningRate: Double) extends Logged {
  /**
  * Run CD to convergence on the given data with initial parameters.
  */
  def maximize(data: Seq[X], init: Vector) = {
    var cur: Vector = init;
    var next: Vector = null;
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
  def converged(currentTheta: Vector, nextTheta: Vector) = {
   norm(currentTheta - nextTheta,2)< (1E-4 * 1E-4)
  }

  /**
  * Take a single step using CD.
  */
  def step(data: Seq[X], theta: Vector) = {
    val perturbedData = data map (trans(theta)) map (_.draw);
    val thetaDeriv = deriv(theta);
    val normalGrad = (data map thetaDeriv).foldLeft(zeros(theta.size)){ (x,y) => 
      x + y / data.length value;
    } 
    val perturbedGrad = (perturbedData map thetaDeriv).foldLeft(zeros(theta.size)){ (x,y) => 
      x + y / data.length value;
    }

    theta - (normalGrad - perturbedGrad) * learningRate value;
  }
}


object TestCD {
  def main(arg: Array[String]) {
    val data = (new Gaussian(3,1).samples take 1000).collect;
    def trans(mean: Vector) = { (x:Double) =>
      new Gaussian(mean(0),1)
    }
    def deriv(theta: Vector) = { (x:Double) => 
      (Array((x-theta(0))) : Vector)
    }
    val opt = new ContrastiveDivergenceOptimizer[Double](trans _ ,deriv _ ,0.01) with ConsoleLogging;
    
    opt.maximize(data,Array(-100.0));
  }
  
}
