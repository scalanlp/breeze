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

import scalala.tensor.Vector;
import scalala.Scalala._;
import scalala.tensor._;
import scalala.tensor.dense.DenseVector;
import scalala.tensor.operators._;
import TensorShapes._;

import scalanlp.stats.sampling._;

import scalanlp.util._;
import Log._;

/**
* Implements Contrastive Divergence for maximizing the probability of parameters.
*
* see www.robots.ox.ac.uk/~ojw/files/NotesOnCD.pdf 
*
* @param X the type of the data
* @param K the type of parameters
* @param T the type of the tensor
* @param trans: transition kernel from X to a new X, given parameters Array[Double
* @param deriv: derivative of the energy function f, where f = log p(x), up to some constant.
* @param learningRate: to slow the gradient descent, or whatever.
* @author dlwh
*/
class ContrastiveDivergenceOptimizer[X,K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]](trans: T=>X=>Rand[X],
        deriv: T=>X=>T,
        learningRate: Double)(implicit arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col]) extends Logged {
  /**
  * Run CD to convergence on the given data with initial parameters.
  */
  def maximize(data: Seq[X], init: T) = {
    val cur: T = init;
	  var converged = false;
    var i = 0;
    do {
      log(INFO)("Starting iteration: " +i);
      log(INFO)("Current v:" + cur);
      val grad = computeGradient(data,cur);
      cur += grad * learningRate;
      i += 1;
      converged = norm(grad,2) < 1E-4;
    } while(!converged);
    cur
  }


  /**
  * Take a single step using CD.
  */
  def computeGradient(data: Seq[X], theta: T) = {
    val perturbedData = data map (trans(theta)) map (_.draw);
    val thetaDeriv = deriv(theta);
    val normalGrad = (data map thetaDeriv).foldLeft(theta.like){ (x,y) => 
      x + y / data.length value;
    } 
    val perturbedGrad = (perturbedData map thetaDeriv).foldLeft(theta.like){ (x,y) => 
      x + y / data.length value;
    }

    (normalGrad - perturbedGrad) value
  }
}


object TestCD {
  def main(arg: Array[String]) {
    val data = (new Gaussian(3,1).samples take 1000).toSeq;
    def trans(mean: Vector) = { (x:Double) =>
      new Gaussian(mean(0),1)
    }
    def deriv(theta: Vector) = { (x:Double) => 
      (DenseVector(1)(x-theta(0)))
    }
    val opt = new ContrastiveDivergenceOptimizer[Double,Int,Vector](trans _ ,deriv _ ,0.01) with ConsoleLogging;
    
    opt.maximize(data,Array(-100.0).asVector);
  }
  
}
