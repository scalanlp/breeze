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
import scalala.tensor._;
import scalala.tensor.dense.DenseVector;
import scalala.library.Library._;

import scalanlp.stats.distributions._;

import scalanlp.util._;
import logging.{ConsoleLogging, Logged}

import scalala.generic.collection.CanCreateZerosLike
import scalala.operators._
import scalala.generic.math.CanNorm

/**
* Implements Contrastive Divergence for maximizing the probability of parameters.
*
* see www.robots.ox.ac.uk/~ojw/files/NotesOnCD.pdf 
*
* @param X the type of the data
* @param T the type of the tensor
* @param trans: transition kernel from X to a new X, given parameters Array[Double
* @param deriv: derivative of the energy function f, where f = log p(x), up to some constant.
* @param learningRate: to slow the gradient descent, or whatever.
* @author dlwh
*/
class ContrastiveDivergenceOptimizer[X,T](trans: T=>X=>Rand[X],
        deriv: T=>X=>T,
        learningRate: Double)
        (implicit zeros: CanCreateZerosLike[T,T],
        view: T => MutableNumericOps[T],
        upAdd: BinaryUpdateOp[T,T,OpAdd],
        opAdd: BinaryOp[T,T,OpAdd,T],
        opSub: BinaryOp[T,T,OpSub,T],
        opMulScalar: BinaryOp[T,Double,OpMul,T],
        opDivScalar: BinaryOp[T,Double,OpDiv,T],
        canNorm: CanNorm[T]
        ) extends Logged {
  /**
  * Run CD to convergence on the given data with initial parameters.
  */
  def maximize(data: Seq[X], init: T) = {
    val cur: T = init;
	  var converged = false;
    var i = 0;
    do {
      log.info("Starting iteration: " +i);
      log.info("Current v:" + cur);
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
    val normalGrad = (data map thetaDeriv).foldLeft(zeros(theta)){ (x,y) =>
      x + y / (1.0 * data.length);
    } 
    val perturbedGrad = (perturbedData map thetaDeriv).foldLeft(zeros(theta)){ (x,y) =>
      x + y / (1.0 * data.length);
    }

    (normalGrad - perturbedGrad)
  }
}


object TestCD {
  def main(arg: Array[String]) {
    val data = (new Gaussian(3,1).samples take 1000).toSeq;
    def trans(mean: DenseVector[Double]) = { (x:Double) =>
      new Gaussian(mean(0),1)
    }
    def deriv(theta: DenseVector[Double]) = { (x:Double) => DenseVector(x-theta(0)) }
    val opt = new ContrastiveDivergenceOptimizer[Double,DenseVector[Double]](trans _ ,deriv _ ,0.01) with ConsoleLogging;

    opt.maximize(data,DenseVector(-10.0));
    println(mean(data))
  }
  
}
