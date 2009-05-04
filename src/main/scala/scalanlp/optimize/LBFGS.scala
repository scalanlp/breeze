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

import scalanlp.math.Arrays._;
import util._;
import Log._;
import java.util.Arrays;
import scala.collection.mutable.ArrayBuffer;

/**
 * Port of LBFGS to Scala.
 * 
 * Special note for LBFGS:
 *  If you use it in published work, you must cite one of:
 *     * J. Nocedal. Updating  Quasi-Newton  Matrices  with  Limited  Storage
 *    (1980), Mathematics of Computation 35, pp. 773-782.
 *  * D.C. Liu and J. Nocedal. On the  Limited  mem  Method  for  Large
 *    Scale  Optimization  (1989),  Mathematical  Programming  B,  45,  3,
 *    pp. 503-528.
 *  * 
 * 
 * @param tol: tolerance of the gradient's l2 norm.
 * @param maxIter: maximum number of iterations, or <= 0 for unlimited
 * @param m: The memory of the search. 3 to 7 is usually sufficient.
 */
class LBFGS(tol: Double, maxIter: Int, m: Int) extends Minimizer[Array[Double], DiffFunction[Array[Double]]] with Logged {
  require(tol > 0);
  require(m > 0);
  private val tolSquared = tol *tol;

  import LBFGS._;
  
  def minimize(f: DiffFunction[Array[Double]], init: Array[Double]) = {
    var iter = 0; 
    var converged = false;
    val n = init.length; // number of parameters
    
    val x = new Array[Double](n);
    System.arraycopy(init,0,x,0,n);

    val memStep = new ArrayBuffer[Array[Double]];
    val memGradDelta = new ArrayBuffer[Array[Double]];
    val memRho = new ArrayBuffer[Double];
    var previousGrad = new Array[Double](n);
    var (v,grad) = f.calculate(x);

    while( (maxIter <= 0 || iter < maxIter) && !converged) {
      log(INFO)("Starting iteration: " + iter);
      log(INFO)("Current v:" + v);
      log(INFO)("Current grad:" + grad.mkString(","));
      try {
        val diag = if(memStep.size > 0) {
          computeDiag(iter,grad,memStep.last,memGradDelta.last);
        } else {
          val arr =  new Array[Double](n);
          Arrays.fill(arr,1.0);
          arr;
        }
        log(INFO)("Diag:" + diag.mkString(","));
        val step = computeDirection(iter,diag, grad, memStep, memGradDelta, memRho);
        log(INFO)("Step:" + step.mkString(","));

        val (stepScale,newVal) = chooseStepSize(iter,f, step, x, grad, v);
        log(INFO)("Scale:" +  stepScale);
        for(i <- 0 until n) {
          step(i) *= stepScale;
          x(i) += step(i);
        }
        val newGrad = f.gradientAt(x);
        log(INFO)("Current X:" + x.mkString("{",",","}"));

        memStep += step;
        val gradDelta = new Array[Double](n);
        for(i <- 0 until n) {
          gradDelta(i) = grad(i) - previousGrad(i);
        }
        memGradDelta += gradDelta;
        memRho += 1/dotProduct(step,gradDelta);
        if(iter >= m) {
          memStep.remove(0);
          memRho.remove(0);
          memGradDelta.remove(0);
        }

        previousGrad = grad;
        grad = newGrad;
        v = newVal;

        converged = checkConvergence(grad);
      } catch {
        case e: NaNHistory => 
          log(ERROR)("Something in the history is giving NaN's, clearing it!");
          memStep.clear;
          memGradDelta.clear;
          memRho.clear;
      }

      iter += 1;
    }
    x
  }

  def computeDiag(iter: Int, grad: Array[Double], prevStep: Array[Double], prevGrad: Array[Double]) = {
    if(iter == 0) {
      val arr = new Array[Double](grad.length);
      Arrays.fill(arr,1.0);
      arr 
    } else {
      val arr = new Array[Double](prevStep.length);
      val sy = dotProduct(prevStep, prevGrad);
      val yy = dotProduct(prevGrad, prevGrad);
      val syyy = if(sy < 0 || sy.isNaN) {
        1.0
      } else {
        sy/yy;
      }
      Arrays.fill(arr,syyy);
      arr
    }
  }
   
  /**
   * Find a descent direction for the current point.
   * 
   * @param iter: The iteration
   * @param grad the gradient 
   * @param memStep the history of step sizes
   * @param memGradStep the history of chagnes in gradients
   * @param memRho: 1 over the dotproduct of step and gradStep
   */
   def computeDirection(iter: Int,
      diag: Array[Double],
      grad: Array[Double],
      memStep: ArrayBuffer[Array[Double]],
      memGradStep: ArrayBuffer[Array[Double]],
      memRho: ArrayBuffer[Double]): Array[Double] = {
    val dir = new Array[Double](grad.length);
    System.arraycopy(grad,0,dir,0,dir.length);
    val as = new Array[Double](m);
    for(i <- (memStep.length-1) to 0 by -1) {
      val rho = memRho(i);
      val gradStep = memGradStep(i);
      val a = rho * dotProduct(memStep(i),dir);
      as(i) = a;
      for(j <- 0 until dir.length) {
        dir(j) -= a * gradStep(j);
      }
    }

    for(j <- 0 until dir.length) {
      if(!diag(j).isNaN) {
        dir(j) *= diag(j);
      } else {
        log(WARN)("Got a NaN diag!");
        throw new NaNHistory;
      }
    }

    for(i <- 0 until memStep.length) {
      val rho = memRho(i);
      val gradStep = memGradStep(i);
      val mStep = memStep(i);
      val a = as(i);
      val beta = rho * dotProduct(gradStep,dir);
      for(j <- 0 until dir.length) {
        dir(j) += mStep(j) * (a - beta);
      }
    }
    for(j <- 0 until dir.length) {
      dir(j) *= -1;
    }

    dir;
  } 

  
  
  /**
   * Given a direction, perform a line search to find 
   * a direction to descend. At the moment, this just executes
   * backtracking, so it does not fulfill the wolfe conditions.
   * 
   * @param f: The objective
   * @param dir: The step direction
   * @param x: The location
   * @return (stepSize, newValue)
   */
  def chooseStepSize(iter: Int,
                     f: DiffFunction[Array[Double]],
                     dir: Array[Double],
                     x: Array[Double],
                     grad: Array[Double], 
                     prevVal: Double) = {
    val normGradInDir = {
      val possibleNorm = dotProduct(dir, grad);
      if (possibleNorm > 0) { // hill climbing is not what we want. Bad LBFGS.
        log(WARN)("Direction of positive gradient chosen!");
        log(WARN)("Direction is:" + possibleNorm)
        // Reverse the direction, clearly it's a bad idea to go up
        for(i <- 0 until dir.length) {
          dir(i) *= -1;
        }
        dotProduct(dir,grad);
      } else {
        possibleNorm;
      }
    }

    val c1 = 0.5;
    var alpha = if(iter < 3) 0.05 else 1.0;

    val c = 0.001 * normGradInDir;

    val newX = new Array[Double](x.length);

    var currentVal = f.valueAt(scaleAdd(x,alpha,dir,newX));

    while( currentVal > prevVal + alpha * c) {
      alpha *= c1;
      currentVal = f.valueAt(scaleAdd(x,alpha,dir,newX));
      log(INFO)(".");
    }
    log(INFO)("Step size: " + alpha);
    (alpha,currentVal)
  }

  /**
   * Return true if the algorithm should be considered to have converged.
   * 
   * @param grad the current gradient
   */
  def checkConvergence(grad: Array[Double]): Boolean = {
    dotProduct(grad,grad) < tolSquared;
  }
}

object LBFGS {
  private sealed class LBFGSException extends RuntimeException;
  private class NaNHistory extends LBFGSException;
}

object TestLBFGS {
  def main(arg: Array[String]) {
    val lbfgs = new LBFGS(1E-8,0,7) with ConsoleLogging;
    val f = new DiffFunction[Array[Double]] {
      def valueAt(x: Array[Double]) = {
        x.map(x => (x-3) * (x-3) ).reduceLeft(_+_)
      }
      def gradientAt(x: Array[Double]) = {
        x.map(x => 2 *x -6) 
      }
    }
    
    lbfgs.minimize(f,Array(30.0,40.0)) foreach println
  }
  
}
