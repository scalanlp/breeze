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
  
  def minimize(f: DiffFunction[Array[Double]], init: Array[Double]) = {
    var k = 0;
    var converged = false;
    val n = init.length;
    
    val x = new Array[Double](n);
    System.arraycopy(init,0,x,0,n);

    val memStep = new Array[Array[Double]](m);
    val memGradDelta = new Array[Array[Double]](m);
    val memRho = new Array[Double](m);
    var previousGrad = new Array[Double](n);
    var (v,grad) = f.calculate(x);

    while( (maxIter <= 0 || k < maxIter) && !converged) {
      log(INFO)("Starting iteration: " + k);
      log(INFO)("Current v:" + v);
      log(INFO)("Current grad:" + grad.mkString(","));

      val diag = computeDiag(k,grad,memStep((k+m-1)%m),memGradDelta((k+m-1)%m));
      log(INFO)("Diag:" + diag.mkString(","));
      val step = computeDirection(k,diag, grad, memStep, memGradDelta, memRho);
      log(INFO)("Step:" + step.mkString(","));

      val (stepScale,newVal,newGrad) = chooseStepSize(f, step, x, grad, v);
      log(INFO)("Scale:" +  stepScale);
      for(i <- 0 until n) {
        step(i) *= stepScale;
        x(i) += step(i);
      }
      log(INFO)("Current X:" + x.mkString("{",",","}"));

      memStep(k%m) = step;
      if(k < m) {
        memGradDelta(k) = new Array[Double](n);
      }
      for(i <- 0 until n) {
        memGradDelta(k%m)(i) = grad(i) - previousGrad(i);
      }
      memStep(k%m) = step;
      memRho(k%m) = -1/dotProduct(step,memGradDelta(k%m));

      previousGrad = grad;
      grad = newGrad;
      v = newVal;

      converged = checkConvergence(grad);

      k += 1;
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
      val yy = dotProduct(prevGrad, prevStep);
      Arrays.fill(arr,sy/yy);
      arr
    }
  }
   
  /**
   * Find a descent direction for the current point.
   * 
   * @param k: The iteration
   * @param grad the gradient 
   * @param memStep the history of step sizes
   * @param memGradStep the history of chagnes in gradients
   * @param memRho: 1 over the dotproduct of step and gradStep
   */
  def computeDirection(k: Int,
      diag: Array[Double],
      grad: Array[Double],
      memStep: Array[Array[Double]],
      memGradStep: Array[Array[Double]],
      memRho: Array[Double]): Array[Double] = {
    val step = new Array[Double](grad.length);
    System.arraycopy(grad,0,step,0,step.length);
    val as = new Array[Double](m);
    for(i <- 1 until m 
        if (k+ i) % m < k) {
      val pos = (k + i) % m;
      val rho = memRho(pos);
      val gradStep = memGradStep(pos);
      val a = rho * dotProduct(memStep(pos),step);
      as(i-1) = a;
      for(j <- 0 until step.length) {
        step(j) -= a * gradStep(j);
      }
    }

    for(j <- 0 until step.length) {
      step(j) *= diag(j);
    }

    for(i <- m until 1 by -1
        if (k+ i) % m < k ) {
      val pos = (k + i)%m;
      val rho = memRho(pos);
      val gradStep = memGradStep(pos);
      val mStep = memStep(pos);
      val a = as(i-1);
      val beta = rho * dotProduct(gradStep,step);
      for(j <- 0 until step.length) {
        step(j) += mStep(j) * (a - beta);
      }
    }
    for(j <- 0 until step.length) {
      step(j) *= -1;
    }

    step;
  }
  
  /**
   * Given a direction, perform a line search to find 
   * a direction to descend. At the moment, this just executes
   * backtracking, so it does not fulfill the wolfe conditions.
   * 
   * @param f: The objective
   * @param dir: The step direction
   * @param x: The location
   * @return (stepSize, newValue, newGradient)
   */
  def chooseStepSize(f: DiffFunction[Array[Double]],
                     dir: Array[Double],
                     x: Array[Double],
                     grad: Array[Double], 
                     prevVal: Double) = {
    val normGradInDir = dotProduct(dir, grad);
    if (normGradInDir > 0) {
      log(WARN)("Positive gradient chosen!");
      log(WARN)("Direction is:" + normGradInDir)
    }

    val c1 = 0.1;
    var alpha = 1.0;

    val c = 0.001 * normGradInDir;

    val newX = new Array[Double](x.length);

    var (currentVal,currentGrad) = f.calculate(scaleAdd(x,alpha,dir,newX));

    while( currentVal > prevVal + alpha * c) {
      alpha = c1 * alpha;
      val valGrad = f.calculate(scaleAdd(x,alpha,dir,newX));
      currentVal = valGrad._1;
      currentGrad = valGrad._2;
    }
    (alpha,currentVal,currentGrad)
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
    
    lbfgs.minimize(f,Array(8.0,6.0)) foreach println
  }
  
}
