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

import util._;
import Log._;
import java.util.Arrays;
import scala.collection.mutable.ArrayBuffer;
import scalala.Scalala._;
import scalala.tensor.Vector;

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
class LBFGS(tol: Double, maxIter: Int, m: Int) extends Minimizer[DiffFunction] with Logged {
  require(tol > 0);
  require(m > 0);
  private val tolSquared = tol *tol;

  import LBFGS._;
  
  def minimize(f: DiffFunction, init: Vector) = {
    var iter = 0; 
    var converged = false;
    val n = init.size; // number of parameters
    
    val x = init.copy;

    val memStep = new ArrayBuffer[Vector];
    val memGradDelta = new ArrayBuffer[Vector];
    val memRho = new ArrayBuffer[Double];
    var (v,grad) = f.calculate(x);

    while( (maxIter <= 0 || iter < maxIter) && !converged) {
      log(INFO)("Starting iteration: " + iter);
      log(INFO)("Current v:" + v);
      log(INFO)("Current grad:" + grad.mkString(","));
      try {
        val diag = if(memStep.size > 0) {
          computeDiag(iter,grad,memStep.last,memGradDelta.last);
        } else {
          ones(n);
        }
        val step = computeDirection(iter,diag, grad, memStep, memGradDelta, memRho);
        log(INFO)("Step:" + step);

        val (stepScale,newVal) = chooseStepSize(iter,f, step, x, grad, v);
        log(INFO)("Scale:" +  stepScale);
        step *= stepScale;
        x += step;
        log(INFO)("Current X:" + x.mkString("{",",","}"));

        val newGrad = f.gradientAt(x);

        memStep += step;
        val gradDelta = newGrad - grad; 

        memGradDelta += gradDelta;
        memRho += 1/(step dot gradDelta);

   //     printHistory(memRho, memGradDelta, memStep);

        if(memStep.length > m) {
          memStep.remove(0);
          memRho.remove(0);
          memGradDelta.remove(0);
        }

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

  /*
  def printHistory(memRho: ArrayBuffer[Double], memGradDelta: Seq[Vector], memStep: Seq[Vector]) = {
    log(INFO)("History:");
    memRho.elements zip memGradDelta.elements zip memStep.elements foreach  (x =>log(INFO)(x))
  }
  */

  def computeDiag(iter: Int, grad: Vector, prevStep: Vector, prevGrad: Vector):Vector = {
    if(iter == 0) {
      ones(grad.size);
    } else {
      val sy = prevStep dot prevGrad;
      val yy = prevGrad dot prevGrad;
      val syyy = if(sy < 0 || sy.isNaN) {
        throw new NaNHistory;
      } else {
        sy/yy;
      }
      ones(grad.size) * sy/yy;
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
      diag: Vector,
      grad: Vector,
      memStep: ArrayBuffer[Vector],
      memGradStep: ArrayBuffer[Vector],
      memRho: ArrayBuffer[Double]): Vector = {
    val dir = grad.copy;
    val as = new Array[Double](m);

    for(i <- (memStep.length-1) to 0 by -1) {
      as(i) = memRho(i) * (memStep(i) dot dir);
      dir -= as(i) * memGradStep(i);
    }

    dir :*= diag;

    for(i <- 0 until memStep.length) {
      val beta = memRho(i) * (memGradStep(i) dot dir);
      dir += memStep(i) * (as(i) - beta);
    }

    dir *= -1;
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
                     f: DiffFunction,
                     dir: Vector,
                     x: Vector,
                     grad: Vector, 
                     prevVal: Double) = {
    val normGradInDir = {
      val possibleNorm = dir dot grad;
      if (possibleNorm > 0) { // hill climbing is not what we want. Bad LBFGS.
        log(WARN)("Direction of positive gradient chosen!");
        log(WARN)("Direction is:" + possibleNorm)
        // Reverse the direction, clearly it's a bad idea to go up
        dir *= -1;
        dir dot grad;
      } else {
        possibleNorm;
      }
    }

    val MAX_ITER = 20;
    var myIter = 0;

    val c1 = 0.1;
    var alpha = if(iter < 3) 0.05 else 1.0;

    val c = 0.001 * normGradInDir;

    val newX = x + alpha * dir;

    var currentVal = f.valueAt(newX);

    while( currentVal > prevVal + alpha * c && myIter < MAX_ITER) {
      alpha *= c1;
      newX := x + alpha * dir;
      currentVal = f.valueAt(newX);
      log(INFO)(".");
      myIter += 1;
    }
    log(INFO)("Step size: " + alpha);
    (alpha,currentVal)
  }

  /**
   * Return true if the algorithm should be considered to have converged.
   * 
   * @param grad the current gradient
   */
  def checkConvergence(grad: Vector): Boolean = {
    norm(grad,2) < tolSquared;
  }
}

object LBFGS {
  private sealed class LBFGSException extends RuntimeException;
  private class NaNHistory extends LBFGSException;
}

object TestLBFGS {
  def main(arg: Array[String]) {
    val lbfgs = new LBFGS(1E-8,0,7) with ConsoleLogging;
    val f = new DiffFunction {
      def valueAt(x: Vector) = {
        norm((x -3) :^ 2,1)
      }
      def gradientAt(x: Vector):Vector = {
        (x * 2) - 6;
      }
    }

    val v = ones(2);
    v(0) = 30.
    v(1) = 40.
    
    lbfgs.minimize(f,v) foreach println
  }
  
}
