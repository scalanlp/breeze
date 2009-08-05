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

import scalanlp.util._;
import scalanlp.util.Log._;
import java.util.Arrays;
import scala.collection.mutable.ArrayBuffer;
import scalala.Scalala._;
import scalala.tensor._;
import scalala.tensor.operators._;
import TensorShapes._;
import scalala.tensor.dense._;


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
 * @param maxIter: maximum number of iterations, or &lt;= 0 for unlimited
 * @param m: The memory of the search. 3 to 7 is usually sufficient.
 */
class LBFGS[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]](tol: Double, maxIter: Int, m: Int)
  (implicit private val op: TensorBuilder[T], arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col])
  extends Minimizer[T,DiffFunction[K,T]] with Logged {
  require(tol > 0);
  require(m > 0);
  private val tolSquared = tol *tol;

  import LBFGS._;
  
  def minimize(f: DiffFunction[K,T], init: T) = {
    var iter = 0; 
    var converged = false;
    val n = init.domain.size; // number of parameters
    
    val x : T = init.copy;

    val memStep = new ArrayBuffer[T];
    val memGradDelta = new ArrayBuffer[T];
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
          val ones : T = grad.like;
          ones += 1;
          ones
        }
        val step = computeDirection(iter, diag, grad, memStep, memGradDelta, memRho);
        log(INFO)("Step:" + step);

        val (stepScale,newVal) = chooseStepSize(iter,f, step, x, grad, v);
        log(INFO)("Scale:" +  stepScale);
        step *= stepScale;
        x += step;
        log(INFO)("Current X:" + x.mkString("{",",","}"));

        val newGrad = f.gradientAt(x);

        memStep += step;
        val gradDelta : T = newGrad.like;
        gradDelta :-= grad;

        memGradDelta += gradDelta;
        memRho += 1/(step dot gradDelta);

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
          memStep.clear();
          memGradDelta.clear();
          memRho.clear();
      }

      iter += 1;
    }
    x
  }

  def computeDiag(iter: Int, grad: T, prevStep: T, prevGrad: T):T = {
    if(iter == 0) {
      grad :== grad value;
    } else {
      val sy = prevStep dot prevGrad;
      val yy = prevGrad dot prevGrad;
      val syyy = if(sy < 0 || sy.isNaN) {
        throw new NaNHistory;
      } else {
        sy/yy;
      }
     (((grad :== grad) * sy/yy) value);
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
      diag: T,
      grad: T,
      memStep: ArrayBuffer[T],
      memGradStep: ArrayBuffer[T],
      memRho: ArrayBuffer[Double]): T = {
    val dir = op.like(grad);
    dir := grad;
    val as = new Array[Double](m);

    for(i <- (memStep.length-1) to 0 by -1) {
      as(i) = memRho(i) * (memStep(i) dot dir);
      dir -= memGradStep(i) * as(i);
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
                     f: DiffFunction[K,T],
                     dir: T,
                     x: T,
                     grad: T, 
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

    val newX = x + dir * alpha value;

    var currentVal = f.valueAt(newX);

    while( currentVal > prevVal + alpha * c && myIter < MAX_ITER) {
      alpha *= c1;
      newX := (x :+ (dir * alpha));
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
  def checkConvergence(grad: T): Boolean = {
    norm(grad,2) < tolSquared;
  }
}

object LBFGS {
  private sealed class LBFGSException extends RuntimeException;
  private class NaNHistory extends LBFGSException;
}

object TestLBFGS {
  def main(arg: Array[String]) {
    val lbfgs = new LBFGS[Int,DenseVector](1E-8,0,7) with ConsoleLogging;
    val f = new DiffFunction[Int,DenseVector] {
      def valueAt(x: DenseVector) = {
        norm((x -3) :^ 2,1)
      }
      def gradientAt(x: DenseVector):DenseVector = {
        (x * 2) - 6 value;
      }
    }

    val v = ones(2);
    v(0) = 30.
    v(1) = 40.
    
    lbfgs.minimize(f,v) foreach println
    test2();
  }

  def test2() {
    import scalanlp.counters.Counters._
    val lbfgs = new LBFGS[String,DoubleCounter[String]](1E-8,0,7) with ConsoleLogging;
    val f = new DiffFunction[String,DoubleCounter[String]] {
      def valueAt(x: DoubleCounter[String]) = {
        norm((x -3) :^ 2,1)
      }
      def gradientAt(x: DoubleCounter[String]):DoubleCounter[String] = {
       ( (x * 2) - 6 ).value.asInstanceOf[DoubleCounter[String]];
      }
    }

    val v = DoubleCounter[String];
    v("0") = 30.
    v("1") = 40.
    
    lbfgs.minimize(f,v) foreach println

  }
  
}
