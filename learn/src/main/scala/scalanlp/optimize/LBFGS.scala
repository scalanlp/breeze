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

import scalanlp.util._
import scalanlp.util.Log._
import scalanlp.optimize.QuasiNewtonMinimizer.{LineSearchFailed, NaNHistory, StepSizeUnderflow};
import scalala.generic.math.CanNorm
import scalala.generic.collection.{CanCopy, CanCreateZerosLike}
import scalala.operators._
import scalala.library.Library.norm
import scalala.tensor.mutable.Counter
;

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
class LBFGS[T](override val maxIter: Int, m: Int)(implicit protected val canNorm: CanNorm[T],
                                                  protected val view: T=>MutableNumericOps[T],
                                                  copy: CanCopy[T],
                                                  zeros: CanCreateZerosLike[T,T],
                                                  protected val opAdd: BinaryOp[T,T,OpAdd,T],
                                                  opAddScalar: BinaryOp[T,Double,OpAdd,T],
                                                  upAdd: BinaryUpdateOp[T,T,OpAdd],
                                                  opDivScalar: BinaryOp[T,Double,OpDiv,T],
                                                  protected val opMulScalar: BinaryOp[T,Double,OpMul,T],
                                                  upMulScalar: BinaryUpdateOp[T,Double,OpMul],
                                                  upMul: BinaryUpdateOp[T,T,OpMul],
                                                  innerProduct: BinaryOp[T,T,OpMulInner,Double],
                                                  upSub: BinaryUpdateOp[T,T,OpSub],
                                                  opSub: BinaryOp[T,T,OpSub,T]) extends QuasiNewtonMinimizer[T] with GradientNormConvergence[T] with Logged {
  require(m > 0);

  class History(private[LBFGS] val memStep: IndexedSeq[T] = IndexedSeq.empty,
                private[LBFGS] val memGradDelta: IndexedSeq[T] = IndexedSeq.empty,
                private[LBFGS] val memRho: IndexedSeq[Double] = IndexedSeq.empty);


  protected def initialHistory(grad: T):History = new History();
  protected def chooseDescentDirection(grad: T, state: State):T = {
    val memStep = state.history.memStep;
    val memGradDelta = state.history.memGradDelta;
    val memRho = state.history.memRho;
    val diag = if(memStep.size > 0) {
      computeDiag(state.iter,grad,memStep.last,memGradDelta.last);
    } else {
      zeros(grad) + 1.;
    }

    val dir = copy(grad)
    val as = new Array[Double](m);

    for(i <- (memStep.length-1) to 0 by -1) {
      as(i) = (memStep(i) dot dir)/memRho(i);
      if(as(i).isNaN) {
        error("NaN!" + (memStep(i) dot dir) + " " + memRho(i));
      }
      assert(!as(i).isInfinite, memRho(i) -> norm(grad,2));
      dir -= memGradDelta(i) * as(i);
    }

    dir :*= diag;

    for(i <- 0 until memStep.length) {
      val beta = (memGradDelta(i) dot dir)/memRho(i);
      dir += memStep(i) * (as(i) - beta);
    }

    dir *= -1.0;
    dir;
  }

  protected def adjustGradient(grad: T, x: T) = grad;

  protected def updateHistory(oldState: State, newGrad: T, newVal: Double, step: T): History = {
    val gradDelta : T = (newGrad :- oldState.grad);

    var memStep = oldState.history.memStep :+ step;
    var memGradDelta = oldState.history.memGradDelta :+ gradDelta;
    var memRho = oldState.history.memRho :+ (step dot gradDelta);
    println(step -> gradDelta -> memRho.last -> newGrad -> oldState.grad);
//    assert(memRho.last != 0.0, (step -> gradDelta -> memRho.last));

    if(memStep.length > m) {
      memStep = memStep.drop(1);
      memRho = memRho.drop(1);
      memGradDelta = memGradDelta.drop(1);
    }

    new History(memStep,memGradDelta,memRho);
  }

  private def computeDiag(iter: Int, grad: T, prevStep: T, prevGrad: T):T = {
    if(iter == 0) {
      zeros(grad) + 1
    } else {
      val sy = prevStep dot prevGrad;
      val yy = prevGrad dot prevGrad;
      val syyy = if(sy < 0 || sy.isNaN) {
        throw new NaNHistory;
      } else {
        sy/yy;
      }
     (zeros(grad) + 1) * (sy/yy)
    }
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
  def chooseStepSize(f: DiffFunction[T], dir: T, grad: T, state: State) = {
    val iter = state.iter;
    val x = state.x;
    val prevVal = state.value;

    val normGradInDir = {
      val possibleNorm = dir dot grad;
      if (possibleNorm > 0) { // hill climbing is not what we want. Bad LBFGS.
        log(WARN)("Direction of positive gradient chosen!");
        log(WARN)("Direction is:" + possibleNorm)
        // Reverse the direction, clearly it's a bad idea to go up
        dir *= -1.0;
        dir dot grad;
      } else {
        possibleNorm;
      }
    }

    val MAX_ITER = 20;
    var myIter = 0;

    val c1 = 0.2;
    val initAlpha = if(iter < 1) 0.5 else 1.0;
    var alpha = initAlpha;

    val c = 0.0001 * normGradInDir;

    var newX = x + dir * alpha;

    var currentVal = f.valueAt(newX);

    while( currentVal > prevVal + alpha * c && myIter < MAX_ITER) {
      alpha *= c1;
      newX = (x :+ (dir * alpha));
      currentVal = f.valueAt(newX);
      log(INFO)(".");
      myIter += 1;
    }
    // Give up.
    if(myIter >= MAX_ITER)
      throw new LineSearchFailed;

    if(alpha * norm(grad,Double.PositiveInfinity) < 1E-10)
      throw new StepSizeUnderflow;
    log(INFO)("Step size: " + alpha);
    (alpha,currentVal)
  }

}

object LBFGS {
  def main(args: Array[String]) {
    val lbfgs = new LBFGS[Counter[Int,Double]](100,4);

    def optimizeThis(init: Counter[Int,Double]) = {
      val f = new DiffFunction[Counter[Int,Double]] {
        def calculate(x: Counter[Int,Double]) = {
          (norm((x -3) :^ 2,1),(x * 2) - 6);
        }
      }

      val result = lbfgs.minimize(f,init)
      println(norm(result - 3,2) < 1E-10)
    }

    optimizeThis(Counter(1->1.,2->2.,3->3.))
    optimizeThis(Counter(3-> -2.,2->3.,1-> -10.))
  }
}

