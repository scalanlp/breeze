package scalanlp.optimize

import scalanlp.util._
import scalanlp.util.Log._
import scalanlp.optimize.QuasiNewtonMinimizer.{NaNHistory, StepSizeUnderflow}

import java.util.Arrays;
import scala.collection.mutable.ArrayBuffer;
import scalala.Scalala._;
import scalala.tensor._;
import scalala.tensor.operators._;
import TensorShapes._;
import scalala.tensor.dense._;

/**
 * Implements the Orthant-wise Limited Memory QuasiNewton method,
 * which is a variant of LBFGS that handles L1 regularization.
 *
 * Paper is Andrew and Gao (2007) Scalable Training of L1-Regularized Log-Linear Models
 *
 * @author dlwh
 */
class OWLQN[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]](maxIter: Int, m: Int, l1reg: Double=1.0)
  (implicit arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col])
  extends LBFGS[K,T](maxIter, m)(arith) with GradientNormConvergence[K,T] with Logged {


  require(m > 0);
  require(l1reg >= 0);
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
  override def chooseStepSize(f: DiffFunction[K,T], dir: T, grad: T, state: State) = {
    val iter = state.iter;
    val x = state.x;
    val prevVal = state.value;

    val orthantVector = computeOrthant(state.x,grad);

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

    val c1 = 0.2;
    val initAlpha = if(iter < 1) 0.5 else 1.0;
    var alpha = initAlpha;

    val c = 0.0001 * normGradInDir;

    val newX = x + dir * alpha value;

    var currentVal = f.valueAt(newX) + l1Part(newX);

    while( currentVal > prevVal + alpha * c && myIter < MAX_ITER) {
      alpha *= c1;
      newX := project(x :+ (dir * alpha),orthantVector);
      currentVal = f.valueAt(newX) + l1Part(newX);
      log(INFO)(".");
      myIter += 1;
    }
    // Give up.
    if(myIter >= MAX_ITER)
      alpha = initAlpha;

    if(alpha * norm(grad,Double.PositiveInfinity) < 1E-10)
      throw new StepSizeUnderflow;
    (alpha,currentVal)
  }

  // projects x to be on the same orthant as y
  // this basically requires that x'_i = x_i if sign(x_i) == sign(y_i), and 0 otherwise.
  private def project(x: T, y: T) = {
    val res = x.like;
    for((i,v) <- x.activeElements) {
      if(math.signum(v) == math.signum(y(i)))
        res(i) = v;
    }
    res;
  }

  protected override def initialState(f: DiffFunction[K,T], init: T):State = {
    val initState = super.initialState(f,init);
    initState.copy(value = initState.value + l1Part(init));
  }

  // Adds in the regularization stuff to the gradient
  protected override def adjustGradient(grad: T, x: T) = {
    val res = grad.like;
    for( (i,v) <- grad.activeElements) {
      val delta_+ = v + (if(x(i) == 0.0) l1reg else math.signum(x(i)) * l1reg);
      val delta_- = v + (if(x(i) == 0.0) -l1reg else math.signum(x(i)) * l1reg);

      val g = if(delta_- > 0) delta_- else if(delta_+ < 0) delta_+ else 0.0;
      res(i) = g;
    }
    res
  }

  private def computeOrthant(x: T, grad: T) = {
    val orth = x.like;
    for( (i,v) <- x.activeElements) {
      if(v != 0) orth(i) = math.signum(v);
      else orth(i) = math.signum(-grad(i));
    }
    orth
  }

  private def l1Part(x: T) = {
    var result = 0.0;
    for(v <- x.activeValues) {
      result += v.abs;
    }
    result * l1reg;
  }

}

