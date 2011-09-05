package scalanlp.optimize

import scalanlp.util._
import logging.{ConfiguredLogging}
import scalanlp.optimize.QuasiNewtonMinimizer.StepSizeUnderflow
import scalala.library.Library.norm
import scalala.generic.math.CanNorm
import scalala.operators._
import bundles.MutableInnerProductSpace
import scalala.tensor.mutable.Tensor1
import scalala.tensor.dense.DenseVector


/**
 * Implements the Orthant-wise Limited Memory QuasiNewton method,
 * which is a variant of LBFGS that handles L1 regularization.
 *
 * Paper is Andrew and Gao (2007) Scalable Training of L1-Regularized Log-Linear Models
 *
 * @author dlwh
 */
class OWLQN[K,T](maxIter: Int, m: Int, l1reg: Double=1.0)(implicit vspace: MutableInnerProductSpace[Double,T],
                                                  view: T <:< Tensor1[K,Double] with scalala.tensor.mutable.TensorLike[K,Double,_,T with Tensor1[K,Double]],
                                                  canNorm: CanNorm[T]) extends LBFGS[T](maxIter, m) with GradientNormConvergence[T] with ConfiguredLogging {
  import vspace._;
  require(m > 0);
  require(l1reg >= 0);
  override def chooseStepSize(f: DiffFunction[T], dir: T, grad: T, state: State) = {
    val iter = state.iter;
    val x = state.x;

    val normGradInDir = {
      val possibleNorm = dir dot grad;
      if (possibleNorm > 0) { // hill climbing is not what we want. Bad LBFGS.
        log.warn("Direction of positive gradient chosen!");
        log.warn("Direction is:" + possibleNorm)
        // Reverse the direction, clearly it's a bad idea to go up
        dir *= -1.0;
        dir dot grad;
      } else {
        possibleNorm;
      }
    }

    def ff(alpha: Double) = {
      val newX = project(x :+ (dir * alpha), grad, state)
      val v =  f.valueAt(newX)
      adjustValue(v, newX)
    };
    val search = new BacktrackingLineSearch(initAlpha = if (iter <= 1) 0.5 else 1.0)
    val iterates = search.iterations(ff)
    val targetState = iterates.find { case search.State(alpha,v) =>
      log.info(".");
      // sufficient descent
      v < state.adjustedValue + alpha * 0.0001 * normGradInDir
    }

    val alpha = (for(search.State(alpha,currentVal) <- targetState) yield {
      if(alpha > 0 && alpha * norm(grad,Double.PositiveInfinity) < 1E-10)
        throw new StepSizeUnderflow;
      log.info("Step size: " + alpha);
      alpha
    }) getOrElse(0.0)

    alpha
  }

  // projects x to be on the same orthant as y
  // this basically requires that x'_i = x_i if sign(x_i) == sign(y_i), and 0 otherwise.
  override protected def project(x: T, grad: T, state: State):T = {
    val orthant = computeOrthant(state.x,grad);
    val res = zeros(x);
    for( (k,v) <- x.pairsIteratorNonZero) {
      if(math.signum(v) == math.signum(orthant(k))) {
        res(k) = v;
      }
    }
    res;
  }

  protected override def initialState(f: DiffFunction[T], init: T):State = {
    val initState = super.initialState(f,init);
    initState.copy(value = initState.value + norm(init,1) * l1reg);
  }

  // Adds in the regularization stuff to the gradient
  protected override def adjustGradient(grad: T, x: T) = {
    val res = zeros(grad);
    for( (i,v) <- grad.nonzero.pairs) {
      val delta_+ = v + (if(x(i) == 0.0) l1reg else math.signum(x(i)) * l1reg);
      val delta_- = v + (if(x(i) == 0.0) -l1reg else math.signum(x(i)) * l1reg);

      val g = if(delta_- > 0) delta_- else if(delta_+ < 0) delta_+ else 0.0;
      res(i) = g;
    }
    res
  }

  override protected def adjustValue(value: Double, x: T) = {
    value + l1reg * norm(x,1);
  }

  private def computeOrthant(x: T, grad: T) = {
    val orth = zeros(x);
    for( (i,v) <- x.nonzero.pairs) {
      if(v != 0) orth(i) = math.signum(v);
      else orth(i) = math.signum(-grad(i));
    }
    orth
  }

}


object OWLQN {
  def main(args: Array[String]) {
    val lbfgs = new OWLQN[Int,DenseVector[Double]](100,4);

    def optimizeThis(init: DenseVector[Double]) = {
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (norm((x -3) :^ 2,1),(x * 2) - 6);
        }
      }

      val result = lbfgs.minimize(f,init)
    }

    //    optimizeThis(Counter(1->1.,2->2.,3->3.))
    //    optimizeThis(Counter(3-> -2.,2->3.,1-> -10.))
    //        optimizeThis(DenseVector(1.,2.,3.))
    optimizeThis(DenseVector( -0.,0.0, -0.))
  }
}

