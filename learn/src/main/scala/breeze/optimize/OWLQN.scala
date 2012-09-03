package breeze.optimize

import breeze.util._
import logging.{ConsoleLogging, ConfiguredLogging}
import breeze.linalg._
import breeze.numerics._
import breeze.math.MutableCoordinateSpace


/**
 * Implements the Orthant-wise Limited Memory QuasiNewton method,
 * which is a variant of LBFGS that handles L1 regularization.
 *
 * Paper is Andrew and Gao (2007) Scalable Training of L1-Regularized Log-Linear Models
 *
 * @author dlwh
 */
class OWLQN[T](maxIter: Int, m: Int, l1reg: Double=1.0)(implicit vspace: MutableCoordinateSpace[T, Double]) extends LBFGS[T](maxIter, m) with  ConfiguredLogging {
  import vspace._
  require(m > 0)
  require(l1reg >= 0)


  override protected def chooseDescentDirection(state: State) = {
    super.chooseDescentDirection(state.copy(grad = state.adjustedGradient))
  }

  override protected def determineStepSize(state: State, f: DiffFunction[T], dir: T) = {
    val iter = state.iter

    val normGradInDir = {
      val possibleNorm = dir dot state.grad
//      if (possibleNorm > 0) { // hill climbing is not what we want. Bad LBFGS.
//        log.warn("Direction of positive gradient chosen!")
//        log.warn("Direction is:" + possibleNorm)
//        Reverse the direction, clearly it's a bad idea to go up
//        dir *= -1.0
//        dir dot state.grad
//      } else {
        possibleNorm
//      }
    }

    def ff(alpha: Double) = {
      val newX = takeStep(state, dir, alpha)
      val v =  f.valueAt(newX)
      v + l1reg * norm(newX,1)
    }
    val search = new BacktrackingLineSearch(initAlpha = if (iter < 1) 0.5/norm(state.grad) else 1.0, cScale= if(iter < 1) 0.1 else 0.5)
    val iterates = search.iterations(ff)
    val targetState = iterates.find { case search.State(alpha,v) =>
      // sufficient descent
      val r = v < state.adjustedValue + alpha * 0.0001 * normGradInDir
      if(!r) log.info(".")
      r
    }

    val alpha = (for(search.State(alpha,currentVal) <- targetState) yield {
      if(alpha > 0 && alpha * norm(state.grad,Double.PositiveInfinity) < 1E-10)
        throw new StepSizeUnderflow
      alpha
    }) getOrElse(0.0)

    alpha
  }

  // projects x to be on the same orthant as y
  // this basically requires that x'_i = x_i if sign(x_i) == sign(y_i), and 0 otherwise.

  override protected def takeStep(state: State, dir: T, stepSize: Double) = {
    val stepped = state.x + dir * stepSize
    val orthant = computeOrthant(state.x,state.grad)
    vspace.zipMapValues.map(stepped, orthant, { case (v, ov) =>
      v * I(math.signum(v) == math.signum(ov))
    })
  }

  // Adds in the regularization stuff to the gradient
  override protected def adjust(newX: T, newGrad: T, newVal: Double) = {
    val res = vspace.zipMapValues.map(newX, newGrad, {case (xv, v) =>
      val delta_+ = v + (if(xv == 0.0) l1reg else math.signum(xv) * l1reg)
      val delta_- = v + (if(xv == 0.0) -l1reg else math.signum(xv) * l1reg)

      val g = if(delta_- > 0) delta_- else if(delta_+ < 0) delta_+ else 0.0
      g
    })
    val adjValue = newVal + l1reg * norm(newX,1)
    adjValue -> res
  }

  private def computeOrthant(x: T, grad: T) = {
    val orth = vspace.zipMapValues.map(x, grad, {case (v, gv) =>
      if(v != 0) math.signum(v)
      else math.signum(-gv)
    })
    orth
  }

}


object OWLQN {
  def main(args: Array[String]) {
    val lbfgs = new OWLQN[DenseVector[Double]](100,4)

    def optimizeThis(init: DenseVector[Double]) = {
      val f = new DiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double]) = {
          (((x - 3.0) :^ 2.0).sum,(x * 2.0) - 6.0)
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

