package breeze.optimize

import breeze.util._
import breeze.linalg._
import breeze.numerics._
import breeze.math.MutableCoordinateSpace
import com.typesafe.scalalogging.slf4j.Logging


/**
 * Implements the Orthant-wise Limited Memory QuasiNewton method,
 * which is a variant of LBFGS that handles L1 regularization.
 *
 * Paper is Andrew and Gao (2007) Scalable Training of L1-Regularized Log-Linear Models
 *
 * @author dlwh
 */
class OWLQN[T](maxIter: Int, m: Int,  l1reg: Double=1.0, tolerance: Double = 1E-8)(implicit vspace: MutableCoordinateSpace[T, Double]) extends LBFGS[T](maxIter, m, tolerance=tolerance) with  Logging {
  import vspace._
  require(m > 0)
  require(l1reg >= 0)


  override protected def chooseDescentDirection(state: State, fn: DiffFunction[T]) = {
    super.chooseDescentDirection(state.copy(grad = state.adjustedGradient), fn)
  }

  override protected def determineStepSize(state: State, f: DiffFunction[T], dir: T) = {
    val iter = state.iter

    val normGradInDir = {
      val possibleNorm = dir dot state.grad
//      if (possibleNorm > 0) { // hill climbing is not what we want. Bad LBFGS.
//        logger.warn("Direction of positive gradient chosen!")
//        logger.warn("Direction is:" + possibleNorm)
//        Reverse the direction, clearly it's a bad idea to go up
//        dir *= -1.0
//        dir dot state.grad
//      } else {
        possibleNorm
//      }
    }

    val ff = new DiffFunction[Double] {
       def calculate(alpha: Double) = {
         val newX = takeStep(state, dir, alpha)
         val (v, newG) =  f.calculate(newX)
         val (adjv, adjgrad) = adjust(newX, newG, v)
         // TODO not sure if this is quite right...
         adjv -> (adjgrad dot dir)
       }
    }
    val search = new BacktrackingLineSearch(shrinkStep= if(iter < 1) 0.1 else 0.5)
    val alpha = search.minimize(ff, if(iter < 1) .5/norm(state.grad) else 1.0)

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
    val adjValue = newVal + l1reg * norm(newX, 1)
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

    //    optimizeThis(Counter(1->1.0,2->2.0,3->3.0))
    //    optimizeThis(Counter(3-> -2.0,2->3.0,1-> -10.0))
    //        optimizeThis(DenseVector(1.0,2.0,3.0))
    optimizeThis(DenseVector( -0.0,0.0, -0.0))
  }
}

