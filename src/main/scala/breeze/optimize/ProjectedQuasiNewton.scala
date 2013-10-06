package breeze.optimize

import breeze.linalg._
import com.typesafe.scalalogging.log4j.Logging
import breeze.collection.mutable.RingBuffer

// Compact representation of an n x n Hessian, maintained via L-BFGS updates
class CompactHessian(M: DenseMatrix[Double], Y: RingBuffer[DenseVector[Double]], S: RingBuffer[DenseVector[Double]], sigma: Double, m: Int) extends NumericOps[CompactHessian] {
  def this(m: Int) = this(null, new RingBuffer(m), new RingBuffer(m), 1.0, m)
  def repr: CompactHessian = this

  def update(y: DenseVector[Double],
             s: DenseVector[Double]):CompactHessian = {
    // Compute scaling factor for initial Hessian, which we choose as
    val yTs = y.dot(s)

    if (yTs < 1e-10) // ensure B remains strictly positive definite
      return this

    val S = this.S :+ s
    val Y = this.Y :+ y
    val sigma = 1.0 / (yTs / y.dot(y))

    val k = Y.size


    // D_k is the k x k diagonal matrix D_k = diag [s_0^Ty_0, ...,s_{k-1}^Ty_{k-1}].
    // L_k is the k x k matrix with (L_k)_{i,j} = if( i > j ) s_i^T y_j else 0
    // (this is a lower triangular matrix with the diagonal set to all zeroes)
    val D = diag(DenseVector.tabulate[Double](k){ i => S(i) dot Y(i)})
    val L = DenseMatrix.tabulate[Double](k, k) { (i, j) =>
      if(i > j) {
        S(i) dot Y(j)
      } else {
        0.0
      }
    }

    // S_k^T S_k is the symmetric k x k matrix with element (i,j) given by <s_i, s_j>
    val STS = DenseMatrix.tabulate[Double](k, k){ (i, j) =>
      sigma * (S(i) dot S(j))
    }


    // M is the 2k x 2k matrix given by: M = [ \sigma * S_k^T S_k    L_k ]
    //                                       [         L_k^T        -D_k ]
    val M = DenseMatrix.vertcat(DenseMatrix.horzcat(STS, L), DenseMatrix.horzcat(L.t, -D))


    val newB = new CompactHessian(M, S, Y, sigma, m)
    newB
  }

  def times(v: DenseVector[Double]): DenseVector[Double] = {
    if (Y.size == 0) {
      v
    } else {
      val temp = v * sigma
      subtractNv(temp, M \ (NTv(v)))
    }
  }

  // Returns a 2k x 1 matrix
  def NTv(v: DenseVector[Double]): DenseMatrix[Double] = {
    val ntv = DenseMatrix.zeros[Double](2 * Y.size, 1)

    val f1 = {
      var i = 0
      for (s <- S) {
        ntv.update(i, 0, s.dot(v) * sigma)
        i += 1
      }
    }
    val f2 =  {
      var i = S.size
      for (y <- Y) {
        ntv.update(i, 0, y.dot(v))
        i += 1
      }
    }
    return ntv
  }

  def subtractNv(subFrom: DenseVector[Double], v: Matrix[Double]): DenseVector[Double] = {
    var i = 0
    for (s <- S) {
      subFrom := subFrom + s * (-sigma * v(i, 0))
      i += 1
    }
    for (y <- Y) {
      subFrom := subFrom + y * (-v(i, 0))
      i += 1
    }
    return subFrom
  }
}



class ProjectedQuasiNewton(val optTol: Double = 1e-3,
                           val m: Int = 10,
                           val initFeas: Boolean = false,
                           val testOpt: Boolean = true,
                           val maxNumIt: Int = 2000,
                           val maxSrchIt: Int = 30,
                           val gamma: Double = 1e-10,
                           val projection: DenseVector[Double] => DenseVector[Double] = identity) extends FirstOrderMinimizer[DenseVector[Double], DiffFunction[DenseVector[Double]]] with Logging {

  case class History(B: CompactHessian)


  protected def initialHistory(f: DiffFunction[DenseVector[Double]], init: DenseVector[Double]): History = {
    History(new CompactHessian(m))
  }

  private def computeGradient(x: DenseVector[Double], g: DenseVector[Double]): DenseVector[Double] = projection(x - g) - x
  private def computeGradientNorm(x: DenseVector[Double], g: DenseVector[Double]): Double = computeGradient(x, g).norm(Double.PositiveInfinity)

  protected def chooseDescentDirection(state: State, fn: DiffFunction[DenseVector[Double]]): DenseVector[Double] = {
    import state._
    import history.B
    if (iter == 1) {
      val gnorm = computeGradientNorm(x, grad)
      computeGradient(x, grad * (0.1 / gnorm))
    } else {
      // Update the limited-memory BFGS approximation to the Hessian
      //B.update(y, s)
      // Solve subproblem; we use the current iterate x as a guess
      val subprob = new ProjectedQuasiNewton.QuadraticSubproblem(fn, state.adjustedValue, x, grad, B)
      val p = new SpectralProjectedGradient(
        testOpt = false,
        optTol = 1e-3,
        maxNumIt = 30,
        initFeas = true,
        M = 5,
        projection = projection
      ).minimize(subprob, x)
      p - x
      //	time += subprob.time
    }
  }


  protected def determineStepSize(state: State, fn: DiffFunction[DenseVector[Double]], dir: DenseVector[Double]): Double = {
    val dirnorm = dir.norm(Double.PositiveInfinity)
    if(dirnorm < 1E-10) return 0.0
    import state._
    // Backtracking line-search
    var accepted = false
    var lambda = 1.0
    val gTd = grad dot dir
    var srchit = 0

    do {
      val candx = x + dir * lambda
      val candf = fn.valueAt(candx)
      val suffdec = gamma * lambda * gTd

      if (testOpt && srchit > 0) {
        logger.debug(f"PQN:    SrchIt $srchit%4d: f $candf%-10.4f t $lambda%-10.4f\n")
      }

      if (candf < state.adjustedValue + suffdec) {
        accepted = true
      } else if (srchit >= maxSrchIt) {
        accepted = true
      } else {
        lambda *= 0.5
        srchit = srchit + 1
      }
    } while (!accepted)

    if (srchit >= maxSrchIt) {
      logger.info("PQN: Line search cannot make further progress")
      throw new LineSearchFailed(state.grad.norm(Double.PositiveInfinity), dir.norm(Double.PositiveInfinity))
    }
    lambda
  }


  protected def takeStep(state: State, dir: DenseVector[Double], stepSize: Double): DenseVector[Double] = {
    projection(state.x + dir * stepSize)
  }


  protected def updateHistory(newX: DenseVector[Double], newGrad: DenseVector[Double], newVal: Double,  f: DiffFunction[DenseVector[Double]], oldState: State): History = {
    import oldState._
    val s = newX - oldState.x
    val y = newGrad - oldState.grad
    History(oldState.history.B.update(y, s))
  }

}

object ProjectedQuasiNewton {
  // Forms a quadratic model around fun, the argmin of which is then a feasible
  // quasi-Newton descent direction
  class QuadraticSubproblem(fun: DiffFunction[DenseVector[Double]],
                            fk: Double,
                            xk: DenseVector[Double],
                            gk: DenseVector[Double],
                            B: CompactHessian) extends DiffFunction[DenseVector[Double]] {

    /**
     * Return value and gradient of the quadratic model at the current iterate:
     *  q_k(p)        = f_k + (p-x_k)^T g_k + 1/2 (p-x_k)^T B_k(p-x_k)
     *  \nabla q_k(p) = g_k + B_k(p-x_k)
     */
    override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
      val d = x - xk
      val Bd = B.times(d)
      val f = fk + d.dot(gk) + (0.5 * d.dot(Bd))
      val g = gk + Bd
      (f, g)
    }
  }
}

