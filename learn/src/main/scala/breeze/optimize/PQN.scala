package breeze.optimize

import scala.actors._
import breeze.linalg._
import com.typesafe.scalalogging.log4j.Logging

// Compact representation of an n x n Hessian, maintained via L-BFGS updates
class CompactHessian(m: Int) {
  var sigma = 1.0
  var M = null.asInstanceOf[DenseMatrix[Double]]
  var Y = new RingBuffer[DenseVector[Double]](m)
  var S = new RingBuffer[DenseVector[Double]](m)
  def update(y: DenseVector[Double],
    s: DenseVector[Double]): Unit = {
    // Compute scaling factor for initial Hessian, which we choose as
    val yTs = y.dot(s)

    if (yTs < 1e-10) // ensure B remains strictly positive definite
      return

    S += s
    Y += y
    sigma = 1.0 / (yTs / y.dot(y))

    val k = Y.size

    // D_k is the k x k diagonal matrix D_k = diag [s_0^Ty_0, ...,s_{k-1}^Ty_{k-1}].
    val fD = Futures.future {
      diag(DenseVector(S.zip(Y).map {case (s,y) => s dot y}:_*))
    }

    // L_k is the k x k matrix with (L_k)_{i,j} = if( i > j ) s_i^T y_j else 0   
    // (this is a lower triangular matrix with the diagonal set to all zeroes)
    val fL = Futures.future {
      val ret = DenseMatrix.zeros[Double](k, k)
      for (j <- 0 until k)
        for (i <- (j + 1) until k)
          ret.update(i, j, S(i) dot Y(j))
      ret
    }

    // S_k^T S_k is the symmetric k x k matrix with element (i,j) given by <s_i, s_j>
    val fSTS = Futures.future {
      val ret = DenseMatrix.zeros[Double](k, k)
      for (j <- 0 until k) {
        for (i <- j until k) {
          val sTs = sigma * (S(i) dot S(j))
          ret.update(i, j, sTs)
          ret.update(j, i, sTs)
        }
      }
      ret
    }

    val L = fL()
    val D = fD()
    val STS = fSTS()

    // M is the 2k x 2k matrix given by: M = [ \sigma * S_k^T S_k    L_k ]
    //                                       [         L_k^T        -D_k ]
    M = DenseMatrix.vertcat(DenseMatrix.horzcat(STS, L), DenseMatrix.horzcat(L.t, -D))


  }

  def times(v: DenseVector[Double], Bd: DenseVector[Double]): DenseVector[Double] = {
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

    val f1 = Futures.future {
      var i = 0
      for (s <- S) {
        ntv.update(i, 0, s.dot(v) * sigma)
        i += 1
      }
    }
    val f2 = Futures.future {
      var i = S.size
      for (y <- Y) {
        ntv.update(i, 0, y.dot(v))
        i += 1
      }
    }
    f1(); f2();
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

// Forms a quadratic model around fun, the argmin of which is then a feasible
// quasi-Newton descent direction
class PQNSubproblem(fun: ProjectableProblem,
  fk: Double,
  xk: DenseVector[Double],
  gk: DenseVector[Double],
  B: CompactHessian) extends ProjectableProblem {
  var Bd = xk.copy
  var time = 0L

  def project(p: DenseVector[Double]): DenseVector[Double] = fun.project(p)
  /**
   * Return value and gradient of the quadratic model at the current iterate:
   *  q_k(p)        = f_k + (p-x_k)^T g_k + 1/2 (p-x_k)^T B_k(p-x_k)
   *  \nabla q_k(p) = g_k + B_k(p-x_k)
   */
  override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
    val d = x - xk
    Bd = B.times(d, Bd)
    val f = fk + d.dot(gk) + (0.5 * d.dot(Bd))
    val g = gk + Bd
    (f, g)
  }
}

class PQN(
  val optTol: Double = 1e-3,
  val m: Int = 10,
  val initFeas: Boolean = false,
  val sleep: Int = 0,
  val testOpt: Boolean = true,
  val maxNumIt: Int = 2000,
  val maxSrchIt: Int = 30,
  val gamma: Double = 1e-10) extends Minimizer[DenseVector[Double], ProjectableProblem] with Logging {
  // strictness of linesearch
  def minimize(prob: ProjectableProblem, guess: DenseVector[Double]): DenseVector[Double] = {

    def computeGradientNorm(x: DenseVector[Double], g: DenseVector[Double]): Double = computeGradient(x, g).norm(Double.PositiveInfinity)
    def computeGradient(x: DenseVector[Double], g: DenseVector[Double]): DenseVector[Double] = prob.project(x - g) - x
    val x = if (initFeas) guess.copy else prob.project(guess.copy)
    val g = prob.gradientAt(x)
    var f = prob.valueAt(x)
    var d = DenseVector.zeros[Double](x.size)
    var gnorm = computeGradientNorm(x, g)
    val B = new CompactHessian(m)
    var y = null.asInstanceOf[DenseVector[Double]]
    var s = null.asInstanceOf[DenseVector[Double]]
    var t = 1
    var fevals = 1
    var time = 0L
    var uptime = 0L
    var subtime = 0L

    logger.debug(f"PQN: Initially  : f $f%-10.4f ||g|| $gnorm%-10.4f")

    while (((testOpt == false) || (gnorm > optTol))
      && (t < maxNumIt) //&& (!prob.hasConverged)
      ) {

      // Find descent direction
      if (t == 1) {
        d = computeGradient(x, g * (0.1 / gnorm))
      } else {
        // Update the limited-memory BFGS approximation to the Hessian
        B.update(y, s)
        // Solve subproblem; we use the current iterate x as a guess
        val subprob = new PQNSubproblem(prob, f, x, g, B)
        val p = new SPG(
          testOpt = false,
          optTol = 1e-3,
          maxNumIt = 30,
          initFeas = true,
          M = 5
        ).minimize(subprob, x)
        d = p - x
        //	time += subprob.time
      }

      // Backtracking line-search
      var accepted = false
      var lambda = 1.0
      val gTd = g.dot(d)
      var srchit = 0

      do {
        val candx = x + d * lambda
        val candg = prob.gradientAt(candx)
        val candf = prob.valueAt(candx)
        val suffdec = gamma * lambda * gTd

        if (testOpt && srchit > 0) {
          logger.debug(f"PQN:    SrchIt $srchit%4d: f $candf%-10.4f t $lambda%-10.4f\n")
        }

        if (candf < f + suffdec) {
          s = candx - x
          y = candg - g
          x := candx
          g := candg
          f = candf
          accepted = true
        } else if (srchit >= maxSrchIt) {
          accepted = true
        } else {
          lambda *= 0.5
          srchit = srchit + 1
        }
        fevals = fevals + 1
      } while (!accepted)

      if (srchit >= maxSrchIt) {
        logger.info("PQN: Line search cannot make further progress")
        return x
      }

      if (testOpt) {
        gnorm = computeGradientNorm(x, g)
        logger.debug(f"PQN: MainIt $t%4d: f $f%-10.4f ||g|| $gnorm%-10.4f")
      }

      t = t + 1
    }

    logger.info(f"PQN: FinIt  $t%4d: f $f%-10.4f ||g|| $gnorm%-10.4f fevals: $fevals%d")

    return x
  }

}
