package breeze.optimize

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.linalg._
import breeze.util.SerializableLogging
import breeze.util.Implicits._


/**
 * This algorithm is refered the paper
 * "A LIMITED MEMOR Y ALGORITHM F OR BOUND CONSTRAINED OPTIMIZA TION" written by
 * Richard H.Byrd   Peihuang Lu   Jorge Nocedal  and Ciyou Zhu
 * Created by fanming.chen on 2015/3/7 0007.
 * If StrongWolfeLineSearch(maxZoomIter,maxLineSearchIter) is small, the wolfeRuleSearch.minimize may throw FirstOrderException,
 * it should increase the two varialbes to appropriate value
 */
class LBFGSB(maxIter:Int = 100, m:Int = 5, tolerance:Double = 1E-8,
              maxZoomIter:Int = 64, maxLineSearchIter:Int = 64)
    extends FirstOrderMinimizer[DenseVector[Double], DiffFunction[DenseVector[Double]]](maxIter, tolerance) with SerializableLogging {
  protected val PROJ_GRADIENT_EPS = 1E-5;
  protected val EPS = 2.2E-16

  var upperBounds = DenseVector.zeros[Double](0)
  var lowerBounds = DenseVector.zeros[Double](0)

  protected var theta = 1.0
  protected var W = DenseMatrix.zeros[Double](0,0)// [Yk theta*Sk]
  protected var M = DenseMatrix.zeros[Double](0,0)

  protected var yHistoryMat = DenseMatrix.zeros[Double](0, 0)
  protected var sHistoryMat = DenseMatrix.zeros[Double](0, 0)

  override type History = Unit
  //initialize only is called once, so it can be used to init some arguments
  override protected def initialHistory(f: DiffFunction[DenseVector[Double]], init: DenseVector[Double]):History = {
    initialize(f, init)
  }

  override protected def updateHistory(newX: DenseVector[Double], newGrad: DenseVector[Double], newVal: Double, f: DiffFunction[DenseVector[Double]], oldState: State): History =  {
    updateSkYkHessianApproxMat(newX - oldState.x, newGrad :- oldState.grad)
  }

  override protected def chooseDescentDirection(state: State, f: DiffFunction[DenseVector[Double]]): DenseVector[Double] = {
    val x = state.x
    val g = state.grad

    //step2:compute the cauchy point by algorithm CP
    val (cauchyPoint, c) = getGeneralizedCauchyPoint(x, g)

    val dirk = if(0 == state.iter) cauchyPoint - x else  {
      //step3:compute a search direction d_k by the primal method
      val subspaceMin = subspaceMinimization(cauchyPoint, x, c, g)
      subspaceMin - x
    };

    dirk
  }


  override protected def determineStepSize(state: State, f: DiffFunction[DenseVector[Double]], direction: DenseVector[Double]): Double =  {
    val x = state.x
    val ff = LineSearch.functionFromSearchDirection(f, x, direction)
    val wolfeRuleSearch = new StrongWolfeLineSearch(maxZoomIter, maxLineSearchIter) // TODO: Need good default values here.
    wolfeRuleSearch.minimize(ff, 1.0)
  }

  override protected def takeStep(state: State, dir: DenseVector[Double], stepSize: Double) = {
    state.x + (dir :* stepSize)
  }

  //return a iterate which will next until convergeTest return true.
  override def iterations(f: DiffFunction[DenseVector[Double]], init: DenseVector[Double]): Iterator[State] = {
    infiniteIterations(f, init).takeUpToWhere(convergeTest)
  }

  private def initialize(f: DiffFunction[DenseVector[Double]], x0: DenseVector[Double]) = {
    val DIM = x0.length
    if (lowerBounds.length == 0) {
      lowerBounds = DenseVector.ones[Double](DIM) :* Double.MinValue
    }
    if (upperBounds.length != DIM) {
      upperBounds = DenseVector.ones[Double](DIM) :* Double.MaxValue
    }
    require(x0.forall( (i, v) => (lowerBounds(i) <= v && v <= upperBounds(i))),
          "seed is not feasible (violates lower bound or upperBounds)")

    theta = 1.0
    W = DenseMatrix.zeros[Double](DIM, 2*m) // [Yk theta*Sk]
    M = DenseMatrix.zeros[Double](2*m, 2*m)
    yHistoryMat = DenseMatrix.zeros[Double](0, 0)
    sHistoryMat = DenseMatrix.zeros[Double](0, 0)
  }

  private def convergeTest(state:State): Boolean = {
    val fVals = state.fVals;
    if (isConvergence(state.x, state.grad)) {
      state.lastConvergenceReason = Some(FirstOrderMinimizer.GradientConverged)
      true
    } else if (state.iter >= maxIter && maxIter >= 0) {
      true //FirstOrderMinimizer.MaxIterations
    } else if (2 <= fVals.length //FirstOrderMinimizer.FunctionValuesConverged
              && norm(fVals(fVals.length - 2) - fVals(fVals.length - 1)) < tolerance) {
      state.lastConvergenceReason = Some(FirstOrderMinimizer.FunctionValuesConverged)
      true
    } else if (state.searchFailed) {
      true //FirstOrderMinimizer.SearchFailed
    } else false
  }

  protected def getGeneralizedCauchyPoint(x:DenseVector[Double], g: DenseVector[Double]) = {
    //Algorithm CP:Computation of generalized Cauchy point
    val n = x.length
    val d = DenseVector.zeros[Double](n)
    val t = g.mapPairs{ (i, gi) =>
      if (0 == gi) {
        (i, Double.MaxValue)
      } else {
        val ti = if (gi < 0) {
          (x(i) - upperBounds(i)) / gi
        } else {
          (x(i) - lowerBounds(i)) / gi
        }
        d(i) = if(0 == ti) 0 else -gi
        (i, ti)
      }
    }

    //Initialize
    val xCauchy = x.copy;
    var p = W.t*d
    var c = DenseVector.zeros[Double](M.rows)
    var fDerivative:Double = g.dot(d)
    var fSecondDerivative = (-1.0*theta) * fDerivative - p.dot(M*p)
    var dtMin = - (fDerivative/fSecondDerivative)
    var oldT = 0.0
    val sortedIndeces = t.map(x => x._1).toArray.
      sortWith((ia, ib) => t(ia)._2 < t(ib)._2)

    var i = sortedIndeces.indexWhere(idx => 0 != t(idx)._2)
    var b = sortedIndeces(i)
    var minT = t(b)._2
    var deltaT = minT - oldT


    //xamination of subsequen t segments
    while(deltaT <= dtMin && (i < n)) {
      xCauchy(b) = if (0 < d(b)) upperBounds(b) else lowerBounds(b)

      val zb = xCauchy(b) - x(b)
      c = c + p :* deltaT

      val bRowOfW:DenseVector[Double] = W(b, ::).t
      fDerivative += deltaT*fSecondDerivative + g(b)*g(b) + theta*g(b)*zb - (bRowOfW.t :* g(b))*(M*c)
      fSecondDerivative += -1.0*theta*g(b)*g(b) - 2.0*(g(b) * (bRowOfW.dot(M*p))) - g(b)*g(b) * (bRowOfW.t*(M*bRowOfW))
      p +=  (bRowOfW :* g(b));
      d(b) = 0.0
      dtMin = -fDerivative/fSecondDerivative
      oldT = minT
      i += 1
      if (i < n) {
        b = sortedIndeces(i)
        minT = t(b)._2
        deltaT = minT - oldT
      }
    }

    dtMin = math.max(dtMin , 0)
    oldT += dtMin

    for(sortIdx <- i until n ) {
      xCauchy(sortedIndeces(sortIdx)) = x(sortedIndeces(sortIdx)) + oldT * d(sortedIndeces(sortIdx))
    }

    c += p :* dtMin

    (xCauchy, c)
  }

  /**
   *
   * @param xCauchy generalize cauchy point
   * @param du gradient directiong
   * @param freeVarIndex
   * @return starAlpha = max{a : a <= 1 and  l_i-xc_i <= a*d_i <= u_i-xc_i}
   */
  protected def findAlpha(xCauchy:DenseVector[Double], du:Vector[Double], freeVarIndex:Array[Int]) = {
    var starAlpha = 1.0
    for((vIdx, i) <- freeVarIndex.zipWithIndex) {
      starAlpha = if (0 < du(i)) {
        math.max(starAlpha, math.min(upperBounds(vIdx) - xCauchy(vIdx) / du(i), 1.0))
      } else {
        math.max(starAlpha, math.min(lowerBounds(vIdx) - xCauchy(vIdx) / du(i), 1.0))
      }
    }

    starAlpha
  }

  //use Direct Primal Method
  protected def subspaceMinimization(xCauchy:DenseVector[Double], x:DenseVector[Double], c:DenseVector[Double], g:DenseVector[Double]) =  {
    val invTheta = 1.0/theta

    var freeVariableIndexes = collection.mutable.ArrayBuffer[Int]()
    xCauchy.mapPairs { (i, v) =>
      if (v != upperBounds(i) && v != lowerBounds(i)) {
        freeVariableIndexes += i
      }
    }
    val freeVarCount = freeVariableIndexes.length
    //WZ = W^T*Z
    val WZ = DenseMatrix.zeros[Double](W.cols, freeVarCount)
    for (i <- 0 until freeVarCount) {
      WZ(::, i) := W(freeVariableIndexes(i), ::).t
    }

    // r=(g+theta*(x_cauchy-x)-W*(M*c));
    val dirTheta:DenseVector[Double] = (xCauchy - x) :* theta;
    val fullR = g + dirTheta -  W*(M*c)
    val rc = fullR(freeVariableIndexes)

    //step2 && step3 v=M*W^T*Z*rc
    var v:DenseVector[Double] = M*(WZ * rc)
    //step4 N = 1/theta * W^T*Z * (W^T*Z)^T
    var N:DenseMatrix[Double] = WZ*WZ.t;
    N = N:*invTheta
    N = DenseMatrix.eye[Double](N.rows) - M*N;
    //step5:v = N^(-1) * v
    val invN = inv(N)
    val invNv = invN*v
    v = N \ v
    //step6
    val wzv:DenseVector[Double] = WZ.t*v :* (invTheta * invTheta)
    val thetaRC = rc:*invTheta
    val du = (thetaRC + wzv) :* (-1.0)
    //step7 find star alpha
    val starAlpha = findAlpha(xCauchy, du, freeVariableIndexes.toArray)

    val dStar = du :* starAlpha

    val subspaceMinX = xCauchy.copy
    for((freeVarIdx, i) <- freeVariableIndexes.zipWithIndex) {
      subspaceMinX(freeVarIdx) = subspaceMinX(freeVarIdx) + dStar(i)
    }

    subspaceMinX
  }

  protected def updateSkYkHessianApproxMat(newS:DenseVector[Double], newY:DenseVector[Double]) = {
    /*step6:test If yk satisfies curvature condition sk'*yk > eps||y||^2  with eps = 2.2*10^(-16),add sk and yk into Sk and Yk.
      *If more than m updates are stored, delete the old columns from Sk and Yk
      */
    val curvatureTest = math.abs(newS.dot(newY))
    if (EPS*norm(newY, 2) < curvatureTest) {
      if (0 == yHistoryMat.cols) {//yHistoryMat.cols means update times
        yHistoryMat = newY.toDenseMatrix.t
        sHistoryMat = newS.toDenseMatrix.t
      } else if(yHistoryMat.cols < m) {
        yHistoryMat = DenseMatrix.horzcat(yHistoryMat, newY.toDenseMatrix.t)
        sHistoryMat = DenseMatrix.horzcat(sHistoryMat, newS.toDenseMatrix.t)
      } else {//m <= k discard the oldest yk and sk
        yHistoryMat(::, 0 until (m - 1)) := yHistoryMat(::, 1 until m)
        sHistoryMat(::, 0 until (m - 1)) := sHistoryMat(::, 1 until m)
        yHistoryMat(::, m - 1) := newY
        sHistoryMat(::, m - 1) := newS
      }

      //step7:update Sk'Sk, Yk'Yk, Lk and Rk, and set theta = yk'*yk/yk'*sk
      theta = newY.t*newY / (newY.t*newS)
      W = DenseMatrix.horzcat(yHistoryMat, sHistoryMat :* theta)
      val A:DenseMatrix[Double] = sHistoryMat.t * yHistoryMat
      val L = strictlyLowerTriangular(A)
      val D:DenseMatrix[Double] = diag(diag(A)) :* (-1.0)

      val STS:DenseMatrix[Double] = sHistoryMat.t*sHistoryMat
      val MM = DenseMatrix.vertcat(DenseMatrix.horzcat(D, L.t) ,
        DenseMatrix.horzcat(L, STS :* theta))
      M = inv(MM)//MM-1 M is defined at formula 3.4
    }
  }

  //norm(P(xk-gk, l, u) - xk, INF) < 1E-5
  protected def isConvergence(x:DenseVector[Double], g:DenseVector[Double]): Boolean = {
    val pMinusX = (x - g).mapPairs { (i, v) =>
      if (v <= lowerBounds(i)) {
        lowerBounds(i) - x(i)
      } else if (upperBounds(i) <= v) {
        upperBounds(i) - x(i)
      } else {
        v - x(i)
      }
    }
    val normPMinusX = norm(pMinusX, Double.PositiveInfinity)

    math.abs(normPMinusX) < PROJ_GRADIENT_EPS
  }
}