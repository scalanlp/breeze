// WIP file holder for a find nearest positive definite matrix function
<<<<<<< HEAD

package breeze.linalg.functions

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.eigSym
import breeze.linalg.fliplr
import breeze.linalg.reverse
import breeze.linalg.diag
import breeze.linalg.*
import breeze.linalg.Axis
import util.control.Breaks.break

/** Generate the nearest (in Frobenius norm) positive definite matrix
  * using the algorithm decribed in (Higham 2002)
  * Inspired by the NearPD function from the Matrix package
  * from Martin Maechler
  * @param matA DenseMatrix of doubles n * n approximately positive definite matrix,
  * typically an approximation to a correlation or covariance matrix.
  * If x is not symmetric (and ensureSymmetry is not false), symmpart(x) is used.
  * @param corrBool Boolean indicating if the matrix should be a correlation matrix.
  * @param keepDiag Boolean generalizing corr: if true, the resulting matrix
  * should have the same diagonal (diag(x)) as the input matrix.
  * @param do2eigen Boolean indicating if a posdefify() eigen step should be
  * applied to the result of the Higham algorithm.
  * @param doSym Boolean indicating if X <- (X + t(X))/2 should be done,
  * after X <- tcrossprod(Qd, Q); some doubt if this is necessary.
  * @param doDykstra Boolean indicating if Dykstra's correction should be used;
  * true by default. If false, the algorithm is basically the direct fixpoint
  * iteration Y(k) = P_U(P_S(Y(k-1))).
  * @param onlyVals Boolean if TRUE, the result is just the vector of
  * eigenvalues of the approximating matrix.
  * the target vector to mean 0 and unit variance, default is false
  * @param eigTol defines relative positiveness of eigenvalues compared to
  * largest one, λ_1. Eigenvalues λ_k are treated as if zero when λ_k / λ_1 ≤ eig.tol.
  * @param convTol convergence tolerance for Higham algorithm.
  * @param posdTol tolerance for enforcing positive definiteness
  * (in the final posdefify step when do2eigen is TRUE).
  * @param maxit maximum number of iterations allowed.
  * @param convNormType convergence norm type (norm(*, type)) used for Higham
  * algorithm. The default is "I" (infinity), for reasons of speed (and back compatibility);
  * using "F" is more in line with Higham's proposal.
  * @param trace Boolean specifying if convergence monitoring should be traced.
  */
/**
 * TODO
 * migrate to breeze-y style, integrate with library, add tests
 * @author ewalsh
 **/

class NearPD(matA: DenseMatrix[Double], corrBool: Boolean = false, keepDiag: Boolean = false,
  do2eigen: Boolean = true, doSym: Boolean = false, doDykstra: Boolean = true,
  onlyVals: Boolean = false, eigTol: Double = 1e-6, convTol: Double = 1e-7, posdTol: Double = 1e-8,
  maxit: Int = 100, convNormType: String = "I", trace: Boolean = false) {
  // ensure symmetry
  val symA = (matA + matA.t) / 2.0
  val diagX0 = diag(symA)
  //--- start algorithm ---
  def generate: DenseMatrix[Double] = {
    // choose Dykstra or direct
    val matX = chooseAlgo(symA, doDykstra)
    // run posdefify ?
    if(do2eigen || onlyVals){
      posDefify(matX)
    } else {
      matX
    }
  }

  // posdefify eigen step
  def posDefify(mat: DenseMatrix[Double]): DenseMatrix[Double] = {
    var matX = mat
    val eigVecVals = eigSym(matX)
    var vecd = reverse(eigVecVals.eigenvalues)
    val n = mat.cols - 1
    val eps = posdTol * breeze.numerics.abs(vecd(0))
    if(vecd(n) < eps){
      vecd = vecd.map(x => {
        if(x < eps){
          eps
        } else {
          x
        }
      })
      if(!onlyVals){
        val matQ = fliplr(eigVecVals.eigenvectors)
        val matDiag = diag(matX)
        matX = matQ * matQ.t(::, *).map(_ * vecd)
        val dVec = breeze.numerics.sqrt(pmax(matDiag, DenseVector(eps))/diag(matX))
        matX = (dVec * dVec.t) *:* matX
      }
    }
    if(onlyVals){
      matX = DenseMatrix(vecd)
    }
    if(corrBool){
      matX = corrDiagFunc(matX, corrBool)
    }
    if(keepDiag){
      matX = keepDiagFunc(matX, keepDiag)
    }
    matX
  }
  // parallel max
  def pmax(vec0: DenseVector[Double], vec1: DenseVector[Double]): DenseVector[Double] = {
    val max0 = breeze.linalg.max(vec0)
    val max1 = breeze.linalg.max(vec1)
    if(max0 > max1){
      vec0
    } else {
      vec1
    }
  }
  // choose algo
  def chooseAlgo(mat: DenseMatrix[Double], bool: Boolean): DenseMatrix[Double] = {
    bool match {
      case true => doDykstraFunc(mat)
      case false => doDirectFixedPt(mat)
    }
  }

  // bool to double
  def bool2Dbl(bool: Boolean): Double = {
    if(bool){
      1.0
    } else {
      0.0
    }
  }
  // keepDiag function
  def keepDiagFunc(mat: DenseMatrix[Double], bool: Boolean): DenseMatrix[Double] = {
    bool match {
      case true => {
        DenseMatrix.tabulate(mat.rows, mat.cols)({
          case(i, j) => {
            if(i == j){
              diagX0(i)
            } else {
              mat(i, j)
            }
          }
        })
      }
     case false => mat
    }
  }
  // doSym function
  def doSymFunc(mat: DenseMatrix[Double], bool: Boolean): DenseMatrix[Double] = {
    bool match {
      case true => (mat * mat.t) / 2.0
      case false => mat
    }
  }
  // corr diag function
  def corrDiagFunc(mat: DenseMatrix[Double], bool: Boolean): DenseMatrix[Double] = {
    bool match {
      case true => {
        DenseMatrix.tabulate(mat.rows, mat.cols)({
          case(i, j) => {
            if(i == j){
              1.0
            } else {
              mat(i, j)
            }
          }
        })
      }
      case false => mat
    }
  }
  // norm type function
  def normTypeFunc(matX: DenseMatrix[Double], matY: DenseMatrix[Double], normType: String): Double = {
    normType match {
      case "F" => breeze.linalg.norm((matY - matX).toDenseVector) / breeze.linalg.norm((matY).toDenseVector)
      case "1" => breeze.linalg.max(breeze.linalg.sum(breeze.numerics.abs(matY - matX), Axis._0)) / breeze.linalg.max(breeze.linalg.sum(breeze.numerics.abs(matY), Axis._0))
      case "2" => breeze.linalg.max(breeze.linalg.svd(matY - matX).singularValues) / breeze.linalg.max(breeze.linalg.svd(matY).singularValues)
      case _ =>  breeze.linalg.max(breeze.linalg.sum(breeze.numerics.abs(matY - matX), Axis._1))  / breeze.linalg.max(breeze.linalg.sum(breeze.numerics.abs(matY), Axis._1)) // Infinity Norm
    }
  }
  // doDykstra function
  def doDykstraFunc(mat: DenseMatrix[Double]): DenseMatrix[Double] = {
    var iter: Int = 0
    var converged: Boolean = false
    var conv = Double.PositiveInfinity
    val n: Int = mat.cols
    var dSubS = DenseMatrix.zeros[Double](mat.rows, n)
    var matX = mat
    var matY = matX
    var matR = matY
    while(iter < maxit && !converged){
      matY = matX
      matR = matY - dSubS
      val eigVecVals = eigSym(matR)
      val matQ = fliplr(eigVecVals.eigenvectors)
      val vecd = reverse(eigVecVals.eigenvalues)
      val p = vecd.map(x => x > eigTol)
      val pcheck = breeze.linalg.sum(p.map(x => bool2Dbl(x)))
      if(pcheck < 1.0){
        println("Matrix seems negative semi-definite")
        break
      }
      val matQtrim = matQ(::, p).toDenseMatrix
      val vecdTrim = vecd(p).toDenseVector
      val cpPt1 =  DenseMatrix.tabulate(matQtrim.rows, matQtrim.cols)({
        case(i, j) => matQtrim(i, j) * vecdTrim(j)
      })
      matX = cpPt1 * matQtrim.t
      dSubS = matX - matR
      matX = doSymFunc(matX, doSym)
      matX = corrDiagFunc(matX, corrBool)
      matX = keepDiagFunc(matX, keepDiag)
      conv = normTypeFunc(matX, matY, convNormType)
      iter += 1
      if(trace){
        println(s"iter ${iter} : p = ${breeze.linalg.sum(vecd)} ||Y-X|| / ||Y|| = ${conv}")
      }
      converged = (conv < convTol)
    }
    if(!converged){
      println(s"Did not converge in ${iter} iterations")
    }
    matX
  }

  def doDirectFixedPt(mat: DenseMatrix[Double]): DenseMatrix[Double] = {
    var iter = 0
    var converged = false
    var conv = Double.PositiveInfinity
    var matX = mat
    var matY = matX
    while(iter < maxit && !converged){
      matY = matX
      val eigVecVals = eigSym(matY)
      val matQ = fliplr(eigVecVals.eigenvectors)
      val vecd = reverse(eigVecVals.eigenvalues)
      val p = vecd.map(x => x > eigTol).toDenseVector
      val pcheck = breeze.linalg.sum(p.map(x => bool2Dbl(x)))
      if(pcheck < 1.0){
        println("Matrix seems negative semi-definite")
        break
      }
      val matQtrim = matQ(::, p).toDenseMatrix
      val vecdTrim = vecd(p).toDenseVector
      val cpPt1 =  DenseMatrix.tabulate(matQtrim.rows, matQtrim.cols)({
        case(i, j) => matQtrim(i, j) * vecdTrim(j)
      })
      matX = cpPt1 * matQtrim.t
      matX = doSymFunc(matX, doSym)
      matX = corrDiagFunc(matX, corrBool)
      matX = keepDiagFunc(matX, keepDiag)
      conv = normTypeFunc(matX, matY, convNormType)
      iter += 1
      if(trace){
        println(s"iter ${iter} : p = ${breeze.linalg.sum(vecd)} ||Y-X|| / ||Y|| = ${conv}")
      }
      converged = (conv < convTol)
    }
    if(!converged){
      println(s"Did not converge in ${iter} iterations")
    }
    matX
  }

}

// ## A longer example, extended from Jens' original,
// ## showing the effects of some of the options:

// val matA = DenseMatrix(Array(Array(1,     0.477, 0.644, 0.478, 0.651, 0.826),
//                Array(0.477, 1,     0.516, 0.233, 0.682, 0.75),
//                Array(0.644, 0.516, 1,     0.599, 0.581, 0.742),
//                Array(0.478, 0.233, 0.599, 1,     0.741, 0.8),
//                Array(0.651, 0.682, 0.581, 0.741, 1,     0.798),
//                Array(0.826, 0.75,  0.742, 0.8,   0.798, 1)):_*
// )
=======
>>>>>>> 694c019cb53517fbaa1b06197ad2ca476772a26f
