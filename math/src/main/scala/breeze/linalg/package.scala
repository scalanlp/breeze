package breeze
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import generic.{CanCollapseAxis, CanMapValues, UReduceable, URFunc}
import linalg.operators._
import linalg.support.{CanNorm, CanCopy}
import math.Semiring
import org.netlib.lapack.LAPACK
import org.netlib.util.intW
import storage.DefaultArrayValue
import org.jblas.NativeBlas

/**
 * This package contains everything relating to Vectors, Matrices, Tensors, etc.
 *
 * If you're doing basic work, you probably want [[breeze.linalg.DenseVector]] and [[breeze.linalg.DenseMatrix]],
 * which support most operations. We also have [[breeze.linalg.SparseVector]]s and (basic!) support
 * for a sparse matrix ([[breeze.linalg.CSCMatrix]]).
 *
 * This package object contains Matlab-esque functions for interacting with tensors and matrices.
 *
 * @author dlwh
 */
package object linalg extends LinearAlgebra {

  /**
   * Returns true if we can use native libraries. You can disable it by writing
   * breeze.linalg.useNativeLibraries to false
   */
  def useNativeLibraries = _useNativeLibraries

  private var _useNativeLibraries = canLoadNativeBlas


  /**
   * Attempts to load the NativeBlas libraries. Returns false if we can't.
   * @return
   */
  def canLoadNativeBlas: Boolean = {
    try {
      // he left this constructor public, which is useful for us.
      // this attempts to load the library, we'll get an exception if false
      new NativeBlas
      true
    } catch {
      case x: UnsatisfiedLinkError => false
    }
  }

  /**
   * Disables or attempts to enable native libraries. Will throw a RuntimeException if you
   * try to set the value to true and we can't load the libraries.
   * @param v
   */
  def useNativeLibraries_=(v: Boolean) = {
    if (v == true) {
      if (!canLoadNativeBlas) throw new RuntimeException("Can't load NativeBlasLibraries")
    }
    _useNativeLibraries = v
  }


  /**
   * returns a vector along the diagonal of v.
   * Requires a square matrix?
   * @param m the matrix
   * @tparam V
   */
  def diag[@specialized(Double) V](m: DenseMatrix[V]): DenseVector[V] = {
    require(m.rows == m.cols, "m must be square")
    new DenseVector(m.data, m.offset, m.majorStride + 1, m.rows)
  }

  // there's a weird compile error I don't understand if I try to use diag in DenseMatrix.scala directly.
  // this is a crappy little function to deal with it.
  private[linalg] def diagM[@specialized(Double) V](m: DenseMatrix[V]): DenseVector[V] = {
    diag(m)
  }


  /**
   * Creates a Diagonal dense matrix from this vector.
   *
   * TODO make a real diagonal matrix class
   * @param t
   * @tparam V
   * @return
   */
  // TODO: make a real diagonal matrix class
  def diag[V:ClassManifest:DefaultArrayValue](t: DenseVector[V]): DenseMatrix[V] = {
    val r = DenseMatrix.zeros[V](t.length, t.length)
    diag(r) := t
    r
  }

  /**
   * Generates a vector of linearly spaced values between a and b (inclusive).
   * The returned vector will have length elements, defaulting to 100.
   */
  def linspace(a : Double, b : Double, length : Int = 100) : DenseVector[Double] = {
    val increment = (b - a) / (length - 1)
    DenseVector.tabulate(length)(i => a + increment * i)
  }

  /**
   * Copy a T. Most tensor objects have a CanCopy implicit, which is what this farms out to.
   */
  def copy[T](t: T)(implicit canCopy: CanCopy[T]): T = canCopy(t)

  /**
   * Computes the norm of an object. Many tensor objects have a CanNorm implicit, which is what this calls.
   */
  def norm[T](t: T, v: Double = 2)(implicit canNorm: CanNorm[T]) = canNorm(t, v)


  /**
   * Normalizes the argument such that its norm is 1.0 (with respect to the argument n).
   * Returns value if value's norm is 0.
   */
  def normalize[T, U>:T](t: T, n: Double = 2)(implicit div: BinaryOp[T, Double, OpDiv, U], canNorm: CanNorm[T]): U = {
    val norm = canNorm(t, n)
    if(norm == 0) t
    else div(t,norm)
  }

  /**
   * Normalizes the argument along each axis such that each row along the axis has norm 1.0 (with respect to the argument n).
   * Each column is unchanged if it's norm is 0
   */
  def normalize[T, Axis, V, Result](value: T, axis: Axis, n: Double)(implicit  collapse: CanCollapseAxis[T, Axis, V, V, Result],
                                                                     div: BinaryOp[V, Double, OpDiv, V], canNorm: CanNorm[V]):Result = {
    collapse(value, axis)(v => normalize[V, V](v, n))
  }

  /**
   * logNormalizes the argument such that the softmax is 0.0.
   * Returns value if value's softmax is -infinity
   */
  def logNormalize[V,K](value: V)(implicit view: V => NumericOps[V],
                                  red: UReduceable[V, Double],
                                  op : BinaryOp[V,Double,OpSub,V]): V = {
    val max = softmax(value)
    if(max == Double.NegativeInfinity) value
    else value - max
  }

  /**
   * logs and then logNormalizes the argument such that the softmax is 0.0.
   * Returns value if value's softmax is -infinity
   */
  def logAndNormalize[V](value: V)(implicit view: V => NumericOps[V],
                                   red: UReduceable[V, Double],
                                   map: CanMapValues[V, Double, Double, V],
                                   op : BinaryOp[V,Double,OpSub,V]):V = {
    logNormalize(numerics.log(value))
  }

  /**
   * logs and then logNormalizes the argument along axis such that each softmax is 0.0.
   * Returns value if value's softmax is -infinity
   */
  def logAndNormalize[T, Axis, V, Result](value: T, axis: Axis)(implicit  collapse: CanCollapseAxis[T, Axis, V, V, Result],
                                  view: V => NumericOps[V],
                                  red: UReduceable[V, Double],
                                  map: CanMapValues[V, Double, Double, V],
                                  op : BinaryOp[V,Double,OpSub,V]):Result = {
    collapse(value, axis)(v => logAndNormalize(v))
  }

  /**
   * A [[breeze.generic.URFunc]] for computing the mean of objects
   */
  val mean:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      val (sum,n) = accumulateAndCount(cc)
      sum / n
    }

    def accumulateAndCount(it : TraversableOnce[Double]):(Double, Int) = it.foldLeft( (0.0,0) ) { (tup,d) =>
      (tup._1 + d, tup._2 + 1)
    }

    override def apply(arr: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      var i = 0
      var used = 0
      var sum = 0.0
      var off = offset
      while(i < length) {
        if(isUsed(i)) {
          sum += arr(off)
          used += 1
        }
        i += 1
        off += stride
      }
      sum / used
    }
  }


  val sum:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      cc.sum
    }

    override def apply(arr: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      var i = 0
      var sum = 0.0
      var off = offset
      while(i < length) {
        if(isUsed(i)) {
          sum += arr(off)
        }
        i += 1
        off += stride
      }
      sum
    }
  }


  /**
   * A [[breeze.generic.URFunc]] for computing the mean and variance of objects.
   * This uses an efficient, numerically stable, one pass algorithm for computing both
   * the mean and the variance.
   */
  val meanAndVariance:URFunc[Double, (Double,Double)] = new URFunc[Double, (Double,Double)] {
    def apply(it: TraversableOnce[Double]) = {
      val (mu,s,n) = it.foldLeft( (0.0,0.0,0)) { (acc,y) =>
        val (oldMu,oldVar,n) = acc
        val i = n+1
        val d = y - oldMu
        val mu = oldMu + 1.0/i * d
        val s = oldVar + (i-1) * d / i * d
        (mu,s,i)
      }
      (mu,s/(n-1))
    }

    override def apply(arr: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      var mu = 0.0
      var s = 0.0
      var n = 0
      var i = 0
      var off = offset
      while(i < length) {
        if(isUsed(i)) {
          val y = arr(off)
          n += 1
          val d = y - mu
          mu = mu + 1.0/n * d
          s = s + (n-1) * d / n * d
        }
        off += stride
        i += 1
      }
      (mu, s/(n-1))

    }
  }

  /**
   * A [[breeze.generic.URFunc]] for computing the variance of objects.
   * The method just calls meanAndVariance and returns the second result.
   */
  val variance:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      meanAndVariance(cc)._2
    }


    override def apply(arr: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      meanAndVariance(arr, offset, stride, length, isUsed)._2
    }
  }

  /**
   * Cmoputes the standard deviation by calling variance and then sqrt'ing
   */
  val stddev:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      scala.math.sqrt(variance(cc))
    }

    override def apply(arr: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      scala.math.sqrt(variance(arr, offset, stride, length, isUsed))
    }
  }

  /**
   * Computes the max, aka the infinity norm.
   */
  val max:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      cc.max
    }

    override def apply(arr: Array[Double], offset: Int, stride: Int,length: Int, isUsed: (Int) => Boolean) = {
      var max = Double.NegativeInfinity
      var i = 0
      var off = offset
      while(i < length) {
        if(isUsed(i)) {
          val m = arr(off)
          if(max < m) max = m
        }
        off += stride
        i += 1
      }
      max
    }
  }


  /**
   * Computes the minimum.
   */
  val min:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      cc.min
    }

    override def apply(arr: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      var min = Double.NegativeInfinity
      var i = 0
      var off = offset
      while(i < length) {
        if(isUsed(i)) {
          val m = arr(off)
          if(min > m) min = m
        }
        off += stride
        i += 1
      }
      min
    }
  }

  /**
   * Computes the softmax (a.k.a. logSum) of an object. Softmax is defined as \log \sum_i \exp(x(i)), but
   * implemented in a more numerically stable way. Softmax is so-called because it is
   * a differentiable function that tends to look quite a lot like max. Consider
   * log(exp(30) + exp(10)). That's basically 30. We use softmax a lot in machine learning.
   */
  val softmax:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      val a = cc.toArray[Double]
      breeze.numerics.logSum(a, a.length)
      // apply(cc.toArray) breaks the compiler...
    }


    override def apply(a: Array[Double], length: Int) = {
      numerics.logSum(a, length)
    }

    override def apply(a: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      length match {
        case 0 => Double.NegativeInfinity
        case 1 => if(isUsed(0)) a(0) else Double.NegativeInfinity
        case 2 =>
          if (isUsed(0))
            if (isUsed(1))
              numerics.logSum(a(0),a(1))
            else a(0)
          else if(isUsed(1)) a(1)
          else Double.NegativeInfinity
        case _ =>
          val m = max(a, offset, stride, length, isUsed)
          if (m.isInfinite) m
          else {
            var i = 0
            var off = offset
            var accum = 0.0
            while(i < length) {
              if(isUsed(i))
                accum += scala.math.exp(a(off) - m)
              i += 1
              off += stride
            }
            if (i > 0)
              m + scala.math.log(accum)
            else Double.NegativeInfinity
          }
      }
    }
  }

}

package linalg {

import math.Ring

/**
 * Basic linear algebraic operations.
 *
 * @author dlwh,dramage,retronym,afwlehmann,lancelet
 */
trait LinearAlgebra {
  //  import breeze.linalg._

  @inline private def requireNonEmptyMatrix[V](mat: Matrix[V]) =
    if (mat.cols == 0 || mat.rows == 0)
      throw new MatrixEmptyException

  @inline private def requireSquareMatrix[V](mat: Matrix[V]) =
    if (mat.rows != mat.cols)
      throw new MatrixNotSquareException

  @inline private def requireSymmetricMatrix[V](mat: Matrix[V]) = {
    requireSquareMatrix(mat)

    for (i <- 0 until mat.rows; j <- 0 until i)
      if (mat(i,j) != mat(j,i))
        throw new MatrixNotSymmetricException
  }

  /**
   * Eigenvalue decomposition (right eigenvectors)
   *
   * This function returns the real and imaginary parts of the eigenvalues,
   * and the corresponding eigenvectors.  For most (?) interesting matrices,
   * the imaginary part of all eigenvalues will be zero (and the corresponding
   * eigenvectors will be real).  Any complex eigenvalues will appear in
   * complex-conjugate pairs, and the real and imaginary components of the
   * eigenvector for each pair will be in the corresponding columns of the
   * eigenvector matrix.  Take the complex conjugate to find the second
   * eigenvector.
   *
   * Based on EVD.java from MTJ 0.9.12
   */
  def eig(m : Matrix[Double]): (DenseVector[Double], DenseVector[Double], DenseMatrix[Double]) = {
    requireNonEmptyMatrix(m)
    requireSquareMatrix(m)

    val n = m.rows

    // Allocate space for the decomposition
    val Wr = DenseVector.zeros[Double](n)
    val Wi = DenseVector.zeros[Double](n)

    val Vr = DenseMatrix.zeros[Double](n,n)

    // Find the needed workspace
    val worksize = Array.ofDim[Double](1)
    val info = new intW(0)
    LAPACK.getInstance.dgeev(
      "N", "V", n,
      Array.empty[Double], scala.math.max(1,n),
      Array.empty[Double], Array.empty[Double],
      Array.empty[Double], scala.math.max(1,n),
      Array.empty[Double], scala.math.max(1,n),
      worksize, -1, info)

    // Allocate the workspace
    val lwork: Int = if (info.`val` != 0)
      scala.math.max(1,4*n)
    else
      scala.math.max(1,worksize(0).toInt)

    val work = Array.ofDim[Double](lwork)

    // Factor it!

    val A = DenseMatrix.zeros[Double](n, n)
    A := m
    if (useNativeLibraries) {
      val i = NativeBlas.dgeev(
        'N', 'V', n,
        A.data, 0, scala.math.max(1,n),
        Wr.data, 0, Wi.data, 0,
        Array.empty[Double], 0, scala.math.max(1,n),
        Vr.data, 0, scala.math.max(1,n))
        info.`val` = i
    } else {
      LAPACK.getInstance.dgeev(
        "N", "V", n,
        A.data, scala.math.max(1,n),
        Wr.data, Wi.data,
        Array.empty[Double], scala.math.max(1,n),
        Vr.data, scala.math.max(1,n),
        work, work.length, info)
    }

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    (Wr, Wi, Vr)
  }

  /**
   * Computes the SVD of a m by n matrix
   * Returns an m*m matrix U, a vector of singular values, and a n*n matrix V'
   */
  def svd(mat: DenseMatrix[Double]):(DenseMatrix[Double],DenseVector[Double],DenseMatrix[Double]) = {
    requireNonEmptyMatrix(mat)

    val m = mat.rows
    val n = mat.cols
    val S = DenseVector.zeros[Double](m min n)
    val U = DenseMatrix.zeros[Double](m,m)
    val Vt = DenseMatrix.zeros[Double](n,n)
    val iwork = new Array[Int](8 * (m min n) )
    val workSize = ( 3
      * scala.math.min(m, n)
      * scala.math.min(m, n)
      + scala.math.max(scala.math.max(m, n), 4 * scala.math.min(m, n)
      * scala.math.min(m, n) + 4 * scala.math.min(m, n))
      )
    val work = new Array[Double](workSize)
    val info = new intW(0)
    val cm = copy(mat)

    if (useNativeLibraries) {
      val i = NativeBlas.dgesvd(
        'A', 'A', m, n,
        cm.data, 0, scala.math.max(1,m),
        S.data, 0, U.data, 0, scala.math.max(1, m),
        Vt.data, 0, scala.math.max(1,n))
        info.`val` = i
    } else {
      LAPACK.getInstance.dgesdd(
        "A", m, n,
        cm.data, scala.math.max(1,m),
        S.data, U.data, scala.math.max(1,m),
        Vt.data, scala.math.max(1,n),
        work,work.length,iwork, info)
    }

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    (U,S,Vt)
  }

  /**
   * Returns the Kronecker product of the two matrices a and b,
   * usually denoted a ⊗ b.
   */
  def kron[V1,V2, M,RV](a : DenseMatrix[V1], b : M)(implicit mul : BinaryOp[V1, M, OpMulScalar,DenseMatrix[RV]],
                                                    asMat: M<:<Matrix[V2],
                                                    man: ClassManifest[RV],
                                                    dfv: DefaultArrayValue[RV]) : DenseMatrix[RV] = {
    val result: DenseMatrix[RV] = DenseMatrix.zeros[RV](a.rows * b.rows, a.cols * b.cols)
    for( ((r,c),av) <- a.activeIterator) {
      result((r*b.rows) until ((r+1)*b.rows), (c * b.cols) until ((c+1) * b.cols)) := mul(av, b)
    }
    result
  }

  /**
   * Vector cross product of 3D vectors a and b.
   */
  def cross[V1](a: DenseVector[V1], b: DenseVector[V1])(implicit ring: Ring[V1], man: ClassManifest[V1]): DenseVector[V1] = {
    require(a.length == 3)
    require(b.length == 3)
    import ring._
    DenseVector(
      ring.-(*(a(1), b(2)), *(a(2), b(1))),
      ring.-(*(a(2), b(0)), *(a(0), b(2))),
      ring.-(*(a(0), b(1)), *(a(1), b(0)))
    )
  }

  /**
   * Returns the rank of each element in the given vector, adjusting for
   * ties.
   */
  def ranks[V:Ordering](x : Vector[V]): Array[Double] = {
    val a = x
    val as = a.argsort
    val rv = new Array[Double](as.length)
    var i = 0
    while (i < as.length) {
      // count number of tied values at rank i
      var numTiedValuesAtI = 1
      while (i + numTiedValuesAtI < as.length && a(as(i + numTiedValuesAtI)) == a(as(i))) {
        numTiedValuesAtI += 1
      }

      // set return value for next numTiedValuesAtI indexes in as
      val rank = 1 + i + (numTiedValuesAtI - 1) / 2.0
      var j = 0
      while (j < numTiedValuesAtI) {
        rv(as(i + j)) = rank
        j += 1
      }

      i += numTiedValuesAtI
    }

    rv
  }

  /**
   * The lower triangular portion of the given real quadratic matrix X. Note
   * that no check will be performed regarding the symmetry of X.
   */
  def lowerTriangular[T: Semiring: ClassManifest:DefaultArrayValue](X: Matrix[T]): DenseMatrix[T] = {
    val N = X.rows
    DenseMatrix.tabulate(N, N)( (i, j) =>
      if(j <= i) X(i,j)
      else implicitly[Semiring[T]].zero
    )
  }

  /**
   * The upper triangular portion of the given real quadratic matrix X. Note
   * that no check will be performed regarding the symmetry of X.
   */
  def upperTriangular[T: Semiring: ClassManifest: DefaultArrayValue](X: Matrix[T]): DenseMatrix[T] = {
    val N = X.rows
    DenseMatrix.tabulate(N, N)( (i, j) =>
      if(j >= i) X(i,j)
      else implicitly[Semiring[T]].zero
    )
  }

  /**
   * Computes the cholesky decomposition A of the given real symmetric
   * positive definite matrix X such that X = A A^T.
   *
   * XXX: For higher dimensionalities, the return value really should be a
   *      sparse matrix due to its inherent lower triangular nature.
   */
  def cholesky(X: Matrix[Double]): DenseMatrix[Double] = {
    requireNonEmptyMatrix(X)

    // As LAPACK doesn't check if the given matrix is in fact symmetric,
    // we have to do it here (or get rid of this time-waster as long as
    // the caller of this function is clearly aware that only the lower
    // triangular portion of the given matrix is used and there is no
    // check for symmetry).
    requireSymmetricMatrix(X)

    // Copy the lower triangular part of X. LAPACK will store the result in A
    val A: DenseMatrix[Double] = lowerTriangular(X)

    val N = X.rows
    val info = new intW(0)
    if (useNativeLibraries) {
      val i = NativeBlas.dpotrf(
        'L', N, A.data, 0, scala.math.max(1,N))
        info.`val` = i
    } else {
      LAPACK.getInstance.dpotrf(
        "L" /* lower triangular */,
        N /* number of rows */, A.data, scala.math.max(1, N) /* LDA */,
        info
      )
    }
    // A value of info.`val` < 0 would tell us that the i-th argument
    // of the call to dpotrf was erroneous (where i == |info.`val`|).
    assert(info.`val` >= 0)

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)

    A
  }

  /**
   * QR Factorization with pivoting
   *
   * input: A m x n matrix
   * output: (Q,R,P,pvt) where AP = QR
   *   Q: m x m
   *   R: m x n
   *   P: n x n : permutation matrix (P(pvt(i),i) = 1)
   *   pvt : pivot indices
   */
  def qrp(A: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Int], Array[Int]) = {
    val m = A.rows
    val n = A.cols
    val lapack = LAPACK.getInstance()

    //Get optimal workspace size
    // we do this by sending -1 as lwork to the lapack function
    val work = new Array[Double](1)
    var info = new intW(0)
    lapack.dgeqrf(m, n, null, m, null, work, -1, info)
    val lwork1 = if(info.`val` != 0) n else work(0).toInt
    lapack.dorgqr(m, m, scala.math.min(m,n), null, m, null, work, -1, info)
    val lwork2 = if(info.`val` != 0) n else work(0).toInt
    //allocate workspace mem. as max of lwork1 and lwork3
    val workspace = new Array[Double](scala.math.max(lwork1, lwork2))

    //Perform the QR factorization with dgep3
    val maxd = scala.math.max(m,n)
    val AFact = DenseMatrix.zeros[Double](m,maxd)
    val pvt = new Array[Int](n)
    val tau = new Array[Double](scala.math.min(m,n))
    for(r <- 0 until m; c <- 0 until n) AFact(r,c) = A(r,c)
    lapack.dgeqp3(m, n, AFact.data, m, pvt, tau, workspace, workspace.length, info)

    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    //Get R
    val R = DenseMatrix.zeros[Double](m,n)

    for(c <- 0 until maxd if(c < n); r <- 0 until m if(r <= c))
      R(r,c) = AFact(r,c)

    //Get Q from the matrix returned by dgep3
    val Q = DenseMatrix.zeros[Double](m,m)
    lapack.dorgqr(m, m, scala.math.min(m,n), AFact.data, m, tau, workspace, workspace.length, info)
    for(r <- 0 until m; c <- 0 until maxd if(c < m))
      Q(r,c) = AFact(r,c)

    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    //Get P
    import NumericOps.Arrays._
    pvt -= 1
    val P = DenseMatrix.zeros[Int](n,n)
    for(i <- 0 until n)
      P(pvt(i), i) = 1

    (Q,R,P,pvt)
  }

  /**
   * QR Factorization
   *
   * @param A m x n matrix
   * @param skipQ (optional) if true, don't reconstruct orthogonal matrix Q (instead returns (null,R))
   * @return: (Q,R) Q: m x m R: m x n
   */
  // TODO: I don't like returning null sometimes here...
  def qr(A: DenseMatrix[Double], skipQ : Boolean = false): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val m = A.rows
    val n = A.cols
    val lapack = LAPACK.getInstance()

    //Get optimal workspace size
    // we do this by sending -1 as lwork to the lapack function
    val work = new Array[Double](1)
    val info = new intW(0)
    lapack.dgeqrf(m, n, null, m, null, work, -1, info)
    val lwork1 = if(info.`val` != 0) n else work(0).toInt
    lapack.dorgqr(m, m, scala.math.min(m,n), null, m, null, work, -1, info)
    val lwork2 = if(info.`val` != 0) n else work(0).toInt
    //allocate workspace mem. as max of lwork1 and lwork3
    val workspace = new Array[Double](scala.math.max(lwork1, lwork2))

    //Perform the QR factorization with dgeqrf
    val maxd = scala.math.max(m,n)
    val mind = scala.math.min(m,n)
    val tau = new Array[Double](mind)
    val outputMat = DenseMatrix.zeros[Double](m,maxd)
    for(r <- 0 until m; c <- 0 until n)
      outputMat(r,c) = A(r,c)
    lapack.dgeqrf(m, n, outputMat.data, m, tau, workspace, workspace.length, info)

    //Error check
    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)
    else if (info.`val` < 0)
      throw new IllegalArgumentException()

    //Get R
    val R = DenseMatrix.zeros[Double](m,n)
    for(c <- 0 until maxd if(c < n); r <- 0 until m if(r <= c))
      R(r,c) = outputMat(r,c)

    //unless the skipq flag is set
    if(!skipQ){
      //Get Q from the matrix returned by dgep3
      val Q = DenseMatrix.zeros[Double](m,m)
      lapack.dorgqr(m, m, scala.math.min(m,n), outputMat.data, m, tau, workspace, workspace.length, info)
      for(r <- 0 until m; c <- 0 until maxd if(c < m))
        Q(r,c) = outputMat(r,c)

      //Error check
      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()
      (Q,R)
    }
    //skip Q and just return R
    else (null,R)
  }


  /**
   * Computes all eigenvalues (and optionally right eigenvectors) of the given
   * real symmetric matrix X.
   */
  def eigSym(X: Matrix[Double], rightEigenvectors: Boolean):
  (DenseVector[Double], Option[DenseMatrix[Double]]) =
  {
    requireNonEmptyMatrix(X)

    // As LAPACK doesn't check if the given matrix is in fact symmetric,
    // we have to do it here (or get rid of this time-waster as long as
    // the caller of this function is clearly aware that only the lower
    // triangular portion of the given matrix is used and there is no
    // check for symmetry).
    requireSymmetricMatrix(X)

    // Copy the lower triangular part of X. LAPACK will store the result in A.
    val A     = lowerTriangular(X)

    val N     = X.rows
    val evs   = DenseVector.zeros[Double](N)
    val lwork = scala.math.max(1, 3*N-1)
    val work  = Array.ofDim[Double](lwork)
    val info  = new intW(0)
    if(useNativeLibraries) {
      val i = NativeBlas.dsyev(if(rightEigenvectors) 'V' else 'N', 'L',
        N, A.data, 0, scala.math.max(1, N),
        evs.data, 0)

      info.`val` = i
    } else {
      LAPACK.getInstance.dsyev(
        if (rightEigenvectors) "V" else "N" /* eigenvalues N, eigenvalues & eigenvectors "V" */,
        "L" /* lower triangular */,
        N /* number of rows */, A.data, scala.math.max(1, N) /* LDA */,
        evs.data,
        work /* workspace */, lwork /* workspace size */,
        info
      )
    }
    // A value of info.`val` < 0 would tell us that the i-th argument
    // of the call to dsyev was erroneous (where i == |info.`val`|).
    assert(info.`val` >= 0)

    if (info.`val` > 0)
      throw new NotConvergedException(NotConvergedException.Iterations)

    (evs, if (rightEigenvectors) Some(A) else None)
  }

  /**
   * Computes the LU factorization of the given real M-by-N matrix X such that
   * X = P * L * U where P is a permutation matrix (row exchanges).
   *
   * Upon completion, a tuple consisting of a matrix A and an integer array P.
   *
   * The upper triangular portion of A resembles U whereas the lower triangular portion of
   * A resembles L up to but not including the diagonal elements of L which are
   * all equal to 1.
   *
   * For 0 <= i < M, each element P(i) denotes whether row i of the matrix X
   * was exchanged with row P(i-1) during computation (the offset is caused by
   * the internal call to LAPACK).
   */
  def LU[T](X: Matrix[T])(implicit td: T => Double): (DenseMatrix[Double], Array[Int]) = {
    requireNonEmptyMatrix(X)

    val M    = X.rows
    val N    = X.cols
    // TODO: use := when that's available
    val Y    = DenseMatrix.tabulate[Double](M,N)(X(_,_))
    val ipiv = Array.ofDim[Int](scala.math.min(M,N))
    val info = new intW(0)
    if(useNativeLibraries) {
      val i = NativeBlas.dgetrf(M, N, Y.data, 0, scala.math.max(1, M), ipiv, 0)
      info.`val` = i
    } else {
      LAPACK.getInstance.dgetrf(
        M /* rows */, N /* cols */,
        Y.data, scala.math.max(1,M) /* LDA */,
        ipiv /* pivot indices */,
        info
      )
    }
    // A value of info.`val` < 0 would tell us that the i-th argument
    // of the call to dsyev was erroneous (where i == |info.`val`|).
    assert(info.`val` >= 0)

    (Y, ipiv)
  }

  /**
   * Computes the determinant of the given real matrix.
   */
  def det[T](X: Matrix[T])(implicit td: T => Double): Double = {
    requireSquareMatrix(X)

    // For triangular N-by-N matrices X, the determinant of X equals the product
    // of the diagonal elements X(i,i) where 0 <= i < N.
    // Since det(AB) = det(A) * det(B), the LU factorization is well-suited for
    // the computation of the determinant of general N-by-N matrices.
    val (m, ipiv) = LU(X)

    // Count the number of exchanged rows.  ipiv contains an array of swapped indices,
    //  but it also contains indices that weren't swapped.  To count the swapped
    //  indices, we have to compare them against their position within the array.  A
    //  final complication is that the array indices are 1-based, due to the LU call
    //  into LAPACK.
    val numExchangedRows = ipiv.map(_ - 1).zipWithIndex.count { piv => piv._1 != piv._2 }

    var acc = if (numExchangedRows % 2 == 1) -1.0 else 1.0
    for (i <- 0 until m.rows)
      acc *= m(i,i)

    acc
  }

  /**
   * Computes the inverse of a given real matrix.
   */
  def inv[T](X: Matrix[T])(implicit td: T => Double): DenseMatrix[Double] = {
    requireSquareMatrix(X)

    val (m, ipiv) = LU(X)
    val N         = m.rows
    val lwork     = scala.math.max(1, N)
    val work      = Array.ofDim[Double](lwork)
    val info      = new intW(0)
    LAPACK.getInstance.dgetri(
      N, m.data, scala.math.max(1, N) /* LDA */,
      ipiv,
      work /* workspace */, lwork /* workspace size */,
      info
    )
    assert(info.`val` >= 0, "Malformed argument %d (LAPACK)".format(-info.`val`))

    if (info.`val` > 0)
      throw new MatrixSingularException

    m
  }

  /**
   * Computes the Moore-Penrose pseudo inverse of the given real matrix X.
   */
  def pinv(X: DenseMatrix[Double]) : DenseMatrix[Double] = {
    requireNonEmptyMatrix(X)

    // The pseudo inverse is nothing but the least-squares solution to AX=B,
    // hence:
    //       d/dX 1/2 (AX-B)^2 = A^T (AX-B)
    // Solving A^T (AX-B) = 0 for X yields
    //       A^T AX = A^T B
    //    =>      X = (A^T A)^(-1) A^T B

    inv(X.t * X) * X.t
  }

  /**
   * Computes the Moore-Penrose pseudo inverse of the given real matrix X.
   */
  def pinv[V](X: DenseMatrix[V])(implicit cast : V=>Double) : DenseMatrix[Double] = {
    requireNonEmptyMatrix(X)

    // The pseudo inverse is nothing but the least-squares solution to AX=B,
    // hence:
    //       d/dX 1/2 (AX-B)^2 = A^T (AX-B)
    // Solving A^T (AX-B) = 0 for X yields
    //       A^T AX = A^T B
    //    =>      X = (A^T A)^(-1) A^T B

    pinv(X.mapValues(cast))
  }

  /**
   * Computes the rank of a DenseMatrix[Double].
   *
   * The rank of the matrix is computed using the SVD method.  The singular values of the SVD
   * which are greater than a specified tolerance are counted.
   *
   * @param m matrix for which to compute the rank
   * @param tol optional tolerance for singular values.  If not supplied, the default
   *   tolerance is: max(m.cols, m.rows) * eps * sigma_max, where
   *   eps is the machine epsilon and sigma_max is the largest singular value of m.
   * @return the rank of the matrix (number of singular values)
   */
  def rank(m: DenseMatrix[Double], tol: Option[Double] = None): Int = {
    val (u, s, vt) = svd(m)
    val useTol = tol.getOrElse {
      // we called LAPACK for the SVD method, so this is the LAPACK definition of eps.
      val eps: Double = 2.0 * LAPACK.getInstance.dlamch("e")
      scala.math.max(m.cols, m.rows) * eps * s.max
    }
    s.data.count(_ > useTol)  // TODO: Use DenseVector[_].count() if/when that is implemented
  }

}

object LinearAlgebra extends LinearAlgebra


/**
 * Exception thrown if a routine has not converged.
 */
class NotConvergedException(val reason: NotConvergedException.Reason, msg: String = "")
  extends RuntimeException(msg)

object NotConvergedException {
  trait Reason
  object Iterations extends Reason
  object Divergence extends Reason
  object Breakdown extends Reason
}

class MatrixNotSymmetricException extends IllegalArgumentException("Matrix is not symmetric")

class MatrixNotSquareException extends IllegalArgumentException("Matrix is not square")

class MatrixEmptyException extends IllegalArgumentException("Matrix is empty")
}

