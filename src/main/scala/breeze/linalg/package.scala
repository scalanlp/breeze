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
import breeze.generic._
import io.{CSVWriter, CSVReader}
import linalg.operators._
import breeze.linalg.support.{RangeSuffix, CanNorm, CanCopy}
import math.Semiring
import org.netlib.util.intW
import storage.DefaultArrayValue
import java.io.{File, FileReader}
import scala.reflect.ClassTag
import breeze.generic.CanTraverseValues.ValuesVisitor


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
package object linalg {

  /**
   * Computes y += x * a, possibly doing less work than actually doing that operation
   */
  def axpy[A, X, Y](a: A, x: X, y: Y)(implicit axpy: CanAxpy[A, X, Y]) { axpy(a,x,y) }

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
  def norm[T](t: T)(implicit canNorm: CanNorm[T, Unit]) = canNorm(t, ())

  /**
   * Computes the norm of an object. Many tensor objects have a CanNorm implicit, which is what this calls.
   */
  def norm[T, NormType](t: T, v: NormType)(implicit canNorm: CanNorm[T, NormType]) = canNorm(t, v)



  /**
   * Normalizes the argument such that its norm is 1.0 (with respect to the argument n).
   * Returns value if value's norm is 0.
   */
  object normalize extends UFunc {
    implicit def normalizeDoubleImpl[T, U>:T](implicit div: BinaryOp[T, Double, OpDiv, U], canNorm: CanNorm[T, Double]):Impl2[T, Double, U] = {
      new Impl2[T, Double, U] {
        def apply(t: T, n: Double): U = {
          val norm = canNorm(t, n)
          if(norm == 0) t
          else div(t,norm)
        }
      }
    }


    implicit def normalizeImpl[T, U>:T](implicit impl: Impl2[T, Double, U]):Impl[T, U] = {
      new Impl[T, U] {
        def apply(v: T): U = impl(v, 2.0)
      }

    }

    implicit def normalizeIntImpl[T, U>:T](implicit impl: Impl2[T, Double, U]):Impl2[T, Int, U] = {
      new Impl2[T, Int, U] {
        def apply(v: T, n: Int): U = impl(v, n)
      }

    }
  }

  object logNormalize extends UFunc {
    implicit def logNormalizeImpl[V](implicit softmaxImpl: softmax.Impl[V, Double],
                                     op : BinaryOp[V,Double,OpSub,V]):Impl[V, V] = new Impl[V, V] {

      def apply(value: V): V = {
        val max = softmax(value)
        if(max == Double.NegativeInfinity) value
        else op(value, max)
      }
    }

  }

  object logAndNormalize extends UFunc {
    implicit def logNormalizeImpl[V](implicit logImpl: breeze.numerics.log.Impl[V, V],
                                     logNormalizeImpl: logNormalize.Impl[V, V]):Impl[V, V] = new Impl[V, V] {

      def apply(value: V): V = {
        logNormalize(numerics.log(value))
      }
    }

  }


  /**
   * A [[breeze.generic.UFunc]] for computing the mean of objects
   */
  object mean extends UFunc {
    implicit def reduceDouble[T](implicit iter: CanTraverseValues[T, Double]): Impl[T, Double] = new Impl[T, Double] {
      def apply(v: T): Double = {
        val visit = new ValuesVisitor[Double] {
          var sum = 0.0
          var n = 0
          def visit(a: Double): Unit = {
            sum += a
            n += 1
          }

          def zeros(numZero: Int, zeroValue: Double): Unit = {
            sum += numZero * zeroValue
            n += numZero
          }
        }

        iter.traverse(v, visit)

        visit.sum / visit.n
      }
    }

  }


  object sum extends UFunc {
    implicit def reduceDouble[T](implicit iter: CanTraverseValues[T, Double]): Impl[T, Double] = new Impl[T, Double] {
      def apply(v: T): Double = {
        val visit = new ValuesVisitor[Double] {
          var sum = 0.0
          def visit(a: Double): Unit = {
            sum += a
          }

          def zeros(numZero: Int, zeroValue: Double): Unit = {
            sum += numZero * zeroValue
          }
        }

        iter.traverse(v, visit)

        visit.sum
      }

    }
  }


  /**
   * A [[breeze.generic.UFunc]] for computing the mean and variance of objects.
   * This uses an efficient, numerically stable, one pass algorithm for computing both
   * the mean and the variance.
   */
  object meanAndVariance extends UFunc {
    implicit def reduceDouble[T](implicit iter: CanTraverseValues[T, Double]): Impl[T, (Double, Double)] = new Impl[T, (Double, Double)] {
      def apply(v: T): (Double, Double) = {
        val visit = new ValuesVisitor[Double] {
          var mu = 0.0
          var s = 0.0
          var n = 0
          def visit(y: Double): Unit = {
            n += 1
            val d = y - mu
            mu = mu + 1.0/n * d
            s = s + (n-1) * d / n * d
          }

          def zeros(numZero: Int, zeroValue: Double): Unit = {
            for(i <- 0 until numZero) visit(zeroValue)
          }
        }
        iter.traverse(v, visit)
        import visit._
        (mu, s/(n-1))
      }
    }
  }


  /**
   * A [[breeze.generic.UFunc]] for computing the variance of objects.
   * The method just calls meanAndVariance and returns the second result.
   */
  object variance extends UFunc {
    implicit def reduceDouble[T](mv: meanAndVariance.Impl[T, (Double, Double)]): Impl[T, Double] = new Impl[T, Double] {
      def apply(v: T): Double = mv(v)._2
    }
  }

  /**
   * Computes the standard deviation by calling variance and then sqrt'ing
   */
  object stddev extends UFunc {
    implicit def reduceDouble[T](mv: variance.Impl[T, Double]): Impl[T, Double] = new Impl[T, Double] {
      def apply(v: T): Double = scala.math.sqrt(mv(v))
    }
  }

  object max extends UFunc {
    implicit def reduceDouble[T](implicit iter: CanTraverseValues[T, Double]): Impl[T, Double] = new Impl[T, Double] {
      def apply(v: T): Double = {
        val visit = new ValuesVisitor[Double] {
          var max = Double.NegativeInfinity
          var visitedOne = false
          def visit(a: Double): Unit = {
            visitedOne = true
            max = scala.math.max(max, a)
          }

          def zeros(numZero: Int, zeroValue: Double): Unit = {
            if(numZero != 0) {
              visitedOne = true
              max = scala.math.max(zeroValue, max)
            }
          }
        }

        iter.traverse(v, visit)
        if(!visit.visitedOne) throw new IllegalArgumentException(s"No values in $v!")

        visit.max
      }

    }
  }




  /**
   * Computes the minimum.
   */
  object min extends UFunc {
    implicit def reduceDouble[T](implicit iter: CanTraverseValues[T, Double]): Impl[T, Double] = new Impl[T, Double] {
      def apply(v: T): Double = {
        val visit = new ValuesVisitor[Double] {
          var min = Double.PositiveInfinity
          var visitedOne = false
          def visit(a: Double): Unit = {
            visitedOne = true
            min = scala.math.min(min, a)
          }

          def zeros(numZero: Int, zeroValue: Double): Unit = {
            if(numZero != 0) {
              visitedOne = true
              min = scala.math.min(zeroValue, min)
            }
          }
        }

        iter.traverse(v, visit)
        if(!visit.visitedOne) throw new IllegalArgumentException(s"No values in $v!")

        visit.min
      }

    }
  }


  /**
   * Computes the softmax (a.k.a. logSum) of an object. Softmax is defined as \log \sum_i \exp(x(i)), but
   * implemented in a more numerically stable way. Softmax is so-called because it is
   * a differentiable function that tends to look quite a lot like max. Consider
   * log(exp(30) + exp(10)). That's basically 30. We use softmax a lot in machine learning.
   */
  object softmax extends UFunc {

    implicit object implDoubleDouble extends Impl2[Double, Double, Double] {
      def apply(a: Double, b: Double): Double = {
        if (a.isNegInfinity) b
        else if (b.isNegInfinity) a
        else if (a < b) b + scala.math.log1p(scala.math.exp(a - b))
        else a + scala.math.log1p(scala.math.exp(b - a))
      }
    }

    implicit def reduceDouble[T](implicit iter: CanTraverseValues[T, Double], maxImpl: max.Impl[T, Double]): Impl[T, Double] = new Impl[T, Double] {
      def apply(v: T): Double = {

        val max = if(iter.onePass(v)) 0.0 else maxImpl(v)

        val visit = new ValuesVisitor[Double] {
          var accum = 0.0
          def visit(a: Double): Unit = {
            accum += scala.math.exp(a - max)
          }

          def zeros(numZero: Int, zeroValue: Double): Unit = {
            if(numZero != 0) {
              accum += (numZero * scala.math.exp(zeroValue - max))
            }
          }

          override def visitArray(arr: Array[Double], offset: Int, length: Int, stride: Int): Unit = {
            var i = 0
            var off = offset
            while(i < length) {
                accum += scala.math.exp(arr(off) - max)
              i += 1
              off += stride
            }

          }
        }

        iter.traverse(v, visit)

        max + scala.math.log(visit.accum)
      }

    }
  }

  object logDiff extends UFunc {
    implicit object implDoubleDouble extends Impl2[Double, Double, Double] {
      def apply(a: Double, b: Double): Double = {
        require(a >= b, s"a should be greater than b, but got $a and $b")
        if (a > b) a + scala.math.log1p(- scala.math.exp(b-a))
        else Double.NegativeInfinity
      }
    }
  }

  // io stuff
  /**
   * Reads in a DenseMatrix from a CSV File
   */
  def csvread(file: File,
             separator: Char=',',
             quote: Char='"',
             escape: Char='\\',
             skipLines: Int = 0): DenseMatrix[Double] = {
    val input = new FileReader(file)
    var mat = CSVReader.read(input, separator, quote, escape, skipLines)
    mat = mat.takeWhile(line => line.length != 0 && line.head.nonEmpty) // empty lines at the end
    input.close()
    if(mat.length == 0) {
      DenseMatrix.zeros[Double](0,0)
    } else {
      DenseMatrix.tabulate(mat.length,mat.head.length)((i,j)=>mat(i)(j).toDouble)
    }
  }

  def csvwrite(file: File, mat: Matrix[Double],
               separator: Char=',',
               quote: Char='\0',
               escape: Char='\\',
               skipLines: Int = 0) {
    CSVWriter.writeFile(file, IndexedSeq.tabulate(mat.rows,mat.cols)(mat(_,_).toString), separator, quote, escape)
  }

  /** for adding slicing */
  implicit class RichIntMethods(val x: Int) extends AnyVal {
    def until(z: ::.type) = new RangeSuffix(x)
  }



  import math.Ring
  import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}

/**
 * Basic linear algebraic operations.
 *
 * @author dlwh,dramage,retronym,afwlehmann,lancelet
 */
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

    lapack.dgeev(
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
      lapack.dgeev(
        "N", "V", n,
        A.data, scala.math.max(1,n),
        Wr.data, Wi.data,
        Array.empty[Double], scala.math.max(1,n),
        Vr.data, scala.math.max(1,n),
        work, work.length, info)

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

    lapack.dgesdd(
      "A", m, n,
      cm.data, scala.math.max(1,m),
      S.data, U.data, scala.math.max(1,m),
      Vt.data, scala.math.max(1,n),
      work,work.length,iwork, info)

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
                                                    man: ClassTag[RV],
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
  def cross[V1](a: DenseVector[V1], b: DenseVector[V1])(implicit ring: Ring[V1], man: ClassTag[V1]): DenseVector[V1] = {
    require(a.length == 3)
    require(b.length == 3)
    DenseVector(
      ring.-(ring.*(a(1), b(2)), ring.*(a(2), b(1))),
      ring.-(ring.*(a(2), b(0)), ring.*(a(0), b(2))),
      ring.-(ring.*(a(0), b(1)), ring.*(a(1), b(0)))
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
  def lowerTriangular[T: Semiring: ClassTag:DefaultArrayValue](X: Matrix[T]): DenseMatrix[T] = {
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
  def upperTriangular[T: Semiring: ClassTag: DefaultArrayValue](X: Matrix[T]): DenseMatrix[T] = {
    val N = X.rows
    DenseMatrix.tabulate(N, N)( (i, j) =>
      if(j >= i) X(i,j)
      else implicitly[Semiring[T]].zero
    )
  }

  /**
   * Computes the cholesky decomposition A of the given real symmetric
   * positive definite matrix X such that X = A A.t.
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
      lapack.dpotrf(
        "L" /* lower triangular */,
        N /* number of rows */, A.data, scala.math.max(1, N) /* LDA */,
        info
      )
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

    //Get optimal workspace size
    // we do this by sending -1 as lwork to the lapack function
    val scratch, work = new Array[Double](1)
    var info = new intW(0)
    lapack.dgeqrf(m, n, scratch, m, scratch, work, -1, info)
    val lwork1 = if(info.`val` != 0) n else work(0).toInt
    lapack.dorgqr(m, m, scala.math.min(m,n), scratch, m, scratch, work, -1, info)
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
   * @return (Q,R) Q: m x m R: m x n
   */
  // TODO: I don't like returning null sometimes here...
  def qr(A: DenseMatrix[Double], skipQ : Boolean = false): (DenseMatrix[Double], DenseMatrix[Double]) = {
    val m = A.rows
    val n = A.cols

    //Get optimal workspace size
    // we do this by sending -1 as lwork to the lapack function
    // can't pass in null arrays on linux to new lapack, so passing in a length 1 called scratch
    val scratch, work = new Array[Double](1)
    val info = new intW(0)
    lapack.dgeqrf(m, n, scratch, m, scratch, work, -1, info)
    val lwork1 = if(info.`val` != 0) n else work(0).toInt
    lapack.dorgqr(m, m, scala.math.min(m,n), scratch, m, scratch, work, -1, info)
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
    lapack.dsyev(
      if (rightEigenvectors) "V" else "N" /* eigenvalues N, eigenvalues & eigenvectors "V" */,
      "L" /* lower triangular */,
      N /* number of rows */, A.data, scala.math.max(1, N) /* LDA */,
      evs.data,
      work /* workspace */, lwork /* workspace size */,
      info
    )
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
    lapack.dgetrf(
      M /* rows */, N /* cols */,
      Y.data, scala.math.max(1,M) /* LDA */,
      ipiv /* pivot indices */,
      info
    )
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
    val (m:DenseMatrix[Double], ipiv:Array[Int]) = LU(X)

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
   * In general, you should avoid using this metho in combination with *.
   * Instead, wherever you might want to write inv(A) * B, you should write
   * A \ B.
   */
  def inv[T](X: Matrix[T])(implicit td: T => Double): DenseMatrix[Double] = {
    requireSquareMatrix(X)

    // Should these type hints be necessary?
    val (m:DenseMatrix[Double], ipiv:Array[Int]) = LU(X)
    val N         = m.rows
    val lwork     = scala.math.max(1, N)
    val work      = Array.ofDim[Double](lwork)
    val info      = new intW(0)
    lapack.dgetri(
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
   *
   *     The pseudo inverse is nothing but the least-squares solution to AX=B,
   * hence:
   *        d/dX 1/2 (AX-B)^2 = A^T (AX-B)
   *  Solving A^T (AX-B) = 0 for X yields
   *        A^T AX = A^T B
   *     =>      X = (A^T A)^(-1) A^T B
   */
  def pinv(X: DenseMatrix[Double]) : DenseMatrix[Double] = {
    requireNonEmptyMatrix(X)



    (X.t * X) \ X.t
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
      val eps: Double = 2.0 * lapack.dlamch("e")
      scala.math.max(m.cols, m.rows) * eps * s.max
    }
    s.data.count(_ > useTol)  // TODO: Use DenseVector[_].count() if/when that is implemented
  }

  /**
   * Raises m to the exp'th power via eigenvalue decomposition. Currently requires
   * that m's eigenvalues are real.
   * @param m
   * @param exp
   */
  def pow(m: DenseMatrix[Double], exp: Double) = {
    requireSquareMatrix(m)
    val (real, imag, evectors) = eig(m)
    require(norm(imag) == 0.0, "We cannot handle complex eigenvalues yet.")
    val exped = new DenseVector(real.data.map(scala.math.pow(_, exp)))

    (evectors.t \ (evectors * diag(exped)).t).t
  }

}


