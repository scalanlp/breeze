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
import io.{CSVWriter, CSVReader}
import linalg.operators._
import breeze.linalg.support.{LiteralRow, CanAxpy, CanCopy}
import breeze.linalg.support.tupleToDenseVector._
import math.Semiring
import storage.DefaultArrayValue
import java.io.{File, FileReader}
import scala.reflect.ClassTag
import breeze.macros.arityize


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


  object RangeExtender {
    val All = new Range(0, -1, 1)
  }
  implicit class RangeExtender(val re: Range) extends Range(re.start, re.end, re.step) {

    def getRangeWithoutNegativeIndexes(totalLength: Int): Range = {
        if(re.isInclusive){
          val (actualStart: Int, actualEnd: Int) =
            (
              if ( re.start < 0 ) totalLength + re.start else re.start  ,  //actualStart will be given as argument to inclusive range "to"

              if ( re.end < 0 ) totalLength + re.end else re.end  //actualEnd will be given as argument to inclusive range "to"
            )
          (actualStart to actualEnd by re.step)

        } else if( re.end < 0 || re.start < 0) {
            throw new IllegalArgumentException("cannot use negative end indexing with 'until', due to ambiguities from Range.end being exclusive")
        } else {
            re
        }
    }

  }

  import math.Ring
  import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}

/**
 * Basic linear algebraic operations.
 *
 * @author dlwh,dramage,retronym,afwlehmann,lancelet
 */
  //  import breeze.linalg._

  private[linalg] def requireNonEmptyMatrix[V](mat: Matrix[V]) =
    if (mat.cols == 0 || mat.rows == 0)
      throw new MatrixEmptyException

  private[linalg] def requireSquareMatrix[V](mat: Matrix[V]) =
    if (mat.rows != mat.cols)
      throw new MatrixNotSquareException

  private[linalg] def requireSymmetricMatrix[V](mat: Matrix[V]) = {
    requireSquareMatrix(mat)

    for (i <- 0 until mat.rows; j <- 0 until i)
      if (mat(i,j) != mat(j,i))
        throw new MatrixNotSymmetricException
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
    val as = argsort(a)
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
   * Performs a principal components analysis on the given numeric data
   * matrix and returns the results as an object of class PCA.
   *
   * If the no covariance matrix is supplied, one obtained from the given
   * data is used.
   */
  def princomp(
    x: DenseMatrix[Double],
    covmatOpt: Option[DenseMatrix[Double]] = None
  ) = {
    covmatOpt match {
      case Some(covmat) => new PCA(x, covmat)
      case None => new PCA(x, cov(x))
    }
  }

  /**
   * A generic function (based on the R function of the same name) whose
   * default method centers and/or scales the columns of a numeric matrix.
   * 
   * If ‘scale’ is ‘TRUE’ then scaling is done by dividing the (centered)
   * columns of ‘x’ by their standard deviations if ‘center’ is ‘TRUE’, and
   * the root mean square otherwise.  If ‘scale’ is ‘FALSE’, no scaling is
   * done.
   */
  def scale(
             x: DenseMatrix[Double],
             center: Boolean = true,
             scale: Boolean = false
             ) = {
    if (center) {
      val xc = x(*,::) - mean(x, Axis._0).toDenseVector
      if (scale)
        xc(*,::) :/ stddev(x(::, *)).toDenseVector
      else
        xc
    } else {
      if (scale)
        x(*,::) :/ columnRMS(x)
      else
        x
    }
  }

  /**
   * Compute the covariance matrix from the given data, centering
   * if necessary. Very simple, just does the basic thing.
   */
  def cov(x: DenseMatrix[Double], center: Boolean = true) = {
    val xc = scale(x,center,false)
    (xc.t * xc) /= xc.rows - 1.0
  }




  /**
   * Helper function to compute the root-mean-square of the columns of a
   * matrix. Feel free to make this more general.
   */
  private def columnRMS(x: DenseMatrix[Double]) = 
    (sum(x:*x,Axis._0) / (x.rows-1.0)).map(scala.math.sqrt).toDenseVector


//  /**
//   * @author ktakagaki
//   * @author dlwh
//   * @date 3/1/14.
//   */
//  implicit class EnrichedTuple[Row, V]( tuple: Row)(implicit lr: LiteralRow[Row, V], ct: ClassTag[V], dav: DefaultArrayValue[V]) {
//    def v = {
//      val len = lr.length(tuple)
//      val dv = DenseVector.zeros[V](len)
//      lr.foreach(tuple, dv(_) = _)
//      dv
//    }
//  }

  @arityize(22)
  implicit def tupleToDenseVecor[@arityize.replicate V](  tuple: Tuple[V @arityize.repeat]  ) = new TupleToDenseVector[V @arityize.repeat](tuple)


}


