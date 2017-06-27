package breeze.linalg
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
import breeze.linalg.Axis.{_0, _1}
import breeze.linalg.operators._
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.linalg.support._
import breeze.math._
import breeze.storage.Zero
import breeze.storage.Zero._
import breeze.util.ArrayUtil
import spire.syntax.cfor._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.{specialized => spec}
import scalaxy.debug._

trait CalculatesLinearIndex {
  def offset: Int
  def rows: Int
  def cols: Int
  def majorStride: Int
  def isTranspose: Boolean

  /** Calculates the index into the data array for row and column */
  def linearIndex(row: Int, col: Int): Int = {
    if(isTranspose)
      offset + col + row * majorStride
    else
      offset + row + col * majorStride
  }
}

trait MatrixPopulator[V] extends CalculatesLinearIndex {
  def rows: Int
  def cols: Int
  def offset: Int
  def majorStride: Int
  def isTranspose: Boolean
  def apply(i: Int, j: Int): V
  def data: Array[V]

  def checkDimensions = {
    if (isTranspose && (math.abs(majorStride) < cols) && majorStride != 0) { throw new IndexOutOfBoundsException("MajorStride == " + majorStride + " is smaller than cols == " + cols + ", which is impossible") }
    if (!isTranspose && (math.abs(majorStride) < rows) && majorStride != 0) { throw new IndexOutOfBoundsException("MajorStride == " + majorStride + " is smaller than rows == " + rows + ", which is impossible") }
    if (rows < 0) { throw new IndexOutOfBoundsException("Rows must be larger than zero. It was " + rows) }
    if (cols < 0) { throw new IndexOutOfBoundsException("Cols must be larger than zero. It was " + cols) }
    if (offset < 0) { throw new IndexOutOfBoundsException("Offset must be larger than zero. It was " + offset) }
    if ((majorStride < 0) && (linearIndex(0, cols-1) < 0)) {
      throw new IndexOutOfBoundsException("Storage array has negative stride " + majorStride + " and offset " + offset + " which can result in negative indices.")
    }
  }
}

case class SimpleMatrixPopulator[V](val rows: Int, val cols: Int, val offset: Int, val majorStride: Int, val isTranspose: Boolean, val data: Array[V]) extends MatrixPopulator[V] {
  override def apply(i: Int, j: Int): V = data(linearIndex(i, j))

  override def checkDimensions = {
    super.checkDimensions

    if (majorStride > 0) { //This is only necessary when the data array is provided.
      if (data.length < linearIndex(rows-1, cols-1)) { throw new IndexOutOfBoundsException("Storage array has size " + data.size + " but indices can grow as large as " + linearIndex(rows-1,cols-1)) }
    } else if (majorStride < 0) {
      if (data.length< linearIndex(rows-1,0)) { throw new IndexOutOfBoundsException("Storage array has size " + data.size + " but indices can grow as large as " + linearIndex(rows-1,cols-1)) }
    }
  }
}

abstract class FunctionMatrixPopulator[V](val rows: Int, val cols: Int)(implicit val tag: ClassTag[V]) extends MatrixPopulator[V] {
  val offset = 0
  val majorStride = rows
  val isTranspose = false

  def data: Array[V] = {
    val result = new Array[V](rows*cols)
    cforRange(0 until rows)(i => {
      cforRange(0 until cols)(j => {
        result(linearIndex(i,j)) = apply(i,j)
      })
    })
    result
  }
}

abstract class DiagonalMatrixPopulator[V:ClassTag:Zero](dim: Int) extends MatrixPopulator[V] {
  val rows = dim
  val cols = dim
  val offset = 0
  val majorStride = rows
  val isTranspose = false

  def data: Array[V] = {
    val result = new Array[V](rows*cols)
    cforRange(0 until rows)(i => {
      result(linearIndex(i,i)) = apply(i,i)
    })
    result
  }
}

/**
 * A DenseMatrix is a matrix with all elements found in an array. It is column major unless isTranspose is true,
 * It is designed to be fast: Double- (and potentially Float-)valued DenseMatrices
 * can be used with blas, and support operations to that effect.
 *
 * @author dlwh
 * @param rows number of rows
 * @param cols number of cols
 * @param data The underlying data.
 *             Column-major unless isTranpose is true.
 *             Mutate at your own risk.
 *             Note that this matrix may be a view of the data.
 *             Use linearIndex(r,c) to calculate indices.
 * @param offset starting point into array
 * @param majorStride distance separating columns (or rows, for isTranspose). should have absolute value >= rows (or cols, for isTranspose)
 * @param isTranspose if true, then the matrix is considered to be "transposed" (that is, row major)
 */
@SerialVersionUID(1L)
final class DenseMatrix[@spec(Double, Int, Float, Long) V](private var populator: MatrixPopulator[V])
  extends Matrix[V] with MatrixLike[V, DenseMatrix[V]] with CalculatesLinearIndex with Serializable {

  lazy val rows = populator.rows
  lazy val cols = populator.cols
  lazy val offset = populator.offset
  lazy val majorStride = populator.majorStride
  lazy val isTranspose = populator.isTranspose
  populator.checkDimensions
  private def initializeDimensions = {
    rows
    cols
    offset
    majorStride
    isTranspose
  }

  def this(rows: Int, cols: Int, data: Array[V], offset: Int, majorStride: Int, isTranspose: Boolean = false) = this(SimpleMatrixPopulator(rows, cols, offset, majorStride, isTranspose, data))
  /** Creates a matrix with the specified data array, rows, and columns. */
  def this(rows: Int, cols: Int)(implicit man: ClassTag[V]) = this(rows, cols, new Array[V](rows * cols), 0, rows)
  /** Creates a matrix with the specified data array, rows, and columns. Data must be column major */
  def this(rows: Int, cols: Int, data: Array[V], offset: Int) = this(rows, cols, data, offset, rows)
  /** Creates a matrix with the specified data array, rows, and columns. Data must be column major */
  def this(rows: Int, cols: Int, data: Array[V]) = this(rows, cols, data, 0, rows)
  /** Creates a matrix with the specified data array and rows. columns inferred automatically */
  def this(rows: Int, data: Array[V], offset: Int) = this(rows, {assert(data.length % rows == 0); data.length/rows}, data, offset)

  private var myData: Array[V] = null
  lazy val data: Array[V] = if (populator == null) {
    myData
  } else {
    this.synchronized {
      if (populator != null) {
        myData = populator.data
        initializeDimensions
        populator = null
      }
    }
    myData
  }


  def apply(row: Int, col: Int) = {
    if(row < - rows || row >= rows) throw new IndexOutOfBoundsException((row,col) + " not in [-"+rows+","+rows+") x [-"+cols+"," + cols+")")
    if(col < - cols || col >= cols) throw new IndexOutOfBoundsException((row,col) + " not in [-"+rows+","+rows+") x [-"+cols+"," + cols+")")
    val trueRow = if(row<0) row + rows else row
    val trueCol = if(col<0) col + cols else col
    if (populator == null) {
      data(linearIndex(trueRow, trueCol))
    } else {
      populator.apply(trueRow, trueCol)
    }
  }

  // don't delete
  DenseMatrix.init()

  def rowColumnFromLinearIndex(index: Int): (Int, Int) = {
    val r = (index - offset)%majorStride
    val c = (index - offset)/majorStride
    if(isTranspose) {
      (c, r)
    } else {
      (r,c)
    }
  }

  def update(row: Int, col: Int, v: V): Unit = {
    if(row < - rows || row >= rows) throw new IndexOutOfBoundsException((row,col) + " not in [-"+rows+","+rows+") x [-"+cols+"," + cols+")")
    if(col < - cols || col >= cols) throw new IndexOutOfBoundsException((row,col) + " not in [-"+rows+","+rows+") x [-"+cols+"," + cols+")")
    val trueRow = if(row<0) row + rows else row
    val trueCol = if(col<0) col + cols else col
    data(linearIndex(trueRow, trueCol)) = v
  }

  @deprecated("This isn't actually any faster according to benchmarks", "0.12-SNAPSHOT")
  def unsafeUpdate(row: Int, col: Int, v: V): Unit = { data(linearIndex(row, col)) = v }

  // <editor-fold defaultstate="collapsed" desc=" conversions (toArray, toDenseVector) ">

  /** Converts this matrix to a flat Array (column-major) */
  def toArray: Array[V] = {
    implicit val man = ClassTag[V](data.getClass.getComponentType.asInstanceOf[Class[V]])
    val ret = new Array[V](rows * cols)
    var i = 0
    while (i < cols) {
      var j = 0
      while (j < rows) {
        ret(i * rows + j) = data(linearIndex(j, i))
        j += 1
      }
      i += 1
    }
    ret
  }

  /** Converts this matrix to a DenseVector (column-major) */
  def toDenseVector: DenseVector[V] = DenseVector( toArray )

  // </editor-fold>

  /** Converts this matrix to a DenseVector (column-major)
    * If view = true (or View.Require), throws an exception if we cannot return a view. otherwise returns a view.
    * If view == false (or View.Copy) returns a copy
    * If view == View.Prefer (the default), returns a view if possible, otherwise returns a copy.
    *
    * Views are only possible (if(isTranspose) majorStride == cols else majorStride == rows) == true
    */
  def flatten(view: View=View.Prefer): DenseVector[V] = view match {
    case View.Require =>
      if(!canFlattenView)
        throw new UnsupportedOperationException("Cannot make a view of this matrix.")
      else
        DenseVector.create(data, offset, 1, rows * cols)
    case View.Copy =>
      toDenseVector
    case View.Prefer =>
      flatten(canFlattenView)
  }


  private def canFlattenView = !isTranspose && majorStride == rows
  private def canReshapeView = canFlattenView

  /** Reshapes this matrix to have the given number of rows and columns
    * If view = true (or View.Require), throws an exception if we cannot return a view. otherwise returns a view.
    * If view == false (or View.Copy) returns a copy
    * If view == View.Prefer (the default), returns a view if possible, otherwise returns a copy.
    *
    * Views are only possible if (!isTranspose && majorStride == rows)
    *
    * rows * cols must equal size, or cols < 0 && (size / rows * rows == size)
    * @param rows the number of rows
    * @param cols the number of columns, or -1 to auto determine based on size and rows
    */
  def reshape(rows: Int, cols: Int, view: View=View.Prefer): DenseMatrix[V] = {
    val _cols = cols//if(cols < 0) size / rows else cols
    require(rows * _cols == size, "Cannot reshape a (%d,%d) matrix to a (%d,%d) matrix!".format(this.rows, this.cols, rows, _cols))

    view match {
      case View.Require =>
        if(!canReshapeView)
          throw new UnsupportedOperationException("Cannot make a view of this matrix.")
        else
          new DenseMatrix(rows, _cols, data, offset, if(isTranspose) cols else rows, isTranspose)
      case View.Copy =>
        // calling copy directly gives a verify error. TODO: submit bug
        val result = new DenseMatrix(this.rows, this.cols, ArrayUtil.newArrayLike(data, size))
        result := this
        result.reshape(rows, _cols, View.Require)
      case View.Prefer =>
        reshape(rows, cols, canReshapeView)
    }
  }

  def repr: DenseMatrix[V] = this

  def activeIterator: Iterator[((Int, Int), V)] = iterator

  def activeValuesIterator: Iterator[V] = valuesIterator

  def activeKeysIterator: Iterator[(Int, Int)] = keysIterator

  /** Computes the sum along the diagonal. */
  @deprecated("use trace(dm) instead", "0.6")
  def trace(implicit numeric: Numeric[V]): V = diag(this:DenseMatrix[V]).sum

  def activeSize = data.length

  def valueAt(i: Int): V = data(i)
  def valueAt(row: Int, col: Int): V = apply(row,col)

  @deprecated("This isn't actually any faster according to benchmarks", "0.12-SNAPSHOT")
  def unsafeValueAt(row: Int, col: Int): V = data(linearIndex(row, col))

  def indexAt(i: Int) = i

  def isActive(i: Int) = true
  def allVisitableIndicesActive = true


  override def toDenseMatrix(implicit cm: ClassTag[V], zero: Zero[V]): DenseMatrix[V] = {
    val result = DenseMatrix.create[V](rows, cols, new Array[V](size))
    result := this
    result
  }

  def copy: DenseMatrix[V] = {
    implicit val man = ClassTag[V](data.getClass.getComponentType.asInstanceOf[Class[V]])
    val result = DenseMatrix.create[V](rows, cols, new Array[V](size))
    result := this
    result
  }

  // TODO: HACK!
  private implicit def dontNeedZero[V]: Zero[V] = null.asInstanceOf[Zero[V]]


  def delete(row: Int, axis: Axis._0.type): DenseMatrix[V] = {
    implicit val man = ClassTag[V](data.getClass.getComponentType.asInstanceOf[Class[V]])
    require(row >= 0 && row < rows, s"row $row is not in bounds: [0, $rows)")
    if (row == 0) this(1 until rows, ::).copy
    else if (row == rows - 1) this(0 until rows-1, ::).copy
    else DenseMatrix.vertcat(this(0 until row, ::), this((row+1) until rows, ::))
  }

  def delete(col: Int, axis: Axis._1.type): DenseMatrix[V] = {
    implicit val man = ClassTag[V](data.getClass.getComponentType.asInstanceOf[Class[V]])
    require(col >= 0 && col < cols, s"col $col is not in bounds: [0, $cols)")
    if (col == 0) this(::, 1 until cols).copy
    else if (col == cols - 1) this(::, 0 until cols-1).copy
    else DenseMatrix.horzcat(this(::, 0 until col), this(::, (col+1) until cols))
  }

  def delete(rows: Seq[Int], axis: Axis._0.type): DenseMatrix[V] = {
    implicit val man = ClassTag[V](data.getClass.getComponentType.asInstanceOf[Class[V]])
    if(rows.isEmpty) copy
    else if(rows.size == 1) delete(rows(0), axis)
    else {
      val sorted = rows.sorted
      require(sorted.head >= 0 && sorted.last < this.rows, s"row $rows are not in bounds: [0, ${this.rows})")
      var last = 0
      val matrices = ArrayBuffer[DenseMatrix[V]]()
      for(index <- sorted) {
        assert(index >= last)
        if(index != last) {
          matrices += this(last until index, ::)
        }
        last = index + 1
      }
      if(last != this.rows) {
        matrices += this(last until this.rows, ::)
      }
      DenseMatrix.vertcat(matrices:_*)
    }
  }


  def delete(cols: Seq[Int], axis: Axis._1.type): DenseMatrix[V] = {
    implicit val man = ClassTag[V](data.getClass.getComponentType.asInstanceOf[Class[V]])
    if(cols.isEmpty) copy
    else if(cols.size == 1) delete(cols(0), axis)
    else {
      val sorted = cols.sorted
      require(sorted.head >= 0 && sorted.last < this.cols, s"col $cols are not in bounds: [0, ${this.cols})")
      var last = 0
      val matrices = ArrayBuffer[DenseMatrix[V]]()
      for(index <- sorted) {
        assert(index >= last)
        if(index != last) {
          matrices += this(::, last until index)
        }
        last = index + 1
      }
      if(last != this.cols) {
        matrices += this(::, last until this.cols)
      }
      DenseMatrix.horzcat(matrices:_*)
    }
  }

  private def majorSize = if(isTranspose) rows else cols
  private def footprint = majorSize * majorStride
  /** Returns true if this dense matrix takes up a contiguous segment of the array */
  def isContiguous: Boolean = (isTranspose && cols == majorStride) || (!isTranspose && rows == majorStride)


  /** Returns true if this dense matrix overlaps any content with the other matrix */
  private[linalg] def overlaps(other: DenseMatrix[V]):Boolean = (this.data eq other.data) && {
    val astart = offset
    val aend = offset+ footprint
    val bstart = other.offset
    val bend = other.offset + other.footprint
    Range(astart, aend).contains(bstart) ||
      Range(astart, aend).contains(bend) ||
      Range(bstart, bend).contains(astart) ||
      Range(bstart, bend).contains(aend)
  }

  private def checkIsSpecialized(): Unit = {
    if(data.isInstanceOf[Array[Double]] && getClass.getName() == "breeze.linalg.DenseMatrix") throw new Exception("...")
  }
  // uncomment to debug places where specialization fails
//  checkIsSpecialized()

}

object DenseMatrix extends LowPriorityDenseMatrix
with DenseMatrixOps
with DenseMatrix_OrderingOps
with DenseMatrixMultOps
with DenseMatrixMultiplyStuff
with DenseMatrixFloatMultiplyStuff
with MatrixConstructors[DenseMatrix] {

  /**
   * The standard way to create an empty matrix, size is rows * cols
   */
  def zeros[@spec(Double, Int, Float, Long) V:ClassTag:Zero](rows: Int, cols: Int): DenseMatrix[V] = {
    val data = new Array[V](rows * cols)
    if(implicitly[Zero[V]] != null && rows * cols != 0 && data(0) != implicitly[Zero[V]].zero)
      ArrayUtil.fill(data, 0, data.length, implicitly[Zero[V]].zero)
    DenseMatrix.create(rows, cols, data)
  }

  /**
   *
   * Creates a new DenseMatrix using the provided array (not making a copy!). In generic contexts, prefer to
   * use this (or the other create methd) instead of `new DenseMatrix[V](rows, cols, data)`, which in general
   * won't give specialized implementations.
   * @param rows
   * @param cols
   * @param data
   * @tparam V
   * @return
   */
  def create[@spec(Double, Int, Float, Long) V:Zero](rows: Int, cols: Int, data: Array[V]): DenseMatrix[V] = {
    create(rows, cols, data, 0, rows, isTranspose = false)
  }

  /**
   *
   * Creates a new DenseMatrix using the provided array (not making a copy!). In generic contexts, prefer to
   * use this (or the other create methd) instead of `new DenseMatrix[V](rows, cols, data)`, which in general
   * won't give specialized implementations.
   * @param rows
   * @param cols
   * @param data
   * @tparam V
   * @return
   */
  def create[@spec(Double, Int, Float, Long) V](rows: Int, cols: Int, data: Array[V], offset: Int, majorStride: Int, isTranspose: Boolean = false): DenseMatrix[V] = {
    (data: Any) match {
      case d: Array[Double] => new DenseMatrix(rows, cols, d, offset, majorStride, isTranspose).asInstanceOf[DenseMatrix[V]]
      case d: Array[Float] => new DenseMatrix(rows, cols, d, offset, majorStride, isTranspose).asInstanceOf[DenseMatrix[V]]
      case d: Array[Long] => new DenseMatrix(rows, cols, d, offset, majorStride, isTranspose).asInstanceOf[DenseMatrix[V]]
      case d: Array[Int] => new DenseMatrix(rows, cols, d, offset, majorStride, isTranspose).asInstanceOf[DenseMatrix[V]]
      case _ => new DenseMatrix(rows, cols, data, offset, majorStride, isTranspose)
    }
  }


  /**
   * Creates a matrix of all ones.
   * @param rows
   * @param cols
   * @tparam V
   * @return
   */
  override def ones[@specialized(Int, Float, Double, Long) V: ClassTag : Zero : Semiring](rows: Int, cols: Int): DenseMatrix[V] = {
    val data = new Array[V](rows * cols)
    if(rows * cols != 0 && data(0) != implicitly[Semiring[V]].one)
      ArrayUtil.fill(data, 0, data.length, implicitly[Semiring[V]].one)
    DenseMatrix.create(rows, cols, data)
  }

  private class IdentityMatrixPopulator[@spec(Double, Int, Float, Long) V: ClassTag:Zero:Semiring](dim: Int) extends DiagonalMatrixPopulator[V](dim) {
    override def apply(i: Int, j: Int): V = if (i == j) { implicitly[Semiring[V]].one } else { implicitly[Semiring[V]].zero }
  }

  /**
   * Creates a square diagonal array of size dim x dim, with 1's along the diagonal.
   */
  def eye[@spec(Double, Int, Float, Long) V: ClassTag:Zero:Semiring](dim: Int): DenseMatrix[V] = {
    new DenseMatrix(populator = new IdentityMatrixPopulator(dim))
  }

  /** Horizontally tiles some matrices. They must have the same number of rows */
  def horzcat[M,V](matrices: M*)(implicit ev: M <:< Matrix[V], opset: OpSet.InPlaceImpl2[DenseMatrix[V], M], vman: ClassTag[V], zero: Zero[V]) = {
    if(matrices.isEmpty) zeros[V](0,0)
    else {
      require(matrices.forall(m => m.rows == matrices(0).rows),"Not all matrices have the same number of rows")
      val numCols = matrices.foldLeft(0)(_ + _.cols)
      val numRows = matrices(0).rows
      val res = DenseMatrix.zeros[V](numRows,numCols)
      var offset = 0
      for(m <- matrices) {
        res(0 until numRows,(offset) until (offset + m.cols)) := m
        offset+= m.cols
      }
      res
    }
  }

  /** Vertically tiles some matrices. They must have the same number of columns */
  def vertcat[V](matrices: DenseMatrix[V]*)(implicit opset: OpSet.InPlaceImpl2[DenseMatrix[V], DenseMatrix[V]], vman: ClassTag[V], zero: Zero[V]) = {
    if(matrices.isEmpty) zeros[V](0,0)
    else {
      require(matrices.forall(m => m.cols == matrices(0).cols),"Not all matrices have the same number of columns")
      val numRows = matrices.foldLeft(0)(_ + _.rows)
      val numCols = matrices(0).cols
      val res = DenseMatrix.zeros[V](numRows,numCols)
      var offset = 0
      for(m <- matrices) {
        res((offset) until (offset + m.rows),0 until numCols) := m
        offset+= m.rows
      }
      res
    }
  }


  // zerosLike
  implicit def canCreateZerosLike[V:ClassTag:Zero]: CanCreateZerosLike[DenseMatrix[V], DenseMatrix[V]] =
    new CanCreateZerosLike[DenseMatrix[V], DenseMatrix[V]] {
      def apply(v1: DenseMatrix[V]): DenseMatrix[V] = {
        zeros[V](v1.rows,v1.cols)
      }
    }

  // slices


  implicit def canSliceCol[V]: CanSlice2[DenseMatrix[V], ::.type, Int, DenseVector[V]] = {
    new CanSlice2[DenseMatrix[V], ::.type, Int, DenseVector[V]] {
      def apply(m: DenseMatrix[V], ignored: ::.type, colWNegative: Int) = {

        if(colWNegative < -m.cols || colWNegative >= m.cols) throw new ArrayIndexOutOfBoundsException("Column must be in bounds for slice!")
        val col = if(colWNegative<0) colWNegative+m.cols else colWNegative

        if(!m.isTranspose)
          DenseVector.create(m.data, length = m.rows, offset = col * m.majorStride + m.offset, stride=1)
        else
          DenseVector.create(m.data, length=m.rows, offset = m.offset + col, stride = m.majorStride)
      }
    }
  }

  implicit def canSliceRow[V]: CanSlice2[DenseMatrix[V], Int, ::.type, Transpose[DenseVector[V]]] = {
    new CanSlice2[DenseMatrix[V], Int, ::.type, Transpose[DenseVector[V]]] {
      def apply(m: DenseMatrix[V], rowWNegative: Int, ignored: ::.type) = {
        canSliceCol[V].apply(m.t, ::, rowWNegative).t
      }
    }
  }

  implicit def canSliceRows[V]: CanSlice2[DenseMatrix[V], Range, ::.type, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], Range, ::.type, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], rowsWNegative: Range, ignored: ::.type) = {

        val rows = rowsWNegative.getRangeWithoutNegativeIndexes(m.rows)

        if(rows.isEmpty) DenseMatrix.create(0, m.cols, m.data, 0, 0)
        else if(!m.isTranspose) {
          require(rows.step == 1, "Sorry, we can't support row ranges with step sizes other than 1")
          val first = rows.head
          require(rows.last < m.rows)
          if(rows.last >= m.rows) {
            throw new IndexOutOfBoundsException(s"Row slice of $rows was bigger than matrix rows of ${m.rows}")
          }
          DenseMatrix.create(rows.length, m.cols, m.data, m.offset + first, m.majorStride)
        } else {
          canSliceCols(m.t, ::, rows).t
        }
      }
    }
  }

  implicit def canSliceCols[V]: CanSlice2[DenseMatrix[V], ::.type, Range, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], ::.type, Range, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], ignored: ::.type, colsWNegative: Range) = {

        val cols = colsWNegative.getRangeWithoutNegativeIndexes(m.cols)

        if(cols.isEmpty) {
          DenseMatrix.create(m.rows, 0, m.data, 0, m.rows)
        } else if(!m.isTranspose) {
          val first = cols.head
          if(cols.last >= m.cols) {
            throw new IndexOutOfBoundsException(s"Col slice of $cols was bigger than matrix cols of ${m.cols}")
          }
          DenseMatrix.create(m.rows, cols.length, m.data, m.offset + first * m.majorStride, m.majorStride * cols.step )
        } else {
          canSliceRows(m.t, cols, ::).t
        }
      }
    }
  }

  implicit def canSliceColsAndRows[V]: CanSlice2[DenseMatrix[V], Range, Range, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], Range, Range, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], rowsWNegative: Range, colsWNegative: Range) = {

        val rows = rowsWNegative.getRangeWithoutNegativeIndexes(m.rows)
        val cols = colsWNegative.getRangeWithoutNegativeIndexes(m.cols)

        if(rows.isEmpty || cols.isEmpty) DenseMatrix.create(rows.size, cols.size, m.data, 0, 0)
        else if(!m.isTranspose) {
          require(rows.step == 1, "Sorry, we can't support row ranges with step sizes other than 1 for non transposed matrices")
          val first = cols.head
          if(rows.last >= m.rows) {
            throw new IndexOutOfBoundsException(s"Row slice of $rows was bigger than matrix rows of ${m.rows}")
          }
          if(cols.last >= m.cols) {
            throw new IndexOutOfBoundsException(s"Col slice of $cols was bigger than matrix cols of ${m.cols}")
          }
          DenseMatrix.create(rows.length, cols.length, m.data, m.offset + first * m.majorStride + rows.head, m.majorStride * cols.step)
        } else {
          require(cols.step == 1, "Sorry, we can't support col ranges with step sizes other than 1 for transposed matrices")
          canSliceColsAndRows(m.t, cols, rows).t
        }
      }
    }
  }



  implicit def negFromScale[V](implicit scale: OpMulScalar.Impl2[DenseMatrix[V], V, DenseMatrix[V]], field: Ring[V]) = {
    new OpNeg.Impl[DenseMatrix[V], DenseMatrix[V]] {
      override def apply(a : DenseMatrix[V]) = {
        scale(a, field.negate(field.one))
      }
    }
  }



  implicit def canSlicePartOfCol[V]: CanSlice2[DenseMatrix[V], Range, Int, DenseVector[V]] = {
    new CanSlice2[DenseMatrix[V], Range, Int, DenseVector[V]] {
      def apply(m: DenseMatrix[V], rowsWNegative: Range, colWNegative: Int) = {

        val rows:Range = rowsWNegative.getRangeWithoutNegativeIndexes(m.rows)
        if(colWNegative < -m.cols || colWNegative >= m.cols) throw new ArrayIndexOutOfBoundsException("Row must be in bounds for slice!")
        val col = if(colWNegative<0) colWNegative + m.cols else colWNegative

        if(rows.isEmpty) {
          DenseVector.create(m.data, 0, 0, 0)
        } else if(!m.isTranspose) {
          if(rows.last >= m.rows) {
            throw new IndexOutOfBoundsException(s"Row slice of $rows was bigger than matrix rows of ${m.rows}")
          }
          DenseVector.create(m.data, col * m.majorStride + m.offset + rows.head, rows.step, rows.length)
        } else {
          // row major, so consecutive rows are separated by m.majorStride
          // we move rows.step * m.majorStride per step in the range
          DenseVector.create(m.data, m.offset + col + rows.head * m.majorStride, m.majorStride * rows.step, rows.length)
        }
      }
    }
  }

  implicit def canSlicePartOfRow[V]: CanSlice2[DenseMatrix[V], Int, Range, Transpose[DenseVector[V]]] = {
    new CanSlice2[DenseMatrix[V], Int, Range, Transpose[DenseVector[V]]] {
      def apply(m: DenseMatrix[V], rowWNegative: Int, colsWNegative: Range) = {
         canSlicePartOfCol[V].apply(m.t, colsWNegative, rowWNegative).t
      }
    }
  }

  implicit def canMapValues[@specialized(Int, Float, Double) V, @specialized(Int, Float, Double) R](implicit r: ClassTag[R]): CanMapValues[DenseMatrix[V], V, R, DenseMatrix[R]] = {
    new CanMapValues[DenseMatrix[V],V,R,DenseMatrix[R]] {

      override def apply(from : DenseMatrix[V], fn : (V=>R)): DenseMatrix[R] = {
        if (from.isContiguous) {
          val data = new Array[R](from.size)
          val isTranspose = from.isTranspose
          val off = from.offset
          val fd = from.data
          if (off == 0) {
            var i = 0
            val iMax = data.length
            while (i < iMax) {
              data(i) = fn(fd(i))
              i += 1
            }
          } else {
            var i = 0
            val iMax = data.length
            while (i < iMax) {
              data(i) = fn(fd(i + off))
              i += 1
            }
          }
          DenseMatrix.create(from.rows, from.cols, data, 0, if (isTranspose) from.cols else from.rows, isTranspose)
        } else {
          val data = new Array[R](from.size)
          var j = 0
          var off = 0
          while (j < from.cols) {
            var i = 0
            while (i < from.rows) {
              data(off) = fn(from(i, j))
              off += 1
              i += 1
            }
            j += 1
          }
          DenseMatrix.create[R](from.rows, from.cols, data, 0, from.rows)
        }
      }

    }
  }

  implicit def scalarOf[T]: ScalarOf[DenseMatrix[T], T] = ScalarOf.dummy

  implicit def canTraverseValues[V]: CanTraverseValues[DenseMatrix[V], V] = {
    new CanTraverseValues[DenseMatrix[V], V] {
      def isTraversableAgain(from: DenseMatrix[V]): Boolean = true


      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: DenseMatrix[V], fn: ValuesVisitor[V]): Unit = {
        import from._
        val idealMajorStride = if(isTranspose) cols else rows

        if(majorStride == idealMajorStride) {
          fn.visitArray(data, offset, rows*cols, 1)
        } else if(!from.isTranspose) {
          var j = 0
          while (j < from.cols) {
            fn.visitArray(data, offset + j * majorStride, rows, 1)
            j += 1
          }
        } else {
          var j = 0
          while (j < from.cols) {
            var i = 0
            while(i < from.rows) {
              fn.visit(from(i, j))
              i += 1
            }
            j += 1
          }
        }
      }

    }
  }

  implicit def canTraverseKeyValuePairs[V]: CanTraverseKeyValuePairs[DenseMatrix[V], (Int, Int), V] = {
    new CanTraverseKeyValuePairs[DenseMatrix[V], (Int, Int), V] {
      def isTraversableAgain(from: DenseMatrix[V]): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: DenseMatrix[V], fn: CanTraverseKeyValuePairs.KeyValuePairsVisitor[(Int, Int), V]): Unit = {
        import from._
        val idealMajorStride = if(isTranspose) cols else rows

        if(majorStride == idealMajorStride) {
          fn.visitArray(from.rowColumnFromLinearIndex, data, offset, rows*cols, 1)
        } else if(!from.isTranspose) {
          var j = 0
          while (j < from.cols) {
            fn.visitArray(from.rowColumnFromLinearIndex, data, offset + j * majorStride, rows, 1)
            j += 1
          }
        } else {
          var j = 0
          while (j < from.cols) {
            var i = 0
            while(i < from.rows) {
              fn.visit((i,j), from(i, j))
              i += 1
            }
            j += 1
          }
        }
      }

    }
  }


  implicit def canTransformValues[@specialized(Int, Float, Double) V]:CanTransformValues[DenseMatrix[V], V] = {
    new CanTransformValues[DenseMatrix[V], V] {
      def transform(from: DenseMatrix[V], fn: (V) => V) {
        if (from.isContiguous) {
          val d = from.data
          cforRange(from.offset until from.offset + from.size) { j =>
            d(j) = fn(d(j))
          }
        } else {
          slowPath(from, fn)
        }
      }

      private def slowPath(from: DenseMatrix[V], fn: (V) => V): Unit = {
        var j = 0
        while (j < from.cols) {
          var i = 0
          while (i < from.rows) {
            from(i, j) = fn(from(i, j))
            i += 1
          }
          j += 1
        }
      }

      def transformActive(from: DenseMatrix[V], fn: (V) => V) {
        transform(from, fn)
      }
    }
  }

  implicit def canMapKeyValuePairs[V, R:ClassTag] = {
    new CanMapKeyValuePairs[DenseMatrix[V],(Int,Int),V,R,DenseMatrix[R]] {
      override def map(from : DenseMatrix[V], fn : (((Int,Int),V)=>R)) = {
        val data = new Array[R](from.data.length)
        var j = 0
        var off = 0
        while (j < from.cols) {
          var i = 0
          while(i < from.rows) {
            data(off) = fn(i -> j, from(i, j))
            off += 1
            i += 1
          }
          j += 1
        }
        DenseMatrix.create(from.rows, from.cols, data, 0, from.rows)
      }

      override def mapActive(from : DenseMatrix[V], fn : (((Int,Int),V)=>R)) =
        map(from, fn)
    }
  }

  implicit def canTranspose[V]: CanTranspose[DenseMatrix[V], DenseMatrix[V]] = {
    new CanTranspose[DenseMatrix[V], DenseMatrix[V]] {
      def apply(from: DenseMatrix[V]) = {
        DenseMatrix.create(data = from.data, offset = from.offset, cols = from.rows, rows = from.cols, majorStride = from.majorStride, isTranspose = !from.isTranspose)
      }
    }
  }

  implicit def canTransposeComplex: CanTranspose[DenseMatrix[Complex], DenseMatrix[Complex]] = {
    new CanTranspose[DenseMatrix[Complex], DenseMatrix[Complex]] {
      def apply(from: DenseMatrix[Complex]) = {
        new DenseMatrix(data = from.data map { _.conjugate },
          offset = from.offset,
          cols = from.rows,
          rows = from.cols,
          majorStride = from.majorStride,
          isTranspose = !from.isTranspose)
      }
    }
  }

  implicit def canCopyDenseMatrix[V:ClassTag] = new CanCopy[DenseMatrix[V]] {
    def apply(v1: DenseMatrix[V]) = {
      v1.copy
    }
  }

  def binaryOpFromUpdateOp[Op<:OpType, V, Other](implicit copy: CanCopy[DenseMatrix[V]],
                                                 op: UFunc.InPlaceImpl2[Op, DenseMatrix[V], Other],
                                                 man: ClassTag[V]):  UFunc.UImpl2[Op, DenseMatrix[V], Other, DenseMatrix[V]]  = {
    new UFunc.UImpl2[Op, DenseMatrix[V], Other, DenseMatrix[V]] {
      override def apply(a : DenseMatrix[V], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  /*
  implicit def binaryLeftMulOpFromBinaryRightOp[V](implicit op: OpMulScalar.Impl2[DenseMatrix[V], V, DenseMatrix[V]]) = {
    new OpMulScalar.Impl2[V, DenseMatrix[V], DenseMatrix[V]] {
      override def apply(a : V, b: DenseMatrix[V]) = {
        op(b, a)
      }
    }
  }
  */

  /**
   * transforms each row into a new row, giving a new matrix.
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def canMapRows[V, R:ClassTag:Zero](implicit implSet: OpSet.InPlaceImpl2[DenseVector[R], DenseVector[R]]): CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], DenseVector[R], DenseMatrix[R]]  = new CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], DenseVector[R], DenseMatrix[R]] {
    def apply(from: DenseMatrix[V], axis: Axis._0.type)(f: (DenseVector[V]) => DenseVector[R]): DenseMatrix[R] = {
      var result:DenseMatrix[R] = null
      for(c <- 0 until from.cols) {
        val col = f(from(::, c))
        if(result eq null) {
          result = DenseMatrix.zeros[R](col.length, from.cols)
        }
        result(::, c) := col
      }

      if(result eq null){
        DenseMatrix.zeros[R](0, from.cols)
      } else {
        result
      }
    }
  }

  implicit def handholdCanMapRows[V]: CanCollapseAxis.HandHold[DenseMatrix[V], Axis._0.type, DenseVector[V]] = new CanCollapseAxis.HandHold[DenseMatrix[V], Axis._0.type, DenseVector[V]]()

  implicit def canMapRowsBitVector[V:ClassTag:Zero]: CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], BitVector, DenseMatrix[Boolean]]  = new CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], BitVector, DenseMatrix[Boolean]] {
    def apply(from: DenseMatrix[V], axis: Axis._0.type)(f: (DenseVector[V]) => BitVector): DenseMatrix[Boolean] = {
      var result:DenseMatrix[Boolean] = null
      for(c <- 0 until from.cols) {
        val col = f(from(::, c))
        if(result eq null) {
          result = DenseMatrix.zeros[Boolean](col.length, from.cols)
        }
        result(::, c) := col
      }

      if(result eq null){
        DenseMatrix.zeros[Boolean](0, from.cols)
      } else {
        result
      }
    }
  }


  /**
   * transforms each column into a new column, giving a new matrix.
   * @tparam V value type
   * @return
   */
  implicit def canMapCols[V, Res:ClassTag:Zero](implicit implSet: OpSet.InPlaceImpl2[DenseVector[Res], DenseVector[Res]]): CanCollapseAxis[DenseMatrix[V], _1.type, DenseVector[V], DenseVector[Res], DenseMatrix[Res]] = {
    new CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], DenseVector[Res], DenseMatrix[Res]] {
      def apply (from: DenseMatrix[V], axis: Axis._1.type) (f: (DenseVector[V] ) => DenseVector[Res] ): DenseMatrix[Res] = {
        var result: DenseMatrix[Res] = null
        import from.rows
        val t = from.t
        for (r <- 0 until from.rows) {
          val row = f (t (::, r) )
          if (result eq null) {
            // scala has decided this method is overloaded, and needs a result type.
            // It has a result type, and is not overloaded.
            //          result = DenseMatrix.zeros[V](from.rows, row.length)
            val data = new Array[Res] (rows * row.length)
            result = DenseMatrix.create(rows, row.length, data)
          }
          result.t apply (::, r) := row
        }

        if (result ne null) {
          result
        } else {
          val data = new Array[Res] (0)
          result = DenseMatrix.create(rows, 0, data)
          result
        }
      }
    }
  }

  implicit def handholdCanMapCols[V]: CanCollapseAxis.HandHold[DenseMatrix[V], Axis._1.type, DenseVector[V]] = new CanCollapseAxis.HandHold[DenseMatrix[V], Axis._1.type, DenseVector[V]]()

  implicit def canMapColsBitVector[V:ClassTag:Zero] = new CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], BitVector, DenseMatrix[Boolean]] {
    def apply(from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => BitVector): DenseMatrix[Boolean] = {
      var result:DenseMatrix[Boolean] = null
      import from.rows
      val t = from.t
      for(r <- 0 until from.rows) {
        val row = f(t(::, r))
        if(result eq null) {
          // scala has decided this method is overloaded, and needs a result type.
          // It has a result type, and is not overloaded.
          //          result = DenseMatrix.zeros[V](from.rows, row.length)
          val data = new Array[Boolean](rows * row.length)
          result = DenseMatrix.create(rows, row.length, data)
        }
        result.t apply (::, r) := row
      }

      if(result ne null) {
        result
      } else {
        val data = new Array[Boolean](0)
        result = DenseMatrix.create(rows, 0, data)
        result
      }
    }
  }


  /**
   * Iterates over each columns
   * @return
   */
  implicit def canTraverseCols[V]: CanTraverseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V]]  = {
    new CanTraverseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V]] {
      def apply[A](from: DenseMatrix[V], axis: Axis._0.type)(f: (DenseVector[V]) => A) {
        cforRange(0 until from.cols) { c =>
          f(from(::, c))
        }
      }
    }
  }

  /**
   * iterates over each column
   * @tparam V
   * @return
   */
  implicit def canTraverseRows[V]: CanTraverseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V]]  = {
    new CanTraverseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V]] {
      def apply[A](from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => A) {
        val t = from.t
        cforRange(0 until from.rows) { r =>
          f(t(::, r))
        }
      }
    }
  }

  /**
   * Iterates over each columns
   * @return
   */
  implicit def canIterateCols[V]: CanIterateAxis[DenseMatrix[V], Axis._0.type, DenseVector[V]]  = {
    new CanIterateAxis[DenseMatrix[V], Axis._0.type, DenseVector[V]] {

      override def apply[A](from: DenseMatrix[V], axis: _0.type): Iterator[DenseVector[V]] = {
        (0 until from.cols).iterator.map(from(::, _))
      }
    }
  }

  /**
   * iterates over each column
   * @tparam V
   * @return
   */
  implicit def canIterateRows[V]: CanIterateAxis[DenseMatrix[V], Axis._1.type, DenseVector[V]]  = {
    new CanIterateAxis[DenseMatrix[V], Axis._1.type, DenseVector[V]] {

      override def apply[A](from: DenseMatrix[V], axis: _1.type): Iterator[DenseVector[V]] = {
        (0 until from.rows).iterator.map(from(_, ::).t)
      }

    }
  }


  //  implicit val setMM_D: OpSet] = new SetDMDMOp[Double.InPlaceImpl2[DenseMatrix[Double], DenseMatrix[Double]]
  //  implicit val setMM_F: OpSet]  = new SetDMDMOp[Float.InPlaceImpl2[DenseMatrix[Float], DenseMatrix[Float]]
  //  implicit val setMM_I: OpSet]  = new SetDMDMOp[Int.InPlaceImpl2[DenseMatrix[Int], DenseMatrix[Int]]

  implicit val setMV_D: OpSet.InPlaceImpl2[DenseMatrix[Double], DenseVector[Double]] = new SetDMDVOp[Double]();
  implicit val setMV_F: OpSet.InPlaceImpl2[DenseMatrix[Float], DenseVector[Float]] = new SetDMDVOp[Float]();
  implicit val setMV_I: OpSet.InPlaceImpl2[DenseMatrix[Int], DenseVector[Int]] = new SetDMDVOp[Int]();

  // There's a bizarre error specializing float's here.
  class CanZipMapValuesDenseMatrix[@spec(Double, Int, Float, Long) V, @specialized(Int, Double) RV: ClassTag]
    extends CanZipMapValues[DenseMatrix[V], V, RV, DenseMatrix[RV]] {

    def create(rows: Int, cols: Int) = DenseMatrix.create(rows, cols, new Array[RV](rows * cols), 0, rows)

    /**Maps all corresponding values from the two collection. */
    def map(from: DenseMatrix[V], from2: DenseMatrix[V], fn: (V, V) => RV) = {
      require(from.rows == from2.rows, "Vector row dimensions must match!")
      require(from.cols == from2.cols, "Vector col dimensions must match!")
      val result = create(from.rows, from.cols)
      var i = 0
      while (i < from.rows) {
        var j = 0
        while (j < from.cols) {
          result(i, j) = fn(from(i, j), from2(i, j))
          j += 1
        }
        i += 1
      }
      result
    }
  }

  implicit def zipMap[V, R: ClassTag]: CanZipMapValuesDenseMatrix[V, R] = new CanZipMapValuesDenseMatrix[V, R]
  implicit val zipMap_d: CanZipMapValuesDenseMatrix[Double, Double] = new CanZipMapValuesDenseMatrix[Double, Double]
  implicit val zipMap_f: CanZipMapValuesDenseMatrix[Float, Float] = new CanZipMapValuesDenseMatrix[Float, Float]
  implicit val zipMap_i: CanZipMapValuesDenseMatrix[Int, Int] = new CanZipMapValuesDenseMatrix[Int, Int]

  class CanZipMapKeyValuesDenseMatrix[@spec(Double, Int, Float, Long) V, @specialized(Int, Double) RV: ClassTag]
    extends CanZipMapKeyValues[DenseMatrix[V], (Int, Int), V, RV, DenseMatrix[RV]] {

    def create(rows: Int, cols: Int) = DenseMatrix.create(rows, cols, new Array[RV](rows * cols), 0, rows)


    override def mapActive(from: DenseMatrix[V], from2: DenseMatrix[V], fn: ((Int, Int), V, V) => RV): DenseMatrix[RV] = {
      map(from, from2, fn)
    }

    /**Maps all corresponding values from the two collection. */
    def map(from: DenseMatrix[V], from2: DenseMatrix[V], fn: ((Int, Int), V, V) => RV) = {
      require(from.rows == from2.rows, "Vector row dimensions must match!")
      require(from.cols == from2.cols, "Vector col dimensions must match!")
      val result = create(from.rows, from.cols)
      var i = 0
      while (i < from.rows) {
        var j = 0
        while (j < from.cols) {
          result(i, j) = fn((i, j), from(i, j), from2(i, j))
          j += 1
        }
        i += 1
      }
      result
    }
  }

  implicit def zipMapKV[V, R: ClassTag]: CanZipMapKeyValuesDenseMatrix[V, R] = new CanZipMapKeyValuesDenseMatrix[V, R]

  implicit def canGaxpy[V: Semiring]: scaleAdd.InPlaceImpl3[DenseMatrix[V], V, DenseMatrix[V]] = {
    new scaleAdd.InPlaceImpl3[DenseMatrix[V], V, DenseMatrix[V]] {
      val ring = implicitly[Semiring[V]]
      def apply(a: DenseMatrix[V], s: V, b: DenseMatrix[V]) {
        require(a.rows == b.rows, "Vector row dimensions must match!")
        require(a.cols == b.cols, "Vector col dimensions must match!")

        var i = 0
        while (i < a.rows) {
          var j = 0
          while (j < a.cols) {
            a(i, j) = ring.+(a(i, j), ring.*(s, b(i, j)))
            j += 1
          }
          i += 1
        }
      }
    }
  }

  implicit def canDim[E]: dim.Impl[DenseMatrix[E], (Int, Int)] = new dim.Impl[DenseMatrix[E],(Int,Int)] {
    def apply(v: DenseMatrix[E]): (Int, Int) = (v.rows,v.cols)
  }

  object FrobeniusInnerProductDenseMatrixSpace {

    implicit def space[S:Field:Zero:ClassTag]: MutableFiniteCoordinateField[DenseMatrix[S], (Int, Int), S] = {
      val norms = EntrywiseMatrixNorms.make[DenseMatrix[S],S]
      import norms._
      MutableFiniteCoordinateField.make[DenseMatrix[S],(Int,Int),S]
    }
  }

  @noinline
  private def init() = {}


}
