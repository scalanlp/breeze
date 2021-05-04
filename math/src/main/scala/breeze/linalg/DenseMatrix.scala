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
import breeze.util.{ArrayUtil, ReflectionUtil}
import breeze.macros._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.{specialized => spec}
import scalaxy.debug._

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
final class DenseMatrix[@spec(Double, Int, Float, Long) V](
    val rows: Int,
    val cols: Int,
    val data: Array[V],
    val offset: Int,
    val majorStride: Int,
    val isTranspose: Boolean = false)
    extends Matrix[V]
    with MatrixLike[V, DenseMatrix[V]]
    with Serializable {

  /** Creates a matrix with the specified data array, rows, and columns. */
  def this(rows: Int, cols: Int)(implicit man: ClassTag[V]) = this(rows, cols, new Array[V](rows * cols), 0, rows)

  /** Creates a matrix with the specified data array, rows, and columns. Data must be column major */
  def this(rows: Int, cols: Int, data: Array[V], offset: Int) = this(rows, cols, data, offset, rows)

  /** Creates a matrix with the specified data array, rows, and columns. Data must be column major */
  def this(rows: Int, cols: Int, data: Array[V]) = this(rows, cols, data, 0, rows)

  /** Creates a matrix with the specified data array and rows. columns inferred automatically */
  def this(rows: Int, data: Array[V], offset: Int) =
    this(rows, { assert(data.length % rows == 0); data.length / rows }, data, offset)

  if (isTranspose && (math.abs(majorStride) < cols) && majorStride != 0) {
    throw new IndexOutOfBoundsException(
      "MajorStride == " + majorStride + " is smaller than cols == " + cols + ", which is impossible")
  }
  if (!isTranspose && (math.abs(majorStride) < rows) && majorStride != 0) {
    throw new IndexOutOfBoundsException(
      "MajorStride == " + majorStride + " is smaller than rows == " + rows + ", which is impossible")
  }
  if (rows < 0) { throw new IndexOutOfBoundsException("Rows must be larger than zero. It was " + rows) }
  if (cols < 0) { throw new IndexOutOfBoundsException("Cols must be larger than zero. It was " + cols) }
  if (offset < 0) { throw new IndexOutOfBoundsException("Offset must be larger than zero. It was " + offset) }
  if (majorStride > 0) {
    if (data.length < linearIndex(rows - 1, cols - 1)) {
      throw new IndexOutOfBoundsException(
        "Storage array has size " + data.size + " but indices can grow as large as " + linearIndex(rows - 1, cols - 1))
    }
  } else if (majorStride < 0) {
    if (data.length < linearIndex(rows - 1, 0)) {
      throw new IndexOutOfBoundsException(
        "Storage array has size " + data.size + " but indices can grow as large as " + linearIndex(rows - 1, cols - 1))
    }
    if (linearIndex(0, cols - 1) < 0) {
      throw new IndexOutOfBoundsException(
        "Storage array has negative stride " + majorStride + " and offset " + offset + " which can result in negative indices.")
    }
  }

  def apply(row: Int, col: Int) = {
    if (row < -rows || row >= rows)
      throw new IndexOutOfBoundsException(
        s"${(row, col)} not in [-$rows,$rows) x [-$cols,$cols)")
    if (col < -cols || col >= cols)
      throw new IndexOutOfBoundsException(
        s"${(row, col)} not in [-$rows,$rows) x [-$cols,$cols)")
    val trueRow = if (row < 0) row + rows else row
    val trueCol = if (col < 0) col + cols else col
    data(linearIndex(trueRow, trueCol))
  }

  // don't delete
  DenseMatrix.init()

  /** Calculates the index into the data array for row and column */
  def linearIndex(row: Int, col: Int): Int = {
    if (isTranspose)
      offset + col + row * majorStride
    else
      offset + row + col * majorStride
  }

  def rowColumnFromLinearIndex(index: Int): (Int, Int) = {
    val r = (index - offset) % majorStride
    val c = (index - offset) / majorStride
    if (isTranspose) {
      (c, r)
    } else {
      (r, c)
    }
  }

  def update(row: Int, col: Int, v: V): Unit = {
    if (row < -rows || row >= rows)
      throw new IndexOutOfBoundsException(
        s"${(row, col)} not in [-$rows,$rows) x [-$cols,$cols)")
    if (col < -cols || col >= cols)
      throw new IndexOutOfBoundsException(
        s"${(row, col)} not in [-$rows,$rows) x [-$cols,$cols)")
    val trueRow = if (row < 0) row + rows else row
    val trueCol = if (col < 0) col + cols else col
    data(linearIndex(trueRow, trueCol)) = v
  }

  @deprecated("This isn't actually any faster according to benchmarks", "0.12-SNAPSHOT")
  def unsafeUpdate(row: Int, col: Int, v: V): Unit = { data(linearIndex(row, col)) = v }

  // <editor-fold defaultstate="collapsed" desc=" conversions (toArray, toDenseVector) ">

  /** Converts this matrix to a flat Array (column-major) */
  def toArray: Array[V] = {
    implicit val man: ClassTag[V] = ReflectionUtil.elemClassTagFromArray(data)
    if (isContiguous && !isTranspose) {
      ArrayUtil.copyOfRange(data, offset, offset + size)
    } else {
      // TODO: consider using cache oblivious transpose here?
      val ret = new Array[V](rows * cols)
      cforRange2(0 until cols, 0 until rows) { (i,j) =>
        ret(i * rows + j) = data(linearIndex(j, i))
      }
      ret
    }
  }

  /** Converts this matrix to a DenseVector (column-major) */
  def toDenseVector: DenseVector[V] = DenseVector(toArray)

  // </editor-fold>

  /** Converts this matrix to a DenseVector (column-major)
   * If view = true (or View.Require), throws an exception if we cannot return a view. otherwise returns a view.
   * If view == false (or View.Copy) returns a copy
   * If view == View.Prefer (the default), returns a view if possible, otherwise returns a copy.
   *
   * Views are only possible (if(isTranspose) majorStride == cols else majorStride == rows) == true
   */
  def flatten(view: View = View.Prefer): DenseVector[V] = view match {
    case View.Require =>
      if (!canFlattenView)
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
  def reshape(rows: Int, cols: Int, view: View = View.Prefer): DenseMatrix[V] = {
    val _cols = cols //if(cols < 0) size / rows else cols
    require(
      rows * _cols == size,
      "Cannot reshape a (%d,%d) matrix to a (%d,%d) matrix!".format(this.rows, this.cols, rows, _cols))

    view match {
      case View.Require =>
        if (!canReshapeView)
          throw new UnsupportedOperationException("Cannot make a view of this matrix.")
        else
          new DenseMatrix(rows, _cols, data, offset, if (isTranspose) cols else rows, isTranspose)
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

  def activeSize = data.length

  def valueAt(i: Int): V = data(i)
  def valueAt(row: Int, col: Int): V = apply(row, col)

  def indexAt(i: Int) = i

  def isActive(i: Int) = true
  def allVisitableIndicesActive = true

  override def toDenseMatrix(implicit cm: ClassTag[V], zero: Zero[V]): DenseMatrix[V] = {
    val result = DenseMatrix.create[V](rows, cols, new Array[V](size))
    result := this
    result
  }

  def copy: DenseMatrix[V] = {
    implicit val man: ClassTag[V] = ReflectionUtil.elemClassTagFromArray(data)
    val result = DenseMatrix.create[V](rows, cols, new Array[V](size))
    result := this
    result
  }

  private implicit def dontNeedZero[V]: Zero[V] = null.asInstanceOf[Zero[V]]

  def delete(row: Int, axis: Axis._0.type): DenseMatrix[V] = {
    implicit val man: ClassTag[V] = ReflectionUtil.elemClassTagFromArray(data)
    require(row >= 0 && row < rows, s"row $row is not in bounds: [0, $rows)")
    if (row == 0) this(1 until rows, ::).copy
    else if (row == rows - 1) this(0 until rows - 1, ::).copy
    else DenseMatrix.vertcat(this(0 until row, ::), this((row + 1) until rows, ::))
  }

  def delete(col: Int, axis: Axis._1.type): DenseMatrix[V] = {
    implicit val man: ClassTag[V] = ReflectionUtil.elemClassTagFromArray(data)
    require(col >= 0 && col < cols, s"col $col is not in bounds: [0, $cols)")
    if (col == 0) this(::, 1 until cols).copy
    else if (col == cols - 1) this(::, 0 until cols - 1).copy
    else DenseMatrix.horzcat(this(::, 0 until col), this(::, (col + 1) until cols))
  }

  def delete(rows: Seq[Int], axis: Axis._0.type): DenseMatrix[V] = {
    implicit val man: ClassTag[V] = ReflectionUtil.elemClassTagFromArray(data)
    if (rows.isEmpty) copy
    else if (rows.size == 1) delete(rows(0), axis)
    else {
      val sorted = rows.sorted
      require(sorted.head >= 0 && sorted.last < this.rows, s"row $rows are not in bounds: [0, ${this.rows})")
      var last = 0
      val matrices = breeze.collection.compat.arraySeqBuilder[DenseMatrix[V]]
      for (index <- sorted) {
        assert(index >= last)
        if (index != last) {
          matrices += this(last until index, ::)
        }
        last = index + 1
      }
      if (last != this.rows) {
        matrices += this(last until this.rows, ::)
      }
      DenseMatrix.vertcat(matrices.result(): _*)
    }
  }

  def delete(cols: Seq[Int], axis: Axis._1.type): DenseMatrix[V] = {
    implicit val man: ClassTag[V] = ReflectionUtil.elemClassTagFromArray(data)
    if (cols.isEmpty) this.copy
    else if (cols.size == 1) delete(cols(0), axis)
    else {
      val sorted = cols.sorted
      require(sorted.head >= 0 && sorted.last < this.cols, s"col $cols are not in bounds: [0, ${this.cols})")
      var last = 0
      val matrices = breeze.collection.compat.arraySeqBuilder[DenseMatrix[V]]
      for (index <- sorted) {
        assert(index >= last)
        if (index != last) {
          matrices += this(::, last until index)
        }
        last = index + 1
      }
      if (last != this.cols) {
        matrices += this(::, last until this.cols)
      }
      DenseMatrix.horzcat(matrices.result(): _*)
    }
  }

  private[linalg] def majorSize = if (isTranspose) rows else cols
  private[linalg] def minorSize = if (isTranspose) cols else rows
  private def footprint = majorSize * majorStride

  /** Returns true if this dense matrix takes up a contiguous segment of the array */
  def isContiguous: Boolean = majorSize == majorStride

  /** Returns true if this dense matrix overlaps any content with the other matrix */
  private[linalg] def overlaps(other: DenseMatrix[V]): Boolean = (this.data eq other.data) && {
    val astart = offset
    val aend = offset + footprint
    val bstart = other.offset
    val bend = other.offset + other.footprint
    Range(astart, aend).contains(bstart) ||
    Range(astart, aend).contains(bend) ||
    Range(bstart, bend).contains(astart) ||
    Range(bstart, bend).contains(aend)
  }

  private def checkIsSpecialized(): Unit = {
    if (data.isInstanceOf[Array[Double]] && getClass.getName() == "breeze.linalg.DenseMatrix")
      throw new Exception("...")
  }
  // uncomment to debug places where specialization fails
//  checkIsSpecialized()

}

object DenseMatrix extends MatrixConstructors[DenseMatrix] {

  /**
   * The standard way to create an empty matrix, size is rows * cols
   */
  def zeros[@spec(Double, Int, Float, Long) V: ClassTag: Zero](rows: Int, cols: Int): DenseMatrix[V] = {
    val data = new Array[V](rows * cols)
    if (implicitly[Zero[V]] != null && rows * cols != 0 && data(0) != implicitly[Zero[V]].zero)
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
  def create[@spec(Double, Int, Float, Long) V: Zero](rows: Int, cols: Int, data: Array[V]): DenseMatrix[V] = {
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
  def create[@spec(Double, Int, Float, Long) V](
      rows: Int,
      cols: Int,
      data: Array[V],
      offset: Int,
      majorStride: Int,
      isTranspose: Boolean = false): DenseMatrix[V] = {
    (data: Any) match {
      case d: Array[Double] =>
        new DenseMatrix(rows, cols, d, offset, majorStride, isTranspose).asInstanceOf[DenseMatrix[V]]
      case d: Array[Float] =>
        new DenseMatrix(rows, cols, d, offset, majorStride, isTranspose).asInstanceOf[DenseMatrix[V]]
      case d: Array[Long] =>
        new DenseMatrix(rows, cols, d, offset, majorStride, isTranspose).asInstanceOf[DenseMatrix[V]]
      case d: Array[Int] =>
        new DenseMatrix(rows, cols, d, offset, majorStride, isTranspose).asInstanceOf[DenseMatrix[V]]
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
  override def ones[@specialized(Int, Float, Double, Long) V: ClassTag: Zero: Semiring](
      rows: Int,
      cols: Int): DenseMatrix[V] = {
    val data = new Array[V](rows * cols)
    if (rows * cols != 0 && data(0) != implicitly[Semiring[V]].one)
      ArrayUtil.fill(data, 0, data.length, implicitly[Semiring[V]].one)
    DenseMatrix.create(rows, cols, data)
  }

  /**
   * Creates a square diagonal array of size dim x dim, with 1's along the diagonal.
   */
  def eye[@spec(Double, Int, Float, Long) V: ClassTag: Zero: Semiring](dim: Int): DenseMatrix[V] = {
    val r = zeros[V](dim, dim)
    breeze.linalg.diag.diagDMDVImpl.apply(r) := implicitly[Semiring[V]].one
    r
  }

  /** Horizontally tiles some matrices. They must have the same number of rows */
  def horzcat[M, V](matrices: M*)(
      implicit ev: M <:< Matrix[V],
      opset: OpSet.InPlaceImpl2[DenseMatrix[V], M],
      vman: ClassTag[V],
      zero: Zero[V]) = {
    if (matrices.isEmpty) zeros[V](0, 0)
    else {
      require(matrices.forall(m => m.rows == matrices(0).rows), "Not all matrices have the same number of rows")
      val numCols = matrices.foldLeft(0)(_ + _.cols)
      val numRows = matrices(0).rows
      val res = DenseMatrix.zeros[V](numRows, numCols)
      var offset = 0
      for (m <- matrices) {
        res(0 until numRows, (offset) until (offset + m.cols)) := m
        offset += m.cols
      }
      res
    }
  }

  /** Vertically tiles some matrices. They must have the same number of columns */
  def vertcat[V](matrices: DenseMatrix[V]*)(
      implicit opset: OpSet.InPlaceImpl2[DenseMatrix[V], DenseMatrix[V]],
      vman: ClassTag[V],
      zero: Zero[V]) = {
    if (matrices.isEmpty) zeros[V](0, 0)
    else {
      require(matrices.forall(m => m.cols == matrices(0).cols), "Not all matrices have the same number of columns")
      val numRows = matrices.foldLeft(0)(_ + _.rows)
      val numCols = matrices(0).cols
      val res = DenseMatrix.zeros[V](numRows, numCols)
      var offset = 0
      for (m <- matrices) {
        res((offset) until (offset + m.rows), 0 until numCols) := m
        offset += m.rows
      }
      res
    }
  }

  implicit def scalarOf[T]: ScalarOf[DenseMatrix[T], T] = ScalarOf.dummy

  // zerosLike
  implicit def canCreateZerosLike[V: ClassTag: Zero]: CanCreateZerosLike[DenseMatrix[V], DenseMatrix[V]] =
    new CanCreateZerosLike[DenseMatrix[V], DenseMatrix[V]] {
      def apply(v1: DenseMatrix[V]): DenseMatrix[V] = {
        zeros[V](v1.rows, v1.cols)
      }
    }

  object FrobeniusInnerProductDenseMatrixSpace {
    implicit def space[S: Field: Zero: ClassTag]: MutableFiniteCoordinateField[DenseMatrix[S], (Int, Int), S] = {
      val norms = EntrywiseMatrixNorms.make[DenseMatrix[S], S]
      import norms._
      MutableFiniteCoordinateField.make[DenseMatrix[S], (Int, Int), S]
    }
  }

  @noinline
  private def init() = {}
}



