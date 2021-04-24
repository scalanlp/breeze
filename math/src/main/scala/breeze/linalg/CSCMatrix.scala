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

import breeze.generic.UFunc

import java.util
import breeze.linalg.operators._
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.linalg.support._
import breeze.math._
import breeze.storage.Zero
import breeze.util.{ArrayUtil, ReflectionUtil, ScalaVersion, SerializableLogging, Sorting, Terminal}

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.{specialized => spec}
import scalaxy.debug._
import breeze.macros._

/**
 * A compressed sparse column matrix, as used in Matlab and CSparse, etc.
 *
 * In general, you should probably NOT use the class's constructors unless you know what you are doing.
 * We don't validate the input data for performance reasons.
 * So make sure you understand the [[https://en.wikipedia.org/wiki/Sparse_matrix#Compressed_sparse_column_.28CSC_or_CCS.29]] correctly.
 * Otherwise, please use the factory methods under [[CSCMatrix]] and [[CSCMatrix#Builder]] to construct CSC matrices.
 *
 *
 *  @param _data active values
 * @param rows number of rows
 * @param cols number of columns
 * @param colPtrs the locations in `data` that start a column
 * @param _rowIndices row indices of the elements in `data`
 *
 * Most implementations based on "Direct Methods for Sparse Linear Systems"
 * by Timothy A. Davis
 * @author dlwh
 */
// TODO: maybe put columns in own array of sparse vectors, making slicing easier?
class CSCMatrix[@spec(Double, Int, Float, Long) V: Zero](
    private var _data: Array[V],
    val rows: Int,
    val cols: Int,
    val colPtrs: Array[Int], // len cols + 1
    private[linalg] var used: Int,
    private var _rowIndices: Array[Int]) // len >= used
    extends Matrix[V]
    with MatrixLike[V, CSCMatrix[V]]
    with Serializable {

  /**
   * Constructs a [[CSCMatrix]] instance. We don't validate the input data for performance reasons.

   */
  def this(data: Array[V], rows: Int, cols: Int, colPtrs: Array[Int], rowIndices: Array[Int]) =
    this(data, rows, cols, colPtrs, data.length, rowIndices)

  def rowIndices = _rowIndices
  def data = _data

  // don't delete
  CSCMatrix.init()

  def apply(row: Int, col: Int): V = {
    if (row >= rows || col >= cols || row < 0 || col < 0)
      throw new IndexOutOfBoundsException()
    val ind = locate(row, col)
    if (ind < 0) zero
    else data(ind)
  }

  def update(row: Int, col: Int, v: V): Unit = {
    if (row >= rows || col >= cols || row < 0 || col < 0)
      throw new IndexOutOfBoundsException()
    val ind = locate(row, col)
    if (ind >= 0) data(ind) = v
    else if (v != zero) {
      val insertPos = ~ind
      used += 1

      if (used > data.length) {
        // need to grow array
        val newLength = {
          if (data.length == 0) { 4 } else if (data.length < 0x0400) { data.length * 2 } else if (data.length < 0x0800) {
            data.length + 0x0400
          } else if (data.length < 0x1000) { data.length + 0x0800 } else if (data.length < 0x2000) {
            data.length + 0x1000
          } else if (data.length < 0x4000) { data.length + 0x2000 } else { data.length + 0x4000 }
        }

        // allocate new arrays
        val newIndex = util.Arrays.copyOf(rowIndices, newLength)
        val newData = ArrayUtil.copyOf(data, newLength)

        // copy existing data into new arrays
        System.arraycopy(_rowIndices, insertPos, newIndex, insertPos + 1, used - insertPos - 1)
        System.arraycopy(data, insertPos, newData, insertPos + 1, used - insertPos - 1)

        _rowIndices = newIndex
        _data = newData
      } else if (used - insertPos > 1) {
        // need to make room for new element mid-array
        System.arraycopy(_rowIndices, insertPos, _rowIndices, insertPos + 1, used - insertPos - 1)
        System.arraycopy(data, insertPos, data, insertPos + 1, used - insertPos - 1)
      }

      // assign new value
      rowIndices(insertPos) = row
      data(insertPos) = v
      cforRange((col + 1) to cols) { c =>
        colPtrs(c) += 1
      }
    }
  }

  def reserve(nnz: Int): Unit = {
    if (nnz >= used && nnz != rowIndices.length) {
      _rowIndices = util.Arrays.copyOf(rowIndices, nnz)
      _data = ArrayUtil.copyOf(data, nnz)
    }
  }

  def compact(): Unit = {
    reserve(used)
  }

  def activeKeysIterator: Iterator[(Int, Int)] = {
    for (c <- Iterator.range(0, cols); rr <- Iterator.range(colPtrs(c), colPtrs(c + 1))) yield (rowIndices(rr), c)
  }

  def activeIterator: Iterator[((Int, Int), V)] = {
    for (c <- Iterator.range(0, cols); rr <- Iterator.range(colPtrs(c), colPtrs(c + 1)))
      yield (rowIndices(rr), c) -> data(rr)
  }

  def activeValuesIterator: Iterator[V] = data.iterator.take(used)

  def activeSize: Int = used

  def repr: CSCMatrix[V] = this

  private def locate(row: Int, col: Int): Int = {
    val start = colPtrs(col)
    val end = colPtrs(col + 1)
    util.Arrays.binarySearch(rowIndices, start, end, row)
  }

  def zero = implicitly[Zero[V]].zero

  override def toString(maxLines: Int, maxWidth: Int): String = {
    val buf = new StringBuilder()
    buf ++= ("%d x %d CSCMatrix".format(rows, cols))
    activeIterator.take(maxLines - 1).foreach {
      case ((r, c), v) =>
        buf += '\n'
        buf ++= "(%d,%d) ".format(r, c)
        buf ++= v.toString
    }
    buf.toString()
  }

  override def toString: String = toString(maxLines = Terminal.terminalHeight - 3)

  /** just uses the data from this matrix. No copies are made. designed for temporaries */
  private[breeze] def use(matrix: CSCMatrix[V]): Unit = {
    use(matrix.data, matrix.colPtrs, matrix.rowIndices, matrix.used)
  }

  def use(data: Array[V], colPtrs: Array[Int], rowIndices: Array[Int], used: Int): Unit = {
    require(colPtrs.length == this.colPtrs.length)
    require(used >= 0)
    require(data.length >= used)
    require(rowIndices.length >= used)
    this._data = data
    System.arraycopy(colPtrs, 0, this.colPtrs, 0, colPtrs.length)
    this._rowIndices = rowIndices
    this.used = used
  }

  def copy: CSCMatrix[V] = {
    new CSCMatrix[V](ArrayUtil.copyOf(_data, activeSize), rows, cols, colPtrs.clone(), activeSize, _rowIndices.clone)
  }

  def flatten(view: View = View.Copy): SparseVector[V] = {
    view match {
      // This seems kind of silly, since you don't save a ton of time, but for parity with DenseMatrix...
      case View.Require =>
        val indices = new Array[Int](data.length)
        var j = 0
        var ind = 0
        while (j < cols) {
          var ip = colPtrs(j)
          while (ip < colPtrs(j + 1)) {
            val i = rowIndices(ip)
            indices(ind) = i * rows + j
            ip += 1
            ind += 1
          }
          j += 1
        }
        new SparseVector[V](indices, data, activeSize, rows * cols)
      case View.Copy =>
        implicit val man: ClassTag[V] = ReflectionUtil.elemClassTagFromArray(data)
        val sv = SparseVector.zeros[V](rows * cols)
        var j = 0
        while (j < cols) {
          var ip = colPtrs(j)
          while (ip < colPtrs(j + 1)) {
            val i = rowIndices(ip)
            sv(i * cols + j) = data(ip)
            ip += 1
          }
          j += 1
        }
        sv
      case View.Prefer => flatten(View.Require)
    }
  }

  override def toDenseMatrix(implicit cm: ClassTag[V], zero: Zero[V]): DenseMatrix[V] = {
    toDense
  }

  def toDense: DenseMatrix[V] = {
    implicit val ctg: ClassTag[V] = ReflectionUtil.elemClassTagFromArray(data)
    val res = DenseMatrix.zeros[V](rows, cols)
    var i = 0
    while (i < cols) {
      var j = colPtrs(i)
      while (j < colPtrs(i + 1)) {
        res(rowIndices(j), i) = data(j)
        j += 1
      }
      i += 1
    }
    res
  }

  override def equals(p1: Any): Boolean = p1 match {
    case y: CSCMatrix[_] =>
      if (this.rows != y.rows || this.cols != y.cols) {
        return false
      } else {
        val xIter = this.activeIterator
        val yIter = y.activeIterator

        while (xIter.hasNext && yIter.hasNext) {
          var xkeyval = xIter.next()
          var ykeyval = yIter.next()
          while (xkeyval._2 == 0 && xIter.hasNext) xkeyval = xIter.next()
          while (ykeyval._2 == 0 && yIter.hasNext) ykeyval = yIter.next()
          if (xkeyval != ykeyval) return false
        }
        if (xIter.hasNext && !yIter.hasNext) {
          while (xIter.hasNext) if (xIter.next()._2 != 0) return false
        }

        if (!xIter.hasNext && yIter.hasNext) {
          while (yIter.hasNext) if (yIter.next()._2 != 0) return false
        }
      }
      return true
    case y: Matrix[_] =>
      return y == this
    case _ =>
      return false
  }

}

object CSCMatrix extends MatrixConstructors[CSCMatrix] {
  def zeros[@specialized(Int, Float, Double) V: ClassTag: Zero](rows: Int, cols: Int, initialNonzero: Int) = {
    new CSCMatrix[V](new Array(initialNonzero), rows, cols, new Array(cols + 1), 0, new Array(initialNonzero))
  }

  def zeros[@spec(Double, Int, Float, Long) V: ClassTag: Zero](rows: Int, cols: Int): CSCMatrix[V] =
    zeros(rows, cols, 0)

  def create[@spec(Double, Int, Float, Long) V: Zero](rows: Int, cols: Int, data: Array[V]): CSCMatrix[V] = {
    implicit val man: ClassTag[V] = ReflectionUtil.elemClassTagFromArray(data)
    new CSCMatrix(
      data,
      rows,
      cols,
      Array.tabulate(cols + 1)(_ * rows),
      Array.fill(cols)(0 until rows).flatten
    )
  }



  /**
   * This is basically an unsorted coordinate matrix.
   * @param rows if negative, result will automatically infer size
   * @param cols if negative, result will automatically infer size
   * @param initNnz initial number of nonzero entries
   */
  class Builder[@spec(Double, Int, Float, Long) T: ClassTag: Semiring: Zero](
      val rows: Int,
      val cols: Int,
      initNnz: Int = 16) {
    private def ring = implicitly[Semiring[T]]

    def add(r: Int, c: Int, v: T): Unit = {
      if (v != 0) {
        numAdded += 1
        vs += v
        indices += (c.toLong << 32) | (r & 0xFFFFFFFFL)
      }
    }

    // we pack rows and columns into a single long. (most significant bits get the column, so columns are the major axis)
    private val indices = new mutable.ArrayBuilder.ofLong()
    private val vs = breeze.util.ArrayBuilder.make[T]
    private var numAdded = 0
    def activeSize: Int = numAdded

    def sizeHint(nnz: Int): Unit = {
      indices.sizeHint(nnz)
      vs.sizeHint(nnz)
    }


    def result: CSCMatrix[T] = result(false, false)

    private def rowFromIndex(idx: Long) = idx.toInt
    private def colFromIndex(idx: Long) = (idx >>> 32).toInt

    def result(keysAlreadyUnique: Boolean = false, keysAlreadySorted: Boolean = false): CSCMatrix[T] = {
      val indices = this.indices.result()
      val vs = this.vs.result()
      // at most this many nnz
      val nnz = indices.length

      val _rows = if (rows >= 0) rows else indices.map(rowFromIndex).foldLeft(0)(_.max(_)) + 1
      val _cols = if (cols >= 0) cols else indices.map(colFromIndex).foldLeft(0)(_.max(_)) + 1

      val outCols = new Array[Int](_cols + 1)

      if (nnz == 0) {
        return new CSCMatrix(vs, _rows, _cols, outCols, 0, Array())
      }

      // This would reorder indices/vs, but it's OK because sorting
      // is idempotent.
      Sorting.indirectSort(indices, vs, 0, nnz)

      val outRows = new Array[Int](nnz)
      val outData = new Array[T](nnz)

      outRows(0) = rowFromIndex(indices(0))
      outData(0) = vs(0)

      var outDataIndex = 0
      var i = 1
      var lastCol = colFromIndex(indices(0))
      while (i < nnz) {
        val index = indices(i)
        val col = colFromIndex(index)
        require(cols < 0 || col < cols, s"Column index $col is out of bounds for number of columns $cols!")
        val colsEqual = col == lastCol
        val row = rowFromIndex(index)
        require(rows < 0 || row < rows, s"Row index $row is out of bounds for number of rows $rows!")
        if (colsEqual && row == rowFromIndex(indices(i - 1))) {
          assert(!keysAlreadyUnique)
          // TODO: might need to codegen to make this fast.
          outData(outDataIndex) = ring.+(outData(outDataIndex), vs(i))
        } else {
          outDataIndex += 1
          outRows(outDataIndex) = row
          outData(outDataIndex) = vs(i)
        }

        // we need to pad the outCols array with zeros until we reach the next non-zero column
        if (!colsEqual) {
          while (lastCol < col) {
            outCols(lastCol + 1) = outDataIndex
            lastCol += 1
          }
        }

        i += 1
      }
      outDataIndex += 1

      if (keysAlreadyUnique) {
        assert(outDataIndex == nnz)
      }

      while (lastCol < _cols) {
        outCols(lastCol + 1) = outDataIndex
        lastCol += 1
      }

      val out = new CSCMatrix[T](outData, _rows, _cols, outCols, outDataIndex, outRows)
      if (!keysAlreadyUnique)
        out.compact()
      out
    }
  }

  object Builder {
    def fromMatrix[@spec(Double, Int, Float, Long) T: ClassTag: Semiring: Zero](matrix: CSCMatrix[T]): Builder[T] = {
      val bldr = new Builder[T](matrix.rows, matrix.cols, matrix.activeSize)
      var c = 0
      while (c < matrix.cols) {
        var rr = matrix.colPtrs(c)
        val rrlast = matrix.colPtrs(c + 1)
        while (rr < rrlast) {
          val r = matrix.rowIndices(rr)
          bldr.add(r, c, matrix.data(rr))
          rr += 1
        }
        c += 1
      }

      bldr
    }
  }



  implicit def canDim[E]: dim.Impl[CSCMatrix[E], (Int, Int)] = new dim.Impl[CSCMatrix[E], (Int, Int)] {
    def apply(v: CSCMatrix[E]): (Int, Int) = (v.rows, v.cols)
  }

  object FrobeniusInnerProductCSCMatrixSpace {
    implicit def space[S: Field: ClassTag]: MutableFiniteCoordinateField[CSCMatrix[S], (Int, Int), S] = {
      val norms = EntrywiseMatrixNorms.make[CSCMatrix[S], S]
      import norms._
      MutableFiniteCoordinateField.make[CSCMatrix[S], (Int, Int), S]
    }
  }

  @noinline
  private def init() = {}
}
