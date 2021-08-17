package breeze.linalg.operators

import breeze.linalg._
import breeze.linalg.support.{CanCopy, CanSlice, CanSlice2, CanTranspose}
import breeze.math.{Complex, EntrywiseMatrixNorms, Field, MutableFiniteCoordinateField, Semiring}
import breeze.storage.Zero
import breeze.macros.require

import scala.reflect.ClassTag

trait DenseVector_SlicingOps extends TensorLowPrio {

  // slicing
  // specialize to get the good class
  implicit def canSlice_DV_Range_eq_DV[@specialized(Int, Float, Long, Double) V]: CanSlice[DenseVector[V], Range, DenseVector[V]] = {
    new CanSlice[DenseVector[V], Range, DenseVector[V]] {
      def apply(v: DenseVector[V], re: Range): DenseVector[V] = {

        val range: Range = re.getRangeWithoutNegativeIndexes(v.length)

        require(range.isEmpty || range.last < v.length)
        require(range.isEmpty || range.start >= 0)
        DenseVector.create(
          v.data,
          offset = v.offset + v.stride * range.start,
          stride = v.stride * range.step,
          length = range.length)
      }
    }
  }

}

trait DenseMatrix_SlicingOps extends DenseMatrix_SlicingOps_LowPrio with DenseMatrix_TraversalOps {

  implicit def canSliceCol[V]: CanSlice2[DenseMatrix[V], ::.type, Int, DenseVector[V]] = {
    new CanSlice2[DenseMatrix[V], ::.type, Int, DenseVector[V]] {
      def apply(m: DenseMatrix[V], ignored: ::.type, colWNegative: Int) = {

        if (colWNegative < -m.cols || colWNegative >= m.cols)
          throw new ArrayIndexOutOfBoundsException("Column must be in bounds for slice!")
        val col = if (colWNegative < 0) colWNegative + m.cols else colWNegative

        if (!m.isTranspose)
          DenseVector.create(m.data, length = m.rows, offset = col * m.majorStride + m.offset, stride = 1)
        else
          DenseVector.create(m.data, length = m.rows, offset = m.offset + col, stride = m.majorStride)
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

        if (rows.isEmpty) DenseMatrix.create(0, m.cols, m.data, 0, 0)
        else if (!m.isTranspose) {
          require(rows.step == 1, "Sorry, we can't support row ranges with step sizes other than 1")
          val first = rows.head
          require(rows.last < m.rows)
          if (rows.last >= m.rows) {
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

        if (cols.isEmpty) {
          DenseMatrix.create(m.rows, 0, m.data, 0, m.rows)
        } else if (!m.isTranspose) {
          val first = cols.head
          if (cols.last >= m.cols) {
            throw new IndexOutOfBoundsException(s"Col slice of $cols was bigger than matrix cols of ${m.cols}")
          }
          DenseMatrix.create(m.rows, cols.length, m.data, m.offset + first * m.majorStride, m.majorStride * cols.step)
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

        if (rows.isEmpty || cols.isEmpty) DenseMatrix.create(rows.size, cols.size, m.data, 0, 0)
        else if (!m.isTranspose) {
          require(
            rows.step == 1,
            "Sorry, we can't support row ranges with step sizes other than 1 for non transposed matrices")
          val first = cols.head
          if (rows.last >= m.rows) {
            throw new IndexOutOfBoundsException(s"Row slice of $rows was bigger than matrix rows of ${m.rows}")
          }
          if (cols.last >= m.cols) {
            throw new IndexOutOfBoundsException(s"Col slice of $cols was bigger than matrix cols of ${m.cols}")
          }
          DenseMatrix.create(
            rows.length,
            cols.length,
            m.data,
            m.offset + first * m.majorStride + rows.head,
            m.majorStride * cols.step)
        } else {
          require(
            cols.step == 1,
            "Sorry, we can't support col ranges with step sizes other than 1 for transposed matrices")
          canSliceColsAndRows(m.t, cols, rows).t
        }
      }
    }
  }

  implicit def canSlicePartOfCol[V]: CanSlice2[DenseMatrix[V], Range, Int, DenseVector[V]] = {
    new CanSlice2[DenseMatrix[V], Range, Int, DenseVector[V]] {
      def apply(m: DenseMatrix[V], rowsWNegative: Range, colWNegative: Int) = {

        val rows: Range = rowsWNegative.getRangeWithoutNegativeIndexes(m.rows)
        if (colWNegative < -m.cols || colWNegative >= m.cols)
          throw new ArrayIndexOutOfBoundsException("Row must be in bounds for slice!")
        val col = if (colWNegative < 0) colWNegative + m.cols else colWNegative

        if (rows.isEmpty) {
          DenseVector.create(m.data, 0, 0, 0)
        } else if (!m.isTranspose) {
          if (rows.last >= m.rows) {
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

}

trait DenseMatrix_SlicingOps_LowPrio extends LowPriorityDenseMatrix1 with GenericOps with TensorLowPrio {

  implicit def canSliceWeirdRows[V: Semiring: ClassTag]
  : CanSlice2[DenseMatrix[V], Seq[Int], ::.type, SliceMatrix[Int, Int, V]] = {
    new CanSlice2[DenseMatrix[V], Seq[Int], ::.type, SliceMatrix[Int, Int, V]] {
      def apply(from: DenseMatrix[V], slice: Seq[Int], slice2: ::.type): SliceMatrix[Int, Int, V] = {
        new SliceMatrix(from, slice.toIndexedSeq, (0 until from.cols))
      }
    }
  }

  implicit def canSliceWeirdCols[V: Semiring: ClassTag]
  : CanSlice2[DenseMatrix[V], ::.type, Seq[Int], SliceMatrix[Int, Int, V]] = {
    new CanSlice2[DenseMatrix[V], ::.type, Seq[Int], SliceMatrix[Int, Int, V]] {
      def apply(from: DenseMatrix[V], slice2: ::.type, slice: Seq[Int]): SliceMatrix[Int, Int, V] = {
        new SliceMatrix(from, (0 until from.rows), slice.toIndexedSeq)
      }
    }
  }

  implicit def canSliceTensorBooleanRows[V: Semiring: ClassTag]
  : CanSlice2[DenseMatrix[V], Tensor[Int, Boolean], ::.type, SliceMatrix[Int, Int, V]] = {
    new CanSlice2[DenseMatrix[V], Tensor[Int, Boolean], ::.type, SliceMatrix[Int, Int, V]] {
      def apply(from: DenseMatrix[V], rows: Tensor[Int, Boolean], cols: ::.type): SliceMatrix[Int, Int, V] = {
        new SliceMatrix(from, rows.findAll(_ == true), (0 until from.cols))
      }
    }
  }

  implicit def canSliceTensorBooleanCols[V: Semiring: ClassTag]
  : CanSlice2[DenseMatrix[V], ::.type, Tensor[Int, Boolean], SliceMatrix[Int, Int, V]] = {
    new CanSlice2[DenseMatrix[V], ::.type, Tensor[Int, Boolean], SliceMatrix[Int, Int, V]] {
      def apply(from: DenseMatrix[V], rows: ::.type, cols: Tensor[Int, Boolean]): SliceMatrix[Int, Int, V] = {
        new SliceMatrix(from, (0 until from.rows), cols.findAll(_ == true))
      }
    }
  }

}

