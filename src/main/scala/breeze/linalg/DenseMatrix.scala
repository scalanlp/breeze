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
import operators._
import com.github.fommil.netlib.BLAS.{getInstance => blas}
import com.github.fommil.netlib.LAPACK.{getInstance => lapack}
import breeze.util.ArrayUtil
import support._
import breeze.generic._
import breeze.math.{Complex, Ring, Semiring}
import breeze.storage.DefaultArrayValue
import breeze.storage.DefaultArrayValue._
import scala.reflect.ClassTag
import org.netlib.util.intW
import breeze.macros.expand
import scala.math.BigInt
import scala.collection.mutable.ArrayBuffer
import CanTraverseValues.ValuesVisitor

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
 * @param majorStride distance separating columns (or rows, for isTranspose). should be >= rows (or cols, for isTranspose)
 * @param isTranspose if true, then the matrix is considered to be "transposed" (that is, row major)
 */
@SerialVersionUID(1L)
final class DenseMatrix[@specialized(Int, Float, Double) V](val rows: Int,
                                                            val cols: Int,
                                                            val data: Array[V],
                                                            val offset: Int,
                                                            val majorStride: Int,
                                                            val isTranspose: Boolean = false)
  extends Matrix[V] with MatrixLike[V, DenseMatrix[V]] with Serializable {
  /** Creates a matrix with the specified data array, rows, and columns. */
  def this(rows: Int, cols: Int)(implicit man: ClassTag[V]) = this(rows, cols, new Array[V](rows * cols), 0, rows)
  /** Creates a matrix with the specified data array, rows, and columns. Data must be column major */
  def this(rows: Int, cols: Int, data: Array[V], offset: Int = 0) = this(rows, cols, data, offset, rows)
  /** Creates a matrix with the specified data array and rows. columns inferred automatically */
  def this(rows: Int, data: Array[V], offset: Int = 0) = this(rows, {assert(data.length % rows == 0); data.length/rows}, data, offset)

  def apply(row: Int, col: Int) = {
    if(row < 0 || row >= rows) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
    if(col < 0 || col >= cols) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
    data(linearIndex(row, col))
  }


  /** Calculates the index into the data array for row and column */
  final def linearIndex(row: Int, col: Int): Int = {
    if(isTranspose)
      offset + col + row * majorStride
    else
      offset + row + col * majorStride
  }

  def update(row: Int, col: Int, v: V) {
    if(row < 0 || row > rows) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
    if(col < 0 || col > cols) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
    data(linearIndex(row, col)) = v
  }

  /** Converts this matrix to a DenseVector (column-major) */
  def toDenseVector: DenseVector[V] = {
    implicit val man = ClassTag[V](data.getClass.getComponentType.asInstanceOf[Class[V]])
    val ret = DenseVector(new Array[V](rows * cols))
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

  /** Converts this matrix to a DenseVector (column-major)
    * If view = true (or View.Require), throws an exception if we cannot return a view. otherwise returns a view.
    * If view == false (or View.Copy) returns a copy
    * If view == View.Prefer (the default), returns a view if possible, otherwise returns a copy.
    *
    * Views are only possible (if(isTranspose) majorStride == cols else majorStride == rows) == true
    */
  def flatten(view: View=View.Prefer):DenseVector[V] = view match {
    case View.Require =>
      if(!canFlattenView)
        throw new UnsupportedOperationException("Cannot make a view of this matrix.")
      else
        new DenseVector(data, offset, 1, rows * cols)
    case View.Copy =>
      toDenseVector
    case View.Prefer =>
      flatten(canFlattenView)
  }


  private def canFlattenView = if(isTranspose) majorStride == cols else majorStride == rows
  private def canReshapeView = if(isTranspose) majorStride == cols else majorStride == rows

  /** Reshapes this matrix to have the given number of rows and columns
    * If view = true (or View.Require), throws an exception if we cannot return a view. otherwise returns a view.
    * If view == false (or View.Copy) returns a copy
    * If view == View.Prefer (the default), returns a view if possible, otherwise returns a copy.
    *
    * Views are only possible (if(isTranspose) majorStride == cols else majorStride == rows) == true
    *
    * rows * cols must equal size, or cols < 0 && (size / rows * rows == size)
    * @param rows the number of rows
    * @param cols the number of columns, or -1 to auto determine based on size and rows
    */
  def reshape(rows: Int, cols: Int, view: View=View.Prefer):DenseMatrix[V] = {
    require(rows > 0)
    val _cols = cols//if(cols < 0) size / rows else cols
    require(rows * _cols == size, "Cannot reshape a (%d,%d) matrix to a (%d,%d) matrix!".format(this.rows, this.cols, rows, _cols))

    view match {
      case View.Require =>
        if(!canFlattenView)
          throw new UnsupportedOperationException("Cannot make a view of this matrix.")
        else
          new DenseMatrix(rows, _cols, data, offset, if(isTranspose) cols else rows, isTranspose)
      case View.Copy =>
        // calling copy directly gives a verify error. TODO: submit bug
        val result = new DenseMatrix[V](this.rows, this.cols, ArrayUtil.newArrayLike(data, size))
        result := this
        result.reshape(rows, _cols, View.Require)
      case View.Prefer =>
        reshape(rows, cols, canReshapeView)
    }
  }

  def repr = this

  def activeIterator = iterator

  def activeValuesIterator = valuesIterator

  def activeKeysIterator = keysIterator

  /** Computes the sum along the diagonal. */
  def trace(implicit numeric: Numeric[V]) = diag(this:DenseMatrix[V]).sum

  override def equals(p1: Any) = p1 match {
    case x: DenseMatrix[_] =>
      // todo: make this faster in obvious cases
      rows == x.rows && cols == x.cols && (valuesIterator sameElements x.valuesIterator )

    case _ => false
  }

  def activeSize = data.length

  def valueAt(i: Int) = data(i)

  def indexAt(i: Int) = i

  def isActive(i: Int) = true
  def allVisitableIndicesActive = true


  override def toDenseMatrix(implicit cm: ClassTag[V], dfv: DefaultArrayValue[V]): DenseMatrix[V] = {
    val result = new DenseMatrix[V](rows, cols, new Array[V](size))
    result := this
    result
  }

  def copy: DenseMatrix[V] = {
    implicit val man = ClassTag[V](data.getClass.getComponentType.asInstanceOf[Class[V]])
    val result = new DenseMatrix[V](rows, cols, new Array[V](size))
    result := this
    result
  }

  private implicit def dontNeedDefaultArrayValue[V]: DefaultArrayValue[V] = null.asInstanceOf[DefaultArrayValue[V]]


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

  def delete(rows: Seq[Int], axis: Axis._0.type):DenseMatrix[V] = {
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


  def delete(cols: Seq[Int], axis: Axis._1.type):DenseMatrix[V] = {
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

}

object DenseMatrix extends LowPriorityDenseMatrix
with DenseMatrixOps
with DenseMatrixMultOps
with DenseMatrixMultiplyStuff
with MatrixConstructors[DenseMatrix] {

  /**
   * The standard way to create an empty matrix, size is rows * cols
   */
  def zeros[@specialized(Int, Float, Double) V:ClassTag:DefaultArrayValue](rows: Int, cols: Int): DenseMatrix[V] = {
    val data = new Array[V](rows * cols)
    if(implicitly[DefaultArrayValue[V]] != null && rows * cols != 0 && data(0) != implicitly[DefaultArrayValue[V]].value)
      ArrayUtil.fill(data, 0, data.length, implicitly[DefaultArrayValue[V]].value)
    new DenseMatrix(rows, cols, data)
  }

  def create[@specialized(Int, Float, Double) V:DefaultArrayValue](rows: Int, cols: Int, data: Array[V]): DenseMatrix[V] = {
    new DenseMatrix(rows, cols, data)
  }

  /**
   * Creates a square diagonal array of size dim x dim, with 1's along the diagonal.
   */
  def eye[@specialized(Int, Float, Double) V: ClassTag:DefaultArrayValue:Semiring](dim: Int) = {
    val r = zeros[V](dim, dim)
    breeze.linalg.diag.diagDMDVImpl.apply(r) := implicitly[Semiring[V]].one
    r
  }

  /** Horizontally tiles some matrices. They must have the same number of rows */
  def horzcat[M,V](matrices: M*)(implicit ev: M <:< Matrix[V], opset: OpSet.InPlaceImpl2[DenseMatrix[V], M], vman: ClassTag[V], dav: DefaultArrayValue[V]) = {
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
  def vertcat[V](matrices: DenseMatrix[V]*)(implicit opset: OpSet.InPlaceImpl2[DenseMatrix[V], DenseMatrix[V]], vman: ClassTag[V], dav: DefaultArrayValue[V]) = {
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


  // slices
  implicit def canSliceRow[V]: CanSlice2[DenseMatrix[V], Int, ::.type, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], Int, ::.type, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], row: Int, ignored: ::.type) = {
        if(row < 0 || row >= m.rows) throw new ArrayIndexOutOfBoundsException("Row must be in bounds for slice!")
        if(!m.isTranspose)
          new DenseMatrix(1, m.cols, m.data, m.offset + row, m.majorStride)
        else
          new DenseMatrix(1, m.cols, m.data, m.offset + row * m.cols, 1)
      }
    }
  }

  implicit def canSliceCol[V]: CanSlice2[DenseMatrix[V], ::.type, Int, DenseVector[V]] = {
    new CanSlice2[DenseMatrix[V], ::.type, Int, DenseVector[V]] {
      def apply(m: DenseMatrix[V], ignored: ::.type, col: Int) = {
        if(col < 0 || col >= m.cols) throw new ArrayIndexOutOfBoundsException("Column must be in bounds for slice!")
        if(!m.isTranspose)
          new DenseVector(m.data, length = m.rows, offset = col * m.majorStride + m.offset, stride=1)
        else
          new DenseVector(m.data, length=m.rows, offset = m.offset + col, stride = m.majorStride)
      }
    }
  }

  implicit def canSliceRows[V]: CanSlice2[DenseMatrix[V], Range, ::.type, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], Range, ::.type, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], rows: Range, ignored: ::.type) = {
        if(rows.isEmpty) new DenseMatrix(0, m.cols, m.data, 0, 0)
        else if(!m.isTranspose) {
          require(rows.step == 1, "Sorry, we can't support row ranges with step sizes other than 1")
          val first = rows.head
          new DenseMatrix(rows.length, m.cols, m.data, m.offset + first, m.majorStride)
        } else {
          canSliceCols(m.t, ::, rows).t
        }
      }
    }
  }

  implicit def canSliceRowsSuffix[V]: CanSlice2[DenseMatrix[V], RangeSuffix, ::.type, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], RangeSuffix, ::.type, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], rows: RangeSuffix, ignored: ::.type) = {
        canSliceRows(m, rows.start until m.rows, ::)
      }
    }
  }

  implicit def canSliceCols[V]: CanSlice2[DenseMatrix[V], ::.type, Range, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], ::.type, Range, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], ignored: ::.type, cols: Range) = {
        if(cols.isEmpty) new DenseMatrix(m.rows, 0, m.data, 0, 1)
        else if(!m.isTranspose) {
          val first = cols.head
          new DenseMatrix(m.rows, cols.length, m.data, m.offset + first * m.majorStride, m.majorStride * cols.step)
        } else {
          canSliceRows(m.t, cols, ::).t
        }
      }
    }
  }

  implicit def canSliceColsSuffix[V]: CanSlice2[DenseMatrix[V], ::.type, RangeSuffix, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], ::.type, RangeSuffix, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], ignored: ::.type, cols: RangeSuffix) = {
        canSliceCols(m, ::, cols.start until m.cols)
      }
    }
  }

  implicit def canSliceColsAndRows[V]: CanSlice2[DenseMatrix[V], Range, Range, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], Range, Range, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], rows: Range, cols: Range) = {
        if(rows.isEmpty || cols.isEmpty) new DenseMatrix(0, 0, m.data, 0, 1)
        else if(!m.isTranspose) {
          require(rows.step == 1, "Sorry, we can't support row ranges with step sizes other than 1 for non transposed matrices")
          val first = cols.head
          new DenseMatrix(rows.length, cols.length, m.data, m.offset + first * m.rows + rows.head, m.majorStride * cols.step)
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

  implicit def canSlicePartOfRow[V]: CanSlice2[DenseMatrix[V], Int, Range, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], Int, Range, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], row: Int, cols: Range) = {
        if(row < 0  || row > m.rows) throw new IndexOutOfBoundsException("Slice with out of bounds row! " + row)
        if(cols.isEmpty) new DenseMatrix(0, 0, m.data, 0, 1)
        else if(!m.isTranspose) {
          val first = cols.head
          new DenseMatrix(1, cols.length, m.data, m.offset + first * m.rows + row, m.majorStride * cols.step)
        } else {
          require(cols.step == 1, "Sorry, we can't support col ranges with step sizes other than 1 for transposed matrices")
          canSlicePartOfCol(m.t, cols, row).t
        }
      }
    }
  }

  implicit def canSlicePartOfCol[V]: CanSlice2[DenseMatrix[V], Range, Int, DenseVector[V]] = {
    new CanSlice2[DenseMatrix[V], Range, Int, DenseVector[V]] {
      def apply(m: DenseMatrix[V], rows: Range, col: Int) = {
        if(rows.isEmpty) new DenseVector(m.data, 0, 0, 0)
        else if(!m.isTranspose) {
          new DenseVector(m.data, col * m.rows + m.offset + rows.head, rows.step, rows.length)
        } else {
          val m2 = canSlicePartOfRow(m.t, col, rows).t
          m2(::, 0)
        }
      }
    }
  }

  implicit def canMapValues[V, R:ClassTag] = {
    new CanMapValues[DenseMatrix[V],V,R,DenseMatrix[R]] {
      override def map(from : DenseMatrix[V], fn : (V=>R)) = {
        val data = new Array[R](from.size)
        var j = 0
        var off = 0
        while (j < from.cols) {
          var i = 0
          while(i < from.rows) {
            data(off) = fn(from(i, j))
            off += 1
            i += 1
          }
          j += 1
        }
        new DenseMatrix[R](from.rows, from.cols, data)
      }

      override def mapActive(from : DenseMatrix[V], fn : (V=>R)) =
        map(from, fn)
    }
  }
  implicit def handholdCMV[T]= new CanMapValues.HandHold[DenseMatrix[T], T]

  implicit def canIterateValues[V]:CanTraverseValues[DenseMatrix[V], V] = {
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


  implicit def canTransformValues[V]:CanTransformValues[DenseMatrix[V], V, V] = {
    new CanTransformValues[DenseMatrix[V], V, V] {
      def transform(from: DenseMatrix[V], fn: (V) => V) {
        var j = 0
        while (j < from.cols) {
          var i = 0
          while(i < from.rows) {
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
        new DenseMatrix(from.rows, from.cols, data)
      }

      override def mapActive(from : DenseMatrix[V], fn : (((Int,Int),V)=>R)) =
        map(from, fn)
    }
  }

  implicit def canTranspose[V]: CanTranspose[DenseMatrix[V], DenseMatrix[V]] = {
    new CanTranspose[DenseMatrix[V], DenseMatrix[V]] {
      def apply(from: DenseMatrix[V]) = {
        new DenseMatrix(data = from.data, offset = from.offset, cols = from.rows, rows = from.cols, majorStride = from.majorStride, isTranspose = !from.isTranspose)
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
  implicit def canMapRows[V:ClassTag:DefaultArrayValue]: CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], DenseVector[V], DenseMatrix[V]]  = new CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], DenseVector[V], DenseMatrix[V]] {
    def apply(from: DenseMatrix[V], axis: Axis._0.type)(f: (DenseVector[V]) => DenseVector[V]): DenseMatrix[V] = {
      var result:DenseMatrix[V] = null
      for(c <- 0 until from.cols) {
        val col = f(from(::, c))
        if(result eq null) {
          result = DenseMatrix.zeros[V](col.length, from.cols)
        }
        result(::, c) := col
      }

      if(result eq null){
        DenseMatrix.zeros[V](0, from.cols)
      } else {
        result
      }
    }
  }

  implicit def handholdCanMapRows[V]: CanCollapseAxis.HandHold[DenseMatrix[V], Axis._0.type, DenseVector[V]] = new CanCollapseAxis.HandHold[DenseMatrix[V], Axis._0.type, DenseVector[V]]()

  implicit def canMapRowsBitVector[V:ClassTag:DefaultArrayValue]: CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], BitVector, DenseMatrix[Boolean]]  = new CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], BitVector, DenseMatrix[Boolean]] {
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
  implicit def canMapCols[V:ClassTag:DefaultArrayValue] = new CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], DenseVector[V], DenseMatrix[V]] {
    def apply(from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => DenseVector[V]): DenseMatrix[V] = {
      var result:DenseMatrix[V] = null
      import from.{rows, cols}
      val t = from.t
      for(r <- 0 until from.rows) {
        val row = f(t(::, r))
        if(result eq null) {
          // scala has decided this method is overloaded, and needs a result type.
          // It has a result type, and is not overloaded.
//          result = DenseMatrix.zeros[V](from.rows, row.length)
          val data = new Array[V](rows * row.length)
          result = new DenseMatrix(rows, row.length, data)
        }
        result.t apply (::, r) := row
      }

      if(result ne null) {
        result
      } else {
        val data = new Array[V](0)
        result = new DenseMatrix(rows, 0, data)
        result
      }
    }
  }
  implicit def handholdCanMapCols[V]: CanCollapseAxis.HandHold[DenseMatrix[V], Axis._1.type, DenseVector[V]] = new CanCollapseAxis.HandHold[DenseMatrix[V], Axis._1.type, DenseVector[V]]()

  implicit def canMapColsBitVector[V:ClassTag:DefaultArrayValue] = new CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], BitVector, DenseMatrix[Boolean]] {
    def apply(from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => BitVector): DenseMatrix[Boolean] = {
      var result:DenseMatrix[Boolean] = null
      import from.{rows, cols}
      val t = from.t
      for(r <- 0 until from.rows) {
        val row = f(t(::, r))
        if(result eq null) {
          // scala has decided this method is overloaded, and needs a result type.
          // It has a result type, and is not overloaded.
          //          result = DenseMatrix.zeros[V](from.rows, row.length)
          val data = new Array[Boolean](rows * row.length)
          result = new DenseMatrix(rows, row.length, data)
        }
        result.t apply (::, r) := row
      }

      if(result ne null) {
        result
      } else {
        val data = new Array[Boolean](0)
        result = new DenseMatrix(rows, 0, data)
        result
      }
    }
  }


  /**
   * Iterates over each columns
   * @return
   */
  implicit def canIterateCols[V:ClassTag:DefaultArrayValue]: CanIterateAxis[DenseMatrix[V], Axis._0.type, DenseVector[V]]  = new CanIterateAxis[DenseMatrix[V], Axis._0.type, DenseVector[V]] {
    def apply[A](from: DenseMatrix[V], axis: Axis._0.type)(f: (DenseVector[V]) => A) {
      for(c <- 0 until from.cols) {
        f(from(::, c))
      }
    }
  }

  /**
   * iterates over each column
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def canIterateRows[V:ClassTag:DefaultArrayValue] = new CanIterateAxis[DenseMatrix[V], Axis._1.type, DenseVector[V]] {
    def apply[A](from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => A) {
      val t = from.t
      for(r <- 0 until from.rows) {
        f(t(::, r))
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
  class CanZipMapValuesDenseMatrix[@specialized(Int, Double, Float) V, @specialized(Int, Double) RV: ClassTag] extends CanZipMapValues[DenseMatrix[V], V, RV, DenseMatrix[RV]] {
    def create(rows: Int, cols: Int) = new DenseMatrix(rows, cols, new Array[RV](rows * cols))

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

  implicit def canGaxpy[V: Semiring]: CanAxpy[V, DenseMatrix[V], DenseMatrix[V]] = {
    new CanAxpy[V, DenseMatrix[V], DenseMatrix[V]] {
      val ring = implicitly[Semiring[V]]
      def apply(s: V, b: DenseMatrix[V], a: DenseMatrix[V]) {
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

}




