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
  def trace(implicit numeric: Numeric[V]) = diagM(this:DenseMatrix[V]).sum

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

  override def ureduce[A](f: URFunc[V, A]): A = {
    val idealMajorStride = if(isTranspose) cols else rows
    if(majorStride == idealMajorStride && offset == 0) f(data, rows*cols)
    else if(majorStride == idealMajorStride) f(data, offset, 1, rows*cols, {_ => {true}})
    else f(valuesIterator)
  }

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
                           with MatrixConstructors[DenseMatrix]  {
  /**
   * The standard way to create an empty matrix, size is rows * cols
   */
  def zeros[@specialized(Int, Float, Double) V:ClassTag:DefaultArrayValue](rows: Int, cols: Int) = {
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
    breeze.linalg.diag(r) := implicitly[Semiring[V]].one
    r
  }

  /** Horizontally tiles some matrices. They must have the same number of rows */
  def horzcat[M,V](matrices: M*)(implicit ev: M <:< Matrix[V], opset: BinaryUpdateOp[DenseMatrix[V], M, OpSet], vman: ClassTag[V], dav: DefaultArrayValue[V]) = {
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
  def vertcat[V](matrices: DenseMatrix[V]*)(implicit opset: BinaryUpdateOp[DenseMatrix[V], DenseMatrix[V], OpSet], vman: ClassTag[V], dav: DefaultArrayValue[V]) = {
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
        if(rows.isEmpty) new DenseMatrix(0, 0, m.data, 0, 0)
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
        if(cols.isEmpty) new DenseMatrix(0, 0, m.data, 0, 1)
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



  implicit def negFromScale[V](implicit scale: BinaryOp[DenseMatrix[V], V, OpMulScalar, DenseMatrix[V]], field: Ring[V]) = {
    new UnaryOp[DenseMatrix[V], OpNeg, DenseMatrix[V]] {
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

  def binaryOpFromBinaryUpdateOp[V, Other, Op<:OpType](implicit copy: CanCopy[DenseMatrix[V]], op: BinaryUpdateOp[DenseMatrix[V], Other, Op], man: ClassTag[V]) = {
    new BinaryOp[DenseMatrix[V], Other, Op, DenseMatrix[V]] {
      override def apply(a : DenseMatrix[V], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def binaryLeftMulOpFromBinaryRightOp[V, Op<:OpType](implicit op: BinaryOp[DenseMatrix[V], V, OpMulScalar, DenseMatrix[V]]) = {
    new BinaryOp[V, DenseMatrix[V], Op, DenseMatrix[V]] {
      override def apply(a : V, b: DenseMatrix[V]) = {
        op(b, a)
      }
    }
  }

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

  /**
   * transforms each column into a new column, giving a new matrix.
   * @tparam V value type
   * @return
   */
  implicit def canMapCols[V:ClassTag:DefaultArrayValue] = new CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], DenseVector[V], DenseMatrix[V]] {
    def apply(from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => DenseVector[V]): DenseMatrix[V] = {
      var result:DenseMatrix[V] = null
      val t = from.t
      for(r <- 0 until from.rows) {
        val row = f(t(::, r))
        if(result eq null) {
          result = DenseMatrix.zeros[V](from.rows, row.length)
        }
        result.t apply (::, r) := row
      }

      if(result ne null) result
      else DenseMatrix.zeros[V](from.rows, 0)
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


//  implicit val setMM_D: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpSet] = new SetDMDMOp[Double]
//  implicit val setMM_F: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpSet]  = new SetDMDMOp[Float]
//  implicit val setMM_I: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpSet]  = new SetDMDMOp[Int]

  implicit val setMV_D: BinaryUpdateOp[DenseMatrix[Double], DenseVector[Double], OpSet] = new SetDMDVOp[Double]
  implicit val setMV_F: BinaryUpdateOp[DenseMatrix[Float], DenseVector[Float], OpSet]  = new SetDMDVOp[Float]
  implicit val setMV_I: BinaryUpdateOp[DenseMatrix[Int], DenseVector[Int], OpSet]  = new SetDMDVOp[Int]

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

trait LowPriorityDenseMatrix1 {
  protected implicit def dontNeedDefaultArrayValue[V]: DefaultArrayValue[V] = null.asInstanceOf[DefaultArrayValue[V]]
  /**
   * Returns a 1xnumCols DenseMatrix
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def canCollapseRows[V, R:ClassTag:DefaultArrayValue]: CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], R, DenseMatrix[R]]  = new CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], R, DenseMatrix[R]] {
    def apply(from: DenseMatrix[V], axis: Axis._0.type)(f: (DenseVector[V]) => R): DenseMatrix[R] = {
      val result = DenseMatrix.zeros[R](1, from.cols)
      for(c <- 0 until from.cols) {
        result(0, c) = f(from(::, c))
      }
      result
    }
  }

  /**
   * Returns a numRows DenseVector
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def canCollapseCols[V, R:ClassTag:DefaultArrayValue] = new CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], R, DenseVector[R]] {
    def apply(from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => R): DenseVector[R] = {
      val result = DenseVector.zeros[R](from.rows)
      val t = from.t
      for(r <- 0 until from.rows) {
        result(r) = f(t(::, r))
      }
      result
    }
  }


  class SetMMOp[@specialized(Int, Double, Float) V] extends BinaryUpdateOp[DenseMatrix[V], Matrix[V], OpSet] {
    def apply(a: DenseMatrix[V], b: Matrix[V]) {
      require(a.rows == b.rows, "Matrixs must have same number of rows")
      require(a.cols == b.cols, "Matrixs must have same number of columns")

      // slow path when we don't have a trivial matrix
      val ad = a.data
      var c = 0
      while(c < a.cols) {
        var r = 0
        while(r < a.rows) {
          ad(a.linearIndex(r, c)) = b(r, c)
          r += 1
        }
        c += 1
      }
    }
  }



  class SetDMVOp[@specialized(Int, Double, Float) V] extends BinaryUpdateOp[DenseMatrix[V], Vector[V], OpSet] {
    def apply(a: DenseMatrix[V], b: Vector[V]) {
      require(a.rows == b.length && a.cols == 1 || a.cols == b.length && a.rows == 1, "DenseMatrix must have same number of rows, or same number of columns, as DenseVector, and the other dim must be 1.")
      val ad = a.data
      var i = 0
      var c = 0
      while(c < a.cols) {
        var r = 0
        while(r < a.rows) {
          ad(a.linearIndex(r, c)) = b(i)
          r += 1
          i += 1
        }
        c += 1
      }
    }
  }

  implicit def setMM[V]: BinaryUpdateOp[DenseMatrix[V], Matrix[V], OpSet] = new SetMMOp[V]
  implicit def setMV[V]: BinaryUpdateOp[DenseMatrix[V], Vector[V], OpSet] = new SetDMVOp[V]
}

trait LowPriorityDenseMatrix extends LowPriorityDenseMatrix1 {

  implicit def canSliceWeirdRows[V]:CanSlice2[DenseMatrix[V], Seq[Int], ::.type, SliceMatrix[Int, Int, V]] = {
    new CanSlice2[DenseMatrix[V], Seq[Int], ::.type, SliceMatrix[Int, Int, V]] {
      def apply(from: DenseMatrix[V], slice: Seq[Int], slice2: ::.type): SliceMatrix[Int, Int, V] = {
        new SliceMatrix(from, slice.toIndexedSeq, (0 until from.cols))
      }
    }
  }

  class SetDMDMOp[@specialized(Int, Double, Float) V] extends BinaryUpdateOp[DenseMatrix[V], DenseMatrix[V], OpSet] {
    def apply(a: DenseMatrix[V], b: DenseMatrix[V]) {
      require(a.rows == b.rows, "Matrixs must have same number of rows")
      require(a.cols == b.cols, "Matrixs must have same number of columns")
      if(a.data.length - a.offset == a.rows * a.cols
        && b.data.length - b.offset == a.rows * a.cols
        && a.majorStride == b.majorStride
        && a.isTranspose == b.isTranspose) {
        System.arraycopy(b.data, b.offset, a.data, a.offset, a.size)
        return
      }

      // slow path when we don't have a trivial matrix
      val ad = a.data
      val bd = b.data
      var c = 0
      while(c < a.cols) {
        var r = 0
        while(r < a.rows) {
          ad(a.linearIndex(r, c)) = bd(b.linearIndex(r, c))
          r += 1
        }
        c += 1
      }
    }
  }

  class SetDMDVOp[@specialized(Int, Double, Float) V] extends BinaryUpdateOp[DenseMatrix[V], DenseVector[V], OpSet] {
    def apply(a: DenseMatrix[V], b: DenseVector[V]) {
      require(a.rows == b.length && a.cols == 1 || a.cols == b.length && a.rows == 1, "DenseMatrix must have same number of rows, or same number of columns, as DenseVector, and the other dim must be 1.")
      val ad = a.data
      val bd = b.data
      var c = 0
      var boff = b.offset
      while(c < a.cols) {
        var r = 0
        while(r < a.rows) {
          ad(a.linearIndex(r, c)) = bd(boff)
          r += 1
          boff += b.stride
        }
        c += 1
      }
    }
  }


  class SetMSOp[@specialized(Int, Double, Float) V] extends BinaryUpdateOp[DenseMatrix[V], V, OpSet] {
    def apply(a: DenseMatrix[V], b: V) {
      if(a.data.length - a.offset == a.rows * a.cols) {
        ArrayUtil.fill(a.data, a.offset, a.size, b)
        return
      }

      // slow path when we don't have a trivial matrix
      val ad = a.data
      var c = 0
      while(c < a.cols) {
        var r = 0
        while(r < a.rows) {
          ad(a.linearIndex(r, c)) = b
          r += 1
        }
        c += 1
      }
    }
  }

  implicit def setDMDM[V]: BinaryUpdateOp[DenseMatrix[V], DenseMatrix[V], OpSet] = new SetDMDMOp[V]
  implicit def setDMDV[V]: BinaryUpdateOp[DenseMatrix[V], DenseVector[V], OpSet] = new SetDMDVOp[V]
  implicit def setDMS[V]: BinaryUpdateOp[DenseMatrix[V], V, OpSet] = new SetMSOp[V]
}

trait DenseMatrixMultiplyStuff extends DenseMatrixOps with DenseMatrixMultOps { this: DenseMatrix.type =>
  implicit object DenseMatrixDMulDenseMatrixD
  extends BinaryOp[DenseMatrix[Double],DenseMatrix[Double],OpMulMatrix,DenseMatrix[Double]] {
    def apply(a : DenseMatrix[Double], b : DenseMatrix[Double]) = {
      val rv = DenseMatrix.zeros[Double](a.rows, b.cols)
      require(a.cols == b.rows, "Dimension mismatch!")
      blas.dgemm(transposeString(a), transposeString(b),
          rv.rows, rv.cols, a.cols,
          1.0, a.data, a.offset, a.majorStride,
          b.data, b.offset, b.majorStride,
          0.0, rv.data, 0, rv.rows)
      rv
    }
  }


  private def transposeString(a: DenseMatrix[Double]): String = {
    if (a.isTranspose) "T" else "N"
  }

  implicit object DenseMatrixDMulDenseVectorD
  extends BinaryOp[DenseMatrix[Double],DenseVector[Double],OpMulMatrix,DenseVector[Double]] {
    def apply(a : DenseMatrix[Double], b : DenseVector[Double]) = {
      require(a.cols == b.length, "Dimension mismatch!")
      val rv = DenseVector.zeros[Double](a.rows)
      blas.dgemv(transposeString(a),
        if(a.isTranspose) a.cols else a.rows, if(a.isTranspose) a.rows else a.cols,
        1.0, a.data, a.offset, a.majorStride,
             b.data, b.offset, b.stride,
        0.0, rv.data, rv.offset, rv.stride)
      rv
    }
  }

  implicit object DenseMatrixCanSolveDenseMatrix
  extends BinaryOp[DenseMatrix[Double],DenseMatrix[Double],OpSolveMatrixBy,DenseMatrix[Double]] {
    override def apply(A : DenseMatrix[Double], V : DenseMatrix[Double]) = {
      require(A.rows == V.rows, "Non-conformant matrix sizes")

      if (A.rows == A.cols) {
        // square: LUSolve
        val X = DenseMatrix.zeros[Double](V.rows, V.cols)
        X := V
        LUSolve(X,A)
        X
      } else {
        // non-square: QRSolve
        val X = DenseMatrix.zeros[Double](A.cols, V.cols)
        QRSolve(X,A,V)
        X
      }
    }

    /** X := A \ X, for square A */
    def LUSolve(X : DenseMatrix[Double], A : DenseMatrix[Double]) = {
      require(X.offset == 0)
      require(A.offset == 0)
      val piv = new Array[Int](A.rows)
      val newA = A.copy
      assert(!newA.isTranspose)

      val info = {
        val info = new intW(0)
        lapack.dgesv(A.rows, X.cols, newA.data, newA.majorStride, piv, X.data, X.majorStride, info)
        info.`val`
      }

      if (info > 0)
        throw new MatrixSingularException()
      else if (info < 0)
        throw new IllegalArgumentException()

      X
    }

    /** X := A \ V, for arbitrary A */
    def QRSolve(X : DenseMatrix[Double], A : DenseMatrix[Double], V : DenseMatrix[Double]) = {
      require(X.offset == 0)
      require(A.offset == 0)
      require(V.offset == 0)
      require(X.rows == A.cols, "Wrong number of rows in return value")
      require(X.cols == V.cols, "Wrong number of rows in return value")
      val transpose = X.isTranspose

      val nrhs = V.cols

      // allocate temporary solution matrix
      val Xtmp = DenseMatrix.zeros[Double](math.max(A.rows, A.cols), nrhs)
      val M = if (!transpose) A.rows else A.cols
      Xtmp(0 until M,0 until nrhs) := V(0 until M, 0 until nrhs)

      val newData = A.data.clone()

      // query optimal workspace
      val queryWork = new Array[Double](1)
      val queryInfo = new intW(0)
      lapack.dgels(
        if (!transpose) "N" else "T",
        A.rows, A.cols, nrhs,
        newData, A.majorStride,
        Xtmp.data, math.max(1,math.max(A.rows,A.cols)),
        queryWork, -1, queryInfo)

      // allocate workspace
      val work = {
        val lwork = {
          if (queryInfo.`val` != 0)
            math.max(1, math.min(A.rows, A.cols) + math.max(math.min(A.rows, A.cols), nrhs))
          else
            math.max(queryWork(0).toInt, 1)
        }
        new Array[Double](lwork)
      }

      // compute factorization
      val info = new intW(0)
      lapack.dgels(
        if (!transpose) "N" else "T",
        A.rows, A.cols, nrhs,
        newData, A.majorStride,
        Xtmp.data, math.max(1,math.max(A.rows,A.cols)),
        work, work.length, info)

      if (info.`val` < 0)
        throw new IllegalArgumentException

      // extract solution
      val N = if (!transpose) A.cols else A.rows
      X(0 until N, 0 until nrhs) := Xtmp(0 until N, 0 until nrhs)

      X
    }
  }

  implicit object DenseMatrixCanSolveDenseVector extends BinaryOp[DenseMatrix[Double],DenseVector[Double],OpSolveMatrixBy,DenseVector[Double]] {
    override def apply(a : DenseMatrix[Double], b : DenseVector[Double]) = {
      val rv = a \ new DenseMatrix[Double](b.size, 1, b.data, b.offset, b.stride, true)
      new DenseVector[Double](rv.data)
    }
  }



  implicit val mulDVDM: BinaryOp[DenseVector[Double], DenseMatrix[Double], OpMulMatrix, DenseMatrix[Double]] = {
    new BinaryOp[DenseVector[Double], DenseMatrix[Double], OpMulMatrix, DenseMatrix[Double]] {
      def apply(a: DenseVector[Double], b: DenseMatrix[Double]) = {
        require(b.rows == 1)
//        val adata =  if(a.stride != 1) {
//          val v = DenseVector.zeros[Double](a.length)
//          v := a
//          v.data
//        } else {
//          a.data
//        }
        val rv = DenseMatrix.zeros[Double](a.length, b.cols)
        blas.dgemm("T", transposeString(b),
          rv.rows, rv.cols, 1,
          1.0, a.data, a.offset, a.stride, b.data, b.offset, b.majorStride,
          0.0, rv.data, 0, rv.rows)
        rv

      }
    }
  }



}


trait DenseMatrixOps { this: DenseMatrix.type =>

  import breeze.math.PowImplicits._

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dm_dm_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[DenseMatrix[T], DenseMatrix[T], Op] = new BinaryUpdateOp[DenseMatrix[T], DenseMatrix[T], Op] {
    def apply(a: DenseMatrix[T], b: DenseMatrix[T]):Unit = {
      val ad = a.data
      val bd = b.data
      var c = 0

      while(c < a.cols) {
        var r = 0
        while(r < a.rows) {
          ad(a.linearIndex(r, c)) = op(ad(a.linearIndex(r,c)), bd(b.linearIndex(r,c)))
          r += 1
        }
        c += 1
      }

    }
    implicitly[BinaryUpdateRegistry[Matrix[T], Matrix[T], Op]].register(this)
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dm_s_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[DenseMatrix[T], T, Op] = new BinaryUpdateOp[DenseMatrix[T], T, Op] {
    def apply(a: DenseMatrix[T], b: T):Unit = {
      val ad = a.data
      var c = 0

      while(c < a.cols) {
        var r = 0
        while(r < a.rows) {
          ad(a.linearIndex(r, c)) = op(ad(a.linearIndex(r,c)), b)
          r += 1
        }
        c += 1
      }

    }
    implicitly[BinaryUpdateRegistry[Matrix[T], T, Op]].register(this)
  }


  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def op_DM_S[@expand.args(Int, Long, Float, Double, BigInt, Complex) T,
                       @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpMod, OpDiv, OpPow) Op]: BinaryOp[DenseMatrix[T], T, Op, DenseMatrix[T]] = {
    val uop = implicitly[BinaryUpdateOp[DenseMatrix[T], T, Op]]
    new BinaryOp[DenseMatrix[T],  T, Op, DenseMatrix[T]] {
      override def apply(a : DenseMatrix[T], b: T) = {
        val c = copy(a)
        uop(c, b)
        c
      }
      implicitly[BinaryRegistry[Matrix[T], T, Op, Matrix[T]]].register(this)
    }
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def op_DM_DM[@expand.args(Int, Long, Float, Double, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMod, OpDiv, OpPow) Op]: BinaryOp[DenseMatrix[T], DenseMatrix[T], Op, DenseMatrix[T]] = {
    val uop = implicitly[BinaryUpdateOp[DenseMatrix[T], DenseMatrix[T], Op]]
    new BinaryOp[DenseMatrix[T],  DenseMatrix[T], Op, DenseMatrix[T]] {
      override def apply(a : DenseMatrix[T], b: DenseMatrix[T]) = {
        val c = copy(a)
        uop(c, b)
        c
      }
      implicitly[BinaryRegistry[Matrix[T], Matrix[T], Op, Matrix[T]]].register(this)
    }
  }

}

trait DenseMatrixOpsLowPrio { this: DenseMatrixOps =>
  // LOL, if we explicitly annotate the type, then the implicit resolution thing will load this recursively.
  // If we don't, then everything works ok.
  @expand
  implicit def canMulM_V_def[@expand.args(Int, Float, Double, Long, Complex, BigInt) T, A, B](implicit bb :  B <:< Vector[T]) = (
    implicitly[BinaryOp[DenseMatrix[T], Vector[T], OpMulMatrix, DenseVector[T]]].asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseVector[T]]]
    )

  // ibid.
  @expand
  implicit def canMulM_M_def[@expand.args(Int, Float, Double, Long, Complex, BigInt) T, B](implicit bb :  B <:< Matrix[T]) = (
    implicitly[BinaryOp[DenseMatrix[T], Matrix[T], OpMulMatrix, DenseMatrix[T]]].asInstanceOf[BinaryOp[DenseMatrix[T], B, OpMulMatrix, DenseMatrix[T]]]
    )
}

trait DenseMatrixMultOps extends DenseMatrixOps with DenseMatrixOpsLowPrio { this: DenseMatrix.type =>
  // I don't know why this import is necessary to make the DefaultArrayValue do the right thing.
  // If I remove the import breeze.storage.DefaultArrayValue._, everything breaks, for some reason.
  import breeze.math.Complex.ComplexDefaultArrayValue

  @expand
  @expand.valify
  implicit def op_DM_V[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]:BinaryRegistry[DenseMatrix[T], Vector[T], OpMulMatrix, DenseVector[T]] = new BinaryRegistry[DenseMatrix[T], Vector[T], OpMulMatrix, DenseVector[T]] {
    override def bindingMissing(a: DenseMatrix[T], b: Vector[T]) = {

      // TODO: this could probably be much faster?
      require(a.cols == b.length)
      val res = DenseVector.zeros[T](a.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          res(r) += v * b(c)
          r += 1
        }
        c += 1
      }

      res
    }
    implicitly[BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix, Vector[T]]].register(this)
  }


  @expand
  @expand.valify
  implicit def op_DM_M[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]:BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix, DenseMatrix[T]] = new BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix, DenseMatrix[T]] {
    override def bindingMissing(a: DenseMatrix[T], b: Matrix[T]) = {

      // TODO: this could probably be much faster
      val res = DenseMatrix.zeros[T](a.rows, b.cols)
      require(a.cols == b.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          var j = 0
          while(j < b.cols) {
            res(r, j) += v * b(c, j)
            j += 1
          }
          r += 1
        }
        c += 1
      }

      res
    }
    implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix, Matrix[T]]].register(this)
  }



  @expand
  @expand.valify
  implicit def op_DM_DM[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]:BinaryOp[DenseMatrix[T], DenseMatrix[T], OpMulMatrix, DenseMatrix[T]] = new BinaryOp[DenseMatrix[T], DenseMatrix[T], OpMulMatrix, DenseMatrix[T]] {
    implicitly[BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix, DenseMatrix[T]]].register(this)
    override def apply(a: DenseMatrix[T], b: DenseMatrix[T]) = {

      // TODO: this could probably be much faster
      val res = DenseMatrix.zeros[T](a.rows, b.cols)
      require(a.cols == b.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          var j = 0
          while(j < b.cols) {
            res(r, j) += v * b(c, j)
            j += 1
          }
          r += 1
        }
        c += 1
      }

      res
    }
    implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix, Matrix[T]]].register(this)
    implicitly[BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix, DenseMatrix[T]]].register(this)
  }

}

/**
 * Thrown when trying to solve using a singular matrix.
 *
 * @author dramage
 */
class MatrixSingularException(msg : String) extends RuntimeException(msg) {
  def this() = this(null)
}
