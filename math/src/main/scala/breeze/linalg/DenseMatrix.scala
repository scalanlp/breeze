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
import org.netlib.util.intW
import org.netlib.lapack.LAPACK
import org.netlib.blas.{Dgemm, BLAS}
import breeze.util.ArrayUtil
import breeze.numerics.IntMath
import support._
import breeze.generic.{URFunc, CanCollapseAxis, CanMapValues}
import breeze.math.Semiring
import breeze.storage.DefaultArrayValue
import org.jblas.NativeBlas

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
  def this(rows: Int, cols: Int)(implicit man: ClassManifest[V]) = this(rows, cols, new Array[V](rows * cols), 0, rows)
  /** Creates a matrix with the specified data array, rows, and columns. Data must be column major */
  def this(rows: Int, cols: Int, data: Array[V], offset: Int = 0) = this(rows, cols, data, offset, rows)
  /** Creates a matrix with the specified data array and rows. columns inferred automatically */
  def this(rows: Int, data: Array[V], offset: Int = 0) = this(rows, {assert(data.length % rows == 0); data.length/rows}, data, offset)

  def apply(row: Int, col: Int) = {
    if(row < 0 || row > rows) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
    if(col < 0 || col > cols) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
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
}

object DenseMatrix extends LowPriorityDenseMatrix
                           with DenseMatrixOps_Int
                           with DenseMatrixOps_Float
                           with DenseMatrixOps_Double
                           with DenseMatrixMultOps_Int
                           with DenseMatrixMultOps_Float
                           with DenseMatrixMultOps_Double
                           with DenseMatrixMultiplyStuff
                           with MatrixConstructors[DenseMatrix]  {
  /**
   * The standard way to create an empty matrix, size is rows * cols
   */
  def zeros[@specialized(Int, Float, Double) V:ClassManifest:DefaultArrayValue](rows: Int, cols: Int) = {
    val data = new Array[V](rows * cols)
    new DenseMatrix(rows, cols, data)
  }

  def create[@specialized(Int, Float, Double) V:DefaultArrayValue](rows: Int, cols: Int, data: Array[V]): DenseMatrix[V] = {
    new DenseMatrix(rows, cols, data)
  }

  /**
   * Creates a square diagonal array of size dim x dim, with 1's along the diagonal.
   */
  def eye[@specialized(Int, Float, Double) V: ClassManifest:Semiring](dim: Int) = {
    val r = zeros[V](dim, dim)
    breeze.linalg.diag(r) := implicitly[Semiring[V]].one
    r
  }

  /** Horizontally tiles some matrices. They must have the same number of rows */
  def horzcat[M,V](matrices: M*)(implicit ev: M <:< Matrix[V], opset: BinaryUpdateOp[DenseMatrix[V], M, OpSet], vman: ClassManifest[V]) = {
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
  def vertcat[V](matrices: DenseMatrix[V]*)(implicit opset: BinaryUpdateOp[DenseMatrix[V], DenseMatrix[V], OpSet], vman: ClassManifest[V]) = {
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
        if(!m.isTranspose)
          new DenseVector(m.data, length = m.rows, offset = col * m.rows + m.offset, stride=1)
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

  implicit def canSliceCols[V]: CanSlice2[DenseMatrix[V], ::.type, Range, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], ::.type, Range, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], ignored: ::.type, cols: Range) = {
        if(cols.isEmpty) new DenseMatrix(0, 0, m.data, 0, 1)
        else if(!m.isTranspose) {
          val first = cols.head
          new DenseMatrix(m.rows, cols.length, m.data, m.offset + first * m.rows, m.majorStride * cols.step)
        } else {
          canSliceRows(m.t, cols, ::).t
        }
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

  implicit def canMapValues[V, R:ClassManifest] = {
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

  implicit def canMapKeyValuePairs[V, R:ClassManifest] = {
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

  implicit def canCopyDenseMatrix[V:ClassManifest] = new CanCopy[DenseMatrix[V]] {
    def apply(v1: DenseMatrix[V]) = {
      val result = DenseMatrix.zeros(v1.rows, v1.cols)
      result := v1
      result
    }
  }

  def binaryOpFromBinaryUpdateOp[V, Other, Op<:OpType](implicit copy: CanCopy[DenseMatrix[V]], op: BinaryUpdateOp[DenseMatrix[V], Other, Op], man: ClassManifest[V]) = {
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
   * Maps the columns into a new dense matrix
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def canMapRows[V:ClassManifest]: CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], DenseVector[V], DenseMatrix[V]]  = new CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], DenseVector[V], DenseMatrix[V]] {
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
   * Returns a numRows DenseVector
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def canMapCols[V:ClassManifest] = new CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], DenseVector[V], DenseMatrix[V]] {
    def apply(from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => DenseVector[V]): DenseMatrix[V] = {
      var result:DenseMatrix[V] = null
      val t = from.t
      for(r <- 0 until from.cols) {
        val row = f(t(::, r))
        if(result eq null) {
          result = DenseMatrix.zeros[V](from.rows, row.length)
        }
        result.t apply (::, r) := row
      }
      result
    }
  }


//  implicit val setMM_D: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpSet] = new SetDMDMOp[Double]
//  implicit val setMM_F: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpSet]  = new SetDMDMOp[Float]
//  implicit val setMM_I: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpSet]  = new SetDMDMOp[Int]

  implicit val setMV_D: BinaryUpdateOp[DenseMatrix[Double], DenseVector[Double], OpSet] = new SetDMDVOp[Double]
  implicit val setMV_F: BinaryUpdateOp[DenseMatrix[Float], DenseVector[Float], OpSet]  = new SetDMDVOp[Float]
  implicit val setMV_I: BinaryUpdateOp[DenseMatrix[Int], DenseVector[Int], OpSet]  = new SetDMDVOp[Int]
}

trait LowPriorityDenseMatrix1 {
  protected implicit def dontNeedDefaultArrayValue[V]: DefaultArrayValue[V] = null.asInstanceOf[DefaultArrayValue[V]]
  /**
   * Returns a 1xnumCols DenseMatrix
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def canCollapseRows[V, R:ClassManifest]: CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], R, DenseMatrix[R]]  = new CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], R, DenseMatrix[R]] {
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
  implicit def canCollapseCols[V, R:ClassManifest] = new CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], R, DenseVector[R]] {
    def apply(from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => R): DenseVector[R] = {
      val result = DenseVector.zeros[R](from.rows)
      val t = from.t
      for(r <- 0 until from.cols) {
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

trait DenseMatrixMultiplyStuff extends DenseMatrixOps_Double with DenseMatrixMultOps_Double { this: DenseMatrix.type =>
  implicit object DenseMatrixDMulDenseMatrixD
  extends BinaryOp[DenseMatrix[Double],DenseMatrix[Double],OpMulMatrix,DenseMatrix[Double]] {
    def apply(a : DenseMatrix[Double], b : DenseMatrix[Double]) = {
      val rv = DenseMatrix.zeros[Double](a.rows, b.cols)
      require(a.cols == b.rows, "Dimension mismatch!")
      if(math.max(a.size,b.size) >= 1000 && useNativeLibraries)
        NativeBlas.dgemm(transposeString(a).charAt(0), transposeString(b).charAt(0),
              rv.rows, rv.cols, a.cols,
              1.0, a.data, a.offset, a.majorStride,
              b.data, b.offset, b.majorStride,
              0.0, rv.data, 0, rv.rows)
      else  Dgemm.dgemm(transposeString(a), transposeString(b),
        rv.rows, rv.cols, a.cols,
        1.0, a.data, a.offset, a.majorStride, b.data, b.offset, b.majorStride,
        0.0, rv.data, 0, rv.rows)
      rv
    }
  }


  private def transposeString(a: DenseMatrix[Double]): String = {
    if (a.isTranspose) "t" else "n"
  }

  implicit object DenseMatrixDMulDenseVectorD
  extends BinaryOp[DenseMatrix[Double],DenseVector[Double],OpMulMatrix,DenseVector[Double]] {
    def apply(a : DenseMatrix[Double], b : DenseVector[Double]) = {
      require(a.cols == b.length, "Dimension mismatch!")
      val rv = DenseVector.zeros[Double](a.rows)
      org.netlib.blas.Dgemv.dgemv(transposeString(a),
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

      val info = new intW(0)
      LAPACK.getInstance().dgesv(
        A.rows, X.cols,
        A.data.clone(), A.majorStride,
        piv,
        X.data, X.majorStride, info)

      if (info.`val` > 0)
        throw new MatrixSingularException()
      else if (info.`val` < 0)
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
      LAPACK.getInstance().dgels(
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
      LAPACK.getInstance().dgels(
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
      val rv = a \ new DenseMatrix[Double](b.size, 1, b.data, b.offset, b.stride)
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
        Dgemm.dgemm("t", transposeString(b),
          rv.rows, rv.cols, 1,
          1.0, a.data, a.offset, a.stride, b.data, b.offset, b.majorStride,
          0.0, rv.data, 0, rv.rows)
        rv

      }
    }
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
