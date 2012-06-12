package breeze.linalg

import breeze.storage.DenseStorage
import operators._
import org.netlib.util.intW
import org.netlib.lapack.LAPACK
import org.netlib.blas.{Dgemm, BLAS}
import breeze.util.ArrayUtil
import breeze.numerics.IntMath
import support._

/**
 *
 * @author dlwh
 */

final class DenseMatrix[@specialized V](val data: Array[V],
                                        val rows: Int,
                                        val cols: Int,
                                        val majorStride: Int,
                                        val offset: Int = 0,
                                        val isTranspose: Boolean = false) extends StorageMatrix[V] with MatrixLike[V, DenseMatrix[V]] with DenseStorage[V] {
  def this(data: Array[V], rows: Int, cols: Int) = this(data, rows, cols, rows)
  def this(data: Array[V], rows: Int) = this(data, rows, {assert(data.length % rows == 0); data.length/rows})

  @inline final def apply(row: Int, col: Int) = {
    if(row < 0 || row > rows) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
    if(col < 0 || col > cols) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
      rawApply(linearIndex(row, col))
  }


  @inline final def linearIndex(row: Int, col: Int): Int = {
    if(isTranspose)
      offset + col + row * majorStride
    else
      offset + row + col * majorStride
  }

  @inline
  final def update(row: Int, col: Int, v: V) {
    if(row < 0 || row > rows) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
    if(col < 0 || col > cols) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
    rawUpdate(linearIndex(row, col), v)
  }

  def repr = this

  def activeIterator = iterator

  def activeValuesIterator = valuesIterator

  def activeKeysIterator = keysIterator

  def trace(implicit numeric: Numeric[V]) = diag(this).sum

  override def equals(p1: Any) = p1 match {
    case x: DenseMatrix[_] =>
      // todo: make this faster in obvious cases
      rows == x.rows && cols == x.cols && (valuesIterator sameElements x.valuesIterator )

    case _ => false
  }


  override def toString() = {
    // TODO:
    valuesIterator.mkString("DenseMatrix(",", ", ")")
  }
}

object DenseMatrix extends LowPriorityDenseMatrix
                           with DenseMatrixOps_Int
                           with DenseMatrixOps_F
                           with DenseMatrixOps_D with DenseMatrixMultiplyStuff {
  def zeros[V:ClassManifest](rows: Int, cols: Int) = {
    val data = new Array[V](rows * cols)
    new DenseMatrix(data, rows, cols)
  }

  /** Creates a dense matrix of the given value repeated of the requested size. */
  def fill[V:ClassManifest](rows: Int, cols: Int)(value: =>V) = {
    new DenseMatrix[V](Array.fill(rows * cols)(value), rows, cols)
  }

  def tabulate[V:ClassManifest](rows : Int, cols : Int)(fn : (Int, Int) => V) = {
    new DenseMatrix(Array.tabulate(rows * cols)(i => fn(i % rows, i / rows)), rows, cols)
  }

  /** Static constructor for a literal matrix. */
  def apply[R,V](rows : R*)(implicit rl : LiteralRow[R,V], man : ClassManifest[V]) = {
    val nRows = rows.length
    val ns = rl.length(rows(0))
    val rv = zeros(nRows, ns)
    for ((row,i) <- rows.zipWithIndex) {
      rl.foreach(row, {(j, v) => rv(i,j) = v})
    }
    rv
  }

  /** Horizontally tiles some matrices. They must have the same number of rows */
  def horzcat[V](matrices: DenseMatrix[V]*)(implicit opset: BinaryUpdateOp[DenseMatrix[V], DenseMatrix[V], OpSet], vman: ClassManifest[V]) = {
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
          new DenseMatrix(m.data, 1, m.cols, m.majorStride, m.offset + row)
        else
          new DenseMatrix(m.data, 1, m.cols, 1, m.offset + row * m.cols)
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
        if(rows.isEmpty) new DenseMatrix(m.data, 0, 0, 1, 0)
        else if(!m.isTranspose) {
          require(rows.step == 1, "Sorry, we can't support row ranges with step sizes other than 1")
          val first = rows.head
          new DenseMatrix(m.data, rows.length, m.cols, m.majorStride, m.offset + first)
        } else {
          canSliceCols(m.t, ::, rows).t
        }
      }
    }
  }

  implicit def canSliceCols[V]: CanSlice2[DenseMatrix[V], ::.type, Range, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], ::.type, Range, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], ignored: ::.type, cols: Range) = {
        if(cols.isEmpty) new DenseMatrix(m.data, 0, 0, 1, 0)
        else if(!m.isTranspose) {
          val first = cols.head
          new DenseMatrix(m.data, m.rows, cols.length, m.majorStride * cols.step, m.offset + first * m.rows)
        } else {
          canSliceRows(m.t, cols, ::).t
        }
      }
    }
  }

  implicit def canSliceColsAndRows[V]: CanSlice2[DenseMatrix[V], Range, Range, DenseMatrix[V]] = {
    new CanSlice2[DenseMatrix[V], Range, Range, DenseMatrix[V]] {
      def apply(m: DenseMatrix[V], rows: Range, cols: Range) = {
        if(rows.isEmpty || cols.isEmpty) new DenseMatrix(m.data, 0, 0, 1, 0)
        else if(!m.isTranspose) {
          require(rows.step == 1, "Sorry, we can't support row ranges with step sizes other than 1 for non transposed matrices")
          val first = cols.head
          new DenseMatrix(m.data, rows.length, cols.length, m.majorStride * cols.step, m.offset + first * m.rows + rows.head)
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
        if(cols.isEmpty) new DenseMatrix(m.data, 0, 0, 1, 0)
        else if(!m.isTranspose) {
          val first = cols.head
          new DenseMatrix(m.data, 1, cols.length, m.majorStride * cols.step, m.offset + first * m.rows + row)
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
          new DenseVector(m.data, m.offset + rows.head, rows.step, rows.length)
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
        var i = 0
        while (i < from.rows) {
          var j = 0
          while(j < from.cols) {
            data(i + j * from.rows) = fn(from(i, j))
            j += 1
          }
          i += 1
        }
        new DenseMatrix[R](data, from.rows, from.cols)
      }

      override def mapActive(from : DenseMatrix[V], fn : (V=>R)) =
        map(from, fn)
    }
  }

  implicit def canMapKeyValuePairs[V, R:ClassManifest] = {
    new CanMapKeyValuePairs[DenseMatrix[V],(Int,Int),V,R,DenseMatrix[R]] {
      override def map(from : DenseMatrix[V], fn : (((Int,Int),V)=>R)) = {
        val data = new Array[R](from.data.length)
        var i = 0
        while (i < from.rows) {
          var j = 0
          while(j < from.cols) {
            data(i + j * from.rows) = fn(i -> j, from(i, j))
            j += 1
          }
          i += 1
        }
        new DenseMatrix(data, from.rows, from.cols)
      }

      override def mapActive(from : DenseMatrix[V], fn : (((Int,Int),V)=>R)) =
        map(from, fn)
    }
  }

  implicit def canTranspose[V]: CanTranspose[DenseMatrix[V], DenseMatrix[V]] = {
    new CanTranspose[DenseMatrix[V], DenseMatrix[V]] {
      def apply(from: DenseMatrix[V]) = {
        new DenseMatrix(from.data, offset = from.offset, cols = from.rows, rows = from.cols, majorStride = from.majorStride, isTranspose = !from.isTranspose)
      }
    }
  }

  implicit def canCopyDenseMatrix[V:ClassManifest] = new CanCopy[DenseMatrix[V]] {
    def apply(v1: DenseMatrix[V]) = {
      new DenseMatrix(Array.tabulate(v1.size)(i => v1(i % v1.rows, i / v1.rows)), v1.rows, v1.cols)
    }
  }

  implicit def binaryOpFromBinaryUpdateOp[V, Other, Op<:OpType](implicit copy: CanCopy[DenseMatrix[V]], op: BinaryUpdateOp[DenseMatrix[V], Other, Op], man: ClassManifest[V]) = {
    new BinaryOp[DenseMatrix[V], Other, Op, DenseMatrix[V]] {
      override def apply(a : DenseMatrix[V], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }



  implicit val setMM_D: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpSet] = new SetMMOp[Double]
  implicit val setMM_F: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpSet]  = new SetMMOp[Float]
  implicit val setMM_I: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpSet]  = new SetMMOp[Int]
}

trait LowPriorityDenseMatrix {
  class SetMMOp[@specialized V] extends BinaryUpdateOp[DenseMatrix[V], DenseMatrix[V], OpSet] {
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

  class SetMSOp[@specialized V] extends BinaryUpdateOp[DenseMatrix[V], V, OpSet] {
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

  implicit def setMM[V]: BinaryUpdateOp[DenseMatrix[V], DenseMatrix[V], OpSet] = new SetMMOp[V]
  implicit def setMV[V]: BinaryUpdateOp[DenseMatrix[V], V, OpSet] = new SetMSOp[V]
}

trait DenseMatrixMultiplyStuff {
  implicit object DenseMatrixDMulDenseMatrixD
  extends BinaryOp[DenseMatrix[Double],DenseMatrix[Double],OpMulMatrix,DenseMatrix[Double]] {
    def apply(a : DenseMatrix[Double], b : DenseMatrix[Double]) = {
      val rv = DenseMatrix.zeros[Double](a.rows, b.cols)
      Dgemm.dgemm(transposeString(a), transposeString(b),
        rv.rows, rv.cols, a.cols,
        1.0, a.data, a.offset, a.majorStride, b.data, b.offset, b.majorStride,
        0.0, rv.data, 0, rv.rows)
      rv
    }
  }


  def transposeString(a: DenseMatrix[Double]): String = {
    if (a.isTranspose) "t" else "n"
  }

  implicit object DenseMatrixDMulDenseVectorD
  extends BinaryOp[DenseMatrix[Double],DenseVector[Double],OpMulMatrix,DenseVector[Double]] {
    def apply(a : DenseMatrix[Double], b : DenseVector[Double]) = {
      val rv = DenseVector.zeros[Double](a.rows)
      org.netlib.blas.Dgemv.dgemv(transposeString(a),
        a.rows, a.cols,
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

    /** X := A \ X */
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

    /** X := A \ V */
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
      for (j <- 0 until nrhs; i <- 0 until M) { Xtmp(i,j) = V(i,j); }

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
      for (j <- 0 until nrhs; i <- 0 until N) X(i,j) = Xtmp(i,j)

      X
    }
  }

  implicit object DenseMatrixCanSolveDenseVector extends BinaryOp[DenseMatrix[Double],DenseVector[Double],OpSolveMatrixBy,DenseVector[Double]] {
    override def apply(a : DenseMatrix[Double], b : DenseVector[Double]) = {
      val rv = a \ new DenseMatrix[Double](b.data, b.size, 1)
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


trait DenseMatrixOps_Int { this: DenseMatrix.type =>
  implicit val canAddIntoMM_I: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpAdd] {
      def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) += bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canAddIntoMS_I: BinaryUpdateOp[DenseMatrix[Int], Int, OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, OpAdd] {
      def apply(a: DenseMatrix[Int], b: Int) {
        if (b == 0) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) += b
            r += 1
          }
          c += 1
        }
      }
    }
  }


  implicit val canSubIntoMM_I: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpSub] {
      def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) -= bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canSubIntoMS_I: BinaryUpdateOp[DenseMatrix[Int], Int, OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, OpSub] {
      def apply(a: DenseMatrix[Int], b: Int) {
        if (b == 0) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) -= b
            r += 1
          }
          c += 1
        }
      }
    }
  }


  implicit val canMulIntoMM_I: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpMulScalar] {
      def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) *= bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canMulIntoMS_I: BinaryUpdateOp[DenseMatrix[Int], Int, OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, OpMulScalar] {
      def apply(a: DenseMatrix[Int], b: Int) {
        if (b == 1) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) *= b
            r += 1
          }
          c += 1
        }
      }
    }
  }


  implicit val canDivIntoMM_I: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpDiv] {
      def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) /= bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canDivIntoMS_I: BinaryUpdateOp[DenseMatrix[Int], Int, OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, OpDiv] {
      def apply(a: DenseMatrix[Int], b: Int) {
        if (b == 1) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) /= b
            r += 1
          }
          c += 1
        }
      }
    }
  }


  implicit val canModIntoMM_I: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpMod] {
      def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) %= bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canModIntoMS_I: BinaryUpdateOp[DenseMatrix[Int], Int, OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, OpMod] {
      def apply(a: DenseMatrix[Int], b: Int) {

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) %= b
            r += 1
          }
          c += 1
        }
      }
    }
  }


  implicit val canPowIntoMM_I: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], OpPow] {
      def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) = IntMath.ipow(ad(a.linearIndex(r,c)), bd(b.linearIndex(r, c)))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canPowIntoMS_I: BinaryUpdateOp[DenseMatrix[Int], Int, OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, OpPow] {
      def apply(a: DenseMatrix[Int], b: Int) {
        if(b == 1) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = IntMath.ipow(ad(a.linearIndex(r,c)),b)
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canAddMM_I = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Int]]], canAddIntoMM_I, implicitly[ClassManifest[Int]])
  implicit val canAddMS_I = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Int]]], canAddIntoMS_I, implicitly[ClassManifest[Int]])
  implicit val canSubMM_I = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Int]]], canSubIntoMM_I, implicitly[ClassManifest[Int]])
  implicit val canSubMS_I = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Int]]], canSubIntoMS_I, implicitly[ClassManifest[Int]])
  implicit val canMulMM_I = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Int]]], canMulIntoMM_I, implicitly[ClassManifest[Int]])
  implicit val canMulMS_I = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Int]]], canMulIntoMS_I, implicitly[ClassManifest[Int]])
  implicit val canDivMM_I = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Int]]], canDivIntoMM_I, implicitly[ClassManifest[Int]])
  implicit val canDivMS_I = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Int]]], canDivIntoMS_I, implicitly[ClassManifest[Int]])
  implicit val canModMM_I = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Int]]], canModIntoMM_I, implicitly[ClassManifest[Int]])
  implicit val canModMS_I = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Int]]], canModIntoMS_I, implicitly[ClassManifest[Int]])
  implicit val canPowMM_I = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Int]]], canPowIntoMM_I, implicitly[ClassManifest[Int]])
  implicit val canPowMS_I = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Int]]], canPowIntoMS_I, implicitly[ClassManifest[Int]])
}

trait DenseMatrixOps_F { this: DenseMatrix.type =>
  implicit val canAddIntoMM_F: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpAdd] {
      def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) += bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canAddIntoMS_F: BinaryUpdateOp[DenseMatrix[Float], Float, OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, OpAdd] {
      def apply(a: DenseMatrix[Float], b: Float) {
        if (b == 0) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) += b
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canSubIntoMM_F: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpSub] {
      def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) -= bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canSubIntoMS_F: BinaryUpdateOp[DenseMatrix[Float], Float, OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, OpSub] {
      def apply(a: DenseMatrix[Float], b: Float) {
        if (b == 0) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) -= b
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canMulIntoMM_F: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpMulScalar] {
      def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) *= bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canMulIntoMS_F: BinaryUpdateOp[DenseMatrix[Float], Float, OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, OpMulScalar] {
      def apply(a: DenseMatrix[Float], b: Float) {
        if (b == 1) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) *= b
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canDivIntoMM_F: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpDiv] {
      def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) /= bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canDivIntoMS_F: BinaryUpdateOp[DenseMatrix[Float], Float, OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, OpDiv] {
      def apply(a: DenseMatrix[Float], b: Float) {
        if (b == 1) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) /= b
            r += 1
          }
          c += 1
        }
      }
    }
  }


  implicit val canModIntoMM_F: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpMod] {
      def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) %= bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canModIntoMS_F: BinaryUpdateOp[DenseMatrix[Float], Float, OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, OpMod] {
      def apply(a: DenseMatrix[Float], b: Float) {

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) %= b
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canPowIntoMM_F: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], OpPow] {
      def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) = scala.math.pow(ad(a.linearIndex(r,c)), bd(b.linearIndex(r, c))).toFloat
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canPowIntoMS_F: BinaryUpdateOp[DenseMatrix[Float], Float, OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, OpPow] {
      def apply(a: DenseMatrix[Float], b: Float) {
        if(b == 1) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = scala.math.pow(ad(a.linearIndex(r,c)),b).toFloat
            r += 1
          }
          c += 1
        }
      }
    }
  }


  implicit val canAddMM_F = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Float]]], canAddIntoMM_F, implicitly[ClassManifest[Float]])
  implicit val canAddMS_F = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Float]]], canAddIntoMS_F, implicitly[ClassManifest[Float]])
  implicit val canSubMM_F = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Float]]], canSubIntoMM_F, implicitly[ClassManifest[Float]])
  implicit val canSubMS_F = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Float]]], canSubIntoMS_F, implicitly[ClassManifest[Float]])
  implicit val canMulMM_F = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Float]]], canMulIntoMM_F, implicitly[ClassManifest[Float]])
  implicit val canMulMS_F = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Float]]], canMulIntoMS_F, implicitly[ClassManifest[Float]])
  implicit val canDivMM_F = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Float]]], canDivIntoMM_F, implicitly[ClassManifest[Float]])
  implicit val canDivMS_F = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Float]]], canDivIntoMS_F, implicitly[ClassManifest[Float]])
  implicit val canModMM_F = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Float]]], canModIntoMM_F, implicitly[ClassManifest[Float]])
  implicit val canModMS_F = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Float]]], canModIntoMS_F, implicitly[ClassManifest[Float]])
  implicit val canPowMM_F = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Float]]], canPowIntoMM_F, implicitly[ClassManifest[Float]])
  implicit val canPowMS_F = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Float]]], canPowIntoMS_F, implicitly[ClassManifest[Float]])
}


trait DenseMatrixOps_D { this: DenseMatrix.type =>
  implicit val canAddIntoMM_D: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpAdd] {
      def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) += bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canAddIntoMS_D: BinaryUpdateOp[DenseMatrix[Double], Double, OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, OpAdd] {
      def apply(a: DenseMatrix[Double], b: Double) {
        if (b == 0) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) += b
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canSubIntoMM_D: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpSub] {
      def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) -= bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canSubIntoMS_D: BinaryUpdateOp[DenseMatrix[Double], Double, OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, OpSub] {
      def apply(a: DenseMatrix[Double], b: Double) {
        if (b == 0) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) -= b
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canMulIntoMM_D: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpMulScalar] {
      def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) *= bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canMulIntoMS_D: BinaryUpdateOp[DenseMatrix[Double], Double, OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, OpMulScalar] {
      def apply(a: DenseMatrix[Double], b: Double) {
        if (b == 1) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) *= b
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canDivIntoMM_D: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpDiv] {
      def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) /= bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canDivIntoMS_D: BinaryUpdateOp[DenseMatrix[Double], Double, OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, OpDiv] {
      def apply(a: DenseMatrix[Double], b: Double) {
        if (b == 1) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) /= b
            r += 1
          }
          c += 1
        }
      }
    }
  }


  implicit val canModIntoMM_D: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpMod] {
      def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) %= bd(b.linearIndex(r, c))
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canModIntoMS_D: BinaryUpdateOp[DenseMatrix[Double], Double, OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, OpMod] {
      def apply(a: DenseMatrix[Double], b: Double) {

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) %= b
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canPowIntoMM_D: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], OpPow] {
      def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            // TODO: this is likely to wreck havoc on the branch predictor? Do we care?
            // we can hand unroll the loops if we want.
            ad(a.linearIndex(r, c)) = scala.math.pow(ad(a.linearIndex(r,c)), bd(b.linearIndex(r, c))).toDouble
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canPowIntoMS_D: BinaryUpdateOp[DenseMatrix[Double], Double, OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, OpPow] {
      def apply(a: DenseMatrix[Double], b: Double) {
        if(b == 1) return

        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = scala.math.pow(ad(a.linearIndex(r,c)),b).toDouble
            r += 1
          }
          c += 1
        }
      }
    }
  }

  implicit val canAddMM_D = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Double]]], canAddIntoMM_D, implicitly[ClassManifest[Double]])
  implicit val canAddMS_D = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Double]]], canAddIntoMS_D, implicitly[ClassManifest[Double]])
  implicit val canSubMM_D = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Double]]], canSubIntoMM_D, implicitly[ClassManifest[Double]])
  implicit val canSubMS_D = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Double]]], canSubIntoMS_D, implicitly[ClassManifest[Double]])
//  implicit val canMulMM_D = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Double]]], canMulIntoMM_D, implicitly[ClassManifest[Double]])
  implicit val canMulMS_D = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Double]]], canMulIntoMS_D, implicitly[ClassManifest[Double]])
  implicit val canDivMM_D = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Double]]], canDivIntoMM_D, implicitly[ClassManifest[Double]])
  implicit val canDivMS_D = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Double]]], canDivIntoMS_D, implicitly[ClassManifest[Double]])
  implicit val canModMM_D = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Double]]], canModIntoMM_D, implicitly[ClassManifest[Double]])
  implicit val canModMS_D = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Double]]], canModIntoMS_D, implicitly[ClassManifest[Double]])
  implicit val canPowMM_D = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Double]]], canPowIntoMM_D, implicitly[ClassManifest[Double]])
  implicit val canPowMS_D = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseMatrix[Double]]], canPowIntoMS_D, implicitly[ClassManifest[Double]])
}

/**
 * Thrown when trying to solve using a singular matrix.
 *
 * @author dramage
 */
class MatrixSingularException(msg : String) extends RuntimeException(msg) {
  def this() = this(null)
}