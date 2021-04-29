package breeze.linalg.operators

import breeze.generic.UFunc
import breeze.linalg
import breeze.linalg.Axis.{_0, _1}
import breeze.linalg._
import breeze.linalg.support.{CanCollapseAxis, CanIterateAxis, CanSlice2, CanTraverseAxis, CanZipMapKeyValues, CanZipMapValues}
import breeze.macros.expand
import breeze.math.{Field, Semiring}
import breeze.storage.Zero
import breeze.util.{ArrayUtil, ReflectionUtil}
import com.github.fommil.netlib.BLAS.{getInstance => blas}
import com.github.fommil.netlib.LAPACK.{getInstance => lapack}
import org.netlib.util.intW
import scalaxy.debug._
import breeze.macros._
import breeze.math.PowImplicits._

import scala.reflect.ClassTag
import scala.{specialized => spec}

trait DenseMatrixOps extends MatrixOps with DenseMatrixExpandedOps with DenseMatrix_ComparisonOps
  with DenseMatrixMultOps with DenseMatrixMultiplyOps with DenseMatrixFloatMultiplyStuff with DenseMatrix_SliceOps {
  implicit val setMV_D: OpSet.InPlaceImpl2[DenseMatrix[Double], DenseVector[Double]] = new SetDMDVOp[Double]();
  implicit val setMV_F: OpSet.InPlaceImpl2[DenseMatrix[Float], DenseVector[Float]] = new SetDMDVOp[Float]();
  implicit val setMV_I: OpSet.InPlaceImpl2[DenseMatrix[Int], DenseVector[Int]] = new SetDMDVOp[Int]();

  /**
   * transforms each row into a new row, giving a new matrix.
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def DM_canMapRows[V, R: ClassTag: Zero](implicit implSet: OpSet.InPlaceImpl2[DenseVector[R], DenseVector[R]])
  : CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], DenseVector[R], DenseMatrix[R]] =
    new CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], DenseVector[R], DenseMatrix[R]] {
      def apply(from: DenseMatrix[V], axis: Axis._0.type)(f: (DenseVector[V]) => DenseVector[R]): DenseMatrix[R] = {
        var result: DenseMatrix[R] = null
        cforRange (0 until from.cols) { c =>
          val col = f(from(::, c))
          if (result eq null) {
            result = DenseMatrix.zeros[R](col.length, from.cols)
          }
          result(::, c) := col
        }

        if (result eq null) {
          DenseMatrix.zeros[R](0, from.cols)
        } else {
          result
        }
      }
    }

  implicit def DM_handholdCanMapRows[V]: CanCollapseAxis.HandHold[DenseMatrix[V], Axis._0.type, DenseVector[V]] =
    new CanCollapseAxis.HandHold[DenseMatrix[V], Axis._0.type, DenseVector[V]]()

  implicit def DM_canMapRowsBitVector[V: ClassTag: Zero]
  : CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], BitVector, DenseMatrix[Boolean]] =
    new CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], BitVector, DenseMatrix[Boolean]] {
      def apply(from: DenseMatrix[V], axis: Axis._0.type)(f: (DenseVector[V]) => BitVector): DenseMatrix[Boolean] = {
        var result: DenseMatrix[Boolean] = null
        cforRange(0 until from.cols) { c =>
          val col = f(from(::, c))
          if (result eq null) {
            result = DenseMatrix.zeros[Boolean](col.length, from.cols)
          }
          result(::, c) := col
        }

        if (result eq null) {
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
  implicit def DM_canMapCols[V, Res: ClassTag: Zero](
                                                   implicit implSet: OpSet.InPlaceImpl2[DenseVector[Res], DenseVector[Res]])
  : CanCollapseAxis[DenseMatrix[V], _1.type, DenseVector[V], DenseVector[Res], DenseMatrix[Res]] = {
    new CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], DenseVector[Res], DenseMatrix[Res]] {
      def apply(from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => DenseVector[Res]): DenseMatrix[Res] = {
        var result: DenseMatrix[Res] = null
        import from.rows
        val t = from.t
        cforRange(0 until from.rows) { r =>
          val row = f(t(::, r))
          if (result eq null) {
            // scala has decided this method is overloaded, and needs a result type.
            // It has a result type, and is not overloaded.
            //          result = DenseMatrix.zeros[V](from.rows, row.length)
            val data = new Array[Res](rows * row.length)
            result = DenseMatrix.create(rows, row.length, data)
          }
          result.t.apply(::, r) := row
        }

        if (result ne null) {
          result
        } else {
          val data = new Array[Res](0)
          result = DenseMatrix.create(rows, 0, data)
          result
        }
      }
    }
  }

  implicit def DM_handholdCanMapCols[V]: CanCollapseAxis.HandHold[DenseMatrix[V], Axis._1.type, DenseVector[V]] =
    new CanCollapseAxis.HandHold[DenseMatrix[V], Axis._1.type, DenseVector[V]]()

  implicit def DM_canMapColsBitVector[V: ClassTag: Zero]: CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], BitVector, DenseMatrix[Boolean]] =
    new CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], BitVector, DenseMatrix[Boolean]] {
      def apply(from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => BitVector): DenseMatrix[Boolean] = {
        var result: DenseMatrix[Boolean] = null
        import from.rows
        val t = from.t
        cforRange(0 until from.rows) { r =>
          val row = f(t(::, r))
          if (result eq null) {
            // scala has decided this method is overloaded, and needs a result type.
            // It has a result type, and is not overloaded.
            //          result = DenseMatrix.zeros[V](from.rows, row.length)
            val data = new Array[Boolean](rows * row.length)
            result = DenseMatrix.create(rows, row.length, data)
          }
          result.t.apply(::, r) := row
        }

        if (result ne null) {
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
  implicit def DM_canTraverseCols[V]: CanTraverseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V]] = {
    new CanTraverseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V]] {
      def apply[A](from: DenseMatrix[V], axis: Axis._0.type)(f: (DenseVector[V]) => A): Unit = {
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
  implicit def DM_canTraverseRows[V]: CanTraverseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V]] = {
    new CanTraverseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V]] {
      def apply[A](from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => A): Unit = {
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
  implicit def DM_canIterateCols[V]: CanIterateAxis[DenseMatrix[V], Axis._0.type, DenseVector[V]] = {
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
  implicit def DM_canIterateRows[V]: CanIterateAxis[DenseMatrix[V], Axis._1.type, DenseVector[V]] = {
    new CanIterateAxis[DenseMatrix[V], Axis._1.type, DenseVector[V]] {

      override def apply[A](from: DenseMatrix[V], axis: _1.type): Iterator[DenseVector[V]] = {
        (0 until from.rows).iterator.map(from(_, ::).t)
      }

    }
  }

  //  implicit val setMM_D: OpSet] = new SetDMDMOp[Double.InPlaceImpl2[DenseMatrix[Double], DenseMatrix[Double]]
  //  implicit val setMM_F: OpSet]  = new SetDMDMOp[Float.InPlaceImpl2[DenseMatrix[Float], DenseMatrix[Float]]
  //  implicit val setMM_I: OpSet]  = new SetDMDMOp[Int.InPlaceImpl2[DenseMatrix[Int], DenseMatrix[Int]]

  // There's a bizarre error specializing float's here.
  class CanZipMapValuesDenseMatrix[@spec(Double, Int, Float, Long) V, @specialized(Int, Double) RV: ClassTag]
    extends CanZipMapValues[DenseMatrix[V], V, RV, DenseMatrix[RV]] {

    def create(rows: Int, cols: Int) = DenseMatrix.create(rows, cols, new Array[RV](rows * cols), 0, rows)

    /**Maps all corresponding values from the two collection. */
    def map(from: DenseMatrix[V], from2: DenseMatrix[V], fn: (V, V) => RV) = {
      require(from.rows == from2.rows, "Vector row dimensions must match!")
      require(from.cols == from2.cols, "Vector col dimensions must match!")
      val result = create(from.rows, from.cols)
      cforRange2 (0 until from.cols, 0 until from.rows) { (j, i) =>
        result(i, j) = fn(from(i, j), from2(i, j))
      }
      result
    }
  }

  implicit def DM_zipMap[V, R: ClassTag]: CanZipMapValuesDenseMatrix[V, R] = new CanZipMapValuesDenseMatrix[V, R]
  implicit val DM_zipMap_d: CanZipMapValuesDenseMatrix[Double, Double] = new CanZipMapValuesDenseMatrix[Double, Double]
  implicit val DM_zipMap_f: CanZipMapValuesDenseMatrix[Float, Float] = new CanZipMapValuesDenseMatrix[Float, Float]
  implicit val DM_zipMap_i: CanZipMapValuesDenseMatrix[Int, Int] = new CanZipMapValuesDenseMatrix[Int, Int]

  class CanZipMapKeyValuesDenseMatrix[@spec(Double, Int, Float, Long) V, @specialized(Int, Double) RV: ClassTag]
    extends CanZipMapKeyValues[DenseMatrix[V], (Int, Int), V, RV, DenseMatrix[RV]] {

    def create(rows: Int, cols: Int) = DenseMatrix.create(rows, cols, new Array[RV](rows * cols), 0, rows)

    override def mapActive(
                            from: DenseMatrix[V],
                            from2: DenseMatrix[V],
                            fn: ((Int, Int), V, V) => RV): DenseMatrix[RV] = {
      map(from, from2, fn)
    }

    /**Maps all corresponding values from the two collection. */
    def map(from: DenseMatrix[V], from2: DenseMatrix[V], fn: ((Int, Int), V, V) => RV) = {
      require(from.rows == from2.rows, "Vector row dimensions must match!")
      require(from.cols == from2.cols, "Vector col dimensions must match!")
      val result = create(from.rows, from.cols)
      cforRange2 (0 until from.cols, 0 until from.rows) { (j, i) =>
        result(i, j) = fn((i, j), from(i, j), from2(i, j))
      }
      result
    }
  }

  implicit def DM_zipMapKV[V, R: ClassTag]: CanZipMapKeyValuesDenseMatrix[V, R] = new CanZipMapKeyValuesDenseMatrix[V, R]

  implicit def DM_canDim[E]: dim.Impl[DenseMatrix[E], (Int, Int)] = new dim.Impl[DenseMatrix[E], (Int, Int)] {
    def apply(v: DenseMatrix[E]): (Int, Int) = (v.rows, v.cols)
  }
}

trait DenseMatrixMultiplyOps extends DenseMatrixExpandedOps with DenseMatrixMultOps with DenseMatrix_SliceOps {

  // <editor-fold defaultstate="collapsed" desc=" OpMulMatrix implementations ">

  implicit def implOpMulMatrix_DVTt_DMT_eq_DMT[T](
      implicit op: OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]])
    : OpMulMatrix.Impl2[Transpose[DenseVector[T]], DenseMatrix[T], Transpose[DenseVector[T]]] =
    new OpMulMatrix.Impl2[Transpose[DenseVector[T]], DenseMatrix[T], Transpose[DenseVector[T]]] {
      override def apply(v: Transpose[DenseVector[T]], v2: DenseMatrix[T]): Transpose[DenseVector[T]] = {
        (v.inner.asDenseMatrix * v2).apply(0, ::)
      }
    }

  implicit def implOpMulMatrix_DVT_DMT_eq_DMT[T](
      implicit op: OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]])
    : OpMulMatrix.Impl2[DenseVector[T], DenseMatrix[T], DenseMatrix[T]] =
    new OpMulMatrix.Impl2[DenseVector[T], DenseMatrix[T], DenseMatrix[T]] {
      def apply(v: DenseVector[T], v2: DenseMatrix[T]): DenseMatrix[T] = {
        require(v2.rows == 1)
        // as DenseMatrix comes out as a row, but we want a column
        op(v.asDenseMatrix.t, v2)
      }
    }

  // for BLAS.dgemm/dgemv
  private def transposeString(a: DenseMatrix[Double]): String = if (a.isTranspose) "T" else "N"

  implicit object implOpMulMatrix_DMD_DMD_eq_DMD
      extends OpMulMatrix.Impl2[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] {

    def apply(_a: DenseMatrix[Double], _b: DenseMatrix[Double]): DenseMatrix[Double] = {

      require(_a.cols == _b.rows, "Dimension mismatch!")

      val rv: DenseMatrix[Double] = DenseMatrix.zeros[Double](_a.rows, _b.cols)

      if (_a.rows == 0 || _b.rows == 0 || _a.cols == 0 || _b.cols == 0) return rv

      // if we have a weird stride...
      val a: DenseMatrix[Double] =
        if (_a.majorStride < math.max(if (_a.isTranspose) _a.cols else _a.rows, 1)) _a.copy else _a
      val b: DenseMatrix[Double] =
        if (_b.majorStride < math.max(if (_b.isTranspose) _b.cols else _b.rows, 1)) _b.copy else _b

      blas.dgemm(
        transposeString(a),
        transposeString(b),
        rv.rows,
        rv.cols,
        a.cols,
        1.0,
        a.data,
        a.offset,
        a.majorStride,
        b.data,
        b.offset,
        b.majorStride,
        0.0,
        rv.data,
        0,
        rv.rows)
      rv
    }
    implicitly[BinaryRegistry[Matrix[Double], Matrix[Double], OpMulMatrix.type, Matrix[Double]]].register(this)
    implicitly[BinaryRegistry[DenseMatrix[Double], Matrix[Double], OpMulMatrix.type, DenseMatrix[Double]]]
      .register(this)
  }

  implicit object implOpMulMatrix_DMD_DVD_eq_DVD
      extends OpMulMatrix.Impl2[DenseMatrix[Double], DenseVector[Double], DenseVector[Double]] {
    def apply(a: DenseMatrix[Double], b: DenseVector[Double]): DenseVector[Double] = {

      require(a.cols == b.length, "Dimension mismatch!")

      if (a.rows == 0 || a.cols == 0) {
        return DenseVector.zeros[Double](a.rows)
      }

      val rv = DenseVector.zeros[Double](a.rows)

      blas.dgemv(
        transposeString(a),
        if (a.isTranspose) a.cols else a.rows,
        if (a.isTranspose) a.rows else a.cols,
        1.0,
        a.data,
        a.offset,
        a.majorStride,
        b.data,
        b.offset,
        b.stride,
        0.0,
        rv.data,
        rv.offset,
        rv.stride
      )
      rv
    }
  }

  implicit val implOpMulMatrix_DVD_DMD_eq_DMD
    : OpMulMatrix.Impl2[DenseVector[Double], DenseMatrix[Double], DenseMatrix[Double]] = {
    new OpMulMatrix.Impl2[DenseVector[Double], DenseMatrix[Double], DenseMatrix[Double]] {
      def apply(a: DenseVector[Double], b: DenseMatrix[Double]): DenseMatrix[Double] = {
        require(b.rows == 1)
        //        val adata =  if(a.stride != 1) {
        //          val v = DenseVector.zeros[Double](a.length)
        //          v := a
        //          v.data
        //        } else {
        //          a.data
        //        }
        val rv = DenseMatrix.zeros[Double](a.length, b.cols)
        blas.dgemm(
          "T",
          transposeString(b),
          rv.rows,
          rv.cols,
          1,
          1.0,
          a.data,
          a.offset,
          a.stride,
          b.data,
          b.offset,
          b.majorStride,
          0.0,
          rv.data,
          0,
          rv.rows)
        rv

      }
    }
  }

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" // <editor-fold defaultstate="collapsed" desc=" OpSolveMatrixBy implementations ">

  implicit object implOpSolveMatrixBy_DMD_DMD_eq_DMD
      extends OpSolveMatrixBy.Impl2[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] {

    override def apply(A: DenseMatrix[Double], V: DenseMatrix[Double]): DenseMatrix[Double] = {
      require(A.rows == V.rows, "Non-conformant matrix sizes")

      if (A.size == 0) {
        DenseMatrix.zeros[Double](0, 0)
      } else if (A.rows == A.cols) {
        // square: LUSolve
        val X = DenseMatrix.zeros[Double](V.rows, V.cols)
        X := V
        LUSolve(X, A)
        X
      } else {
        // non-square: QRSolve
        val X = DenseMatrix.zeros[Double](A.cols, V.cols)
        QRSolve(X, A, V)
        X
      }
    }

    /** X := A \ X, for square A */
    def LUSolve(X: DenseMatrix[Double], A: DenseMatrix[Double]): DenseMatrix[Double] = {

      val piv = new Array[Int](A.rows)
      val newA = A.copy
      assert(!newA.isTranspose)

      val info: Int = {
        val info = new intW(0)
        lapack.dgesv(
          A.rows,
          X.cols,
          newA.data,
          newA.offset,
          newA.majorStride,
          piv,
          0,
          X.data,
          X.offset,
          X.majorStride,
          info)
        info.`val`
      }

      if (info > 0)
        throw new MatrixSingularException()
      else if (info < 0)
        throw new IllegalArgumentException()

      X
    }

    /** X := A \ V, for arbitrary A */
    def QRSolve(X: DenseMatrix[Double], A: DenseMatrix[Double], V: DenseMatrix[Double]): DenseMatrix[Double] = {

      require(X.offset == 0)
      require(A.offset == 0)
      require(V.offset == 0)
      require(X.rows == A.cols, "Wrong number of rows in return value")
      require(X.cols == V.cols, "Wrong number of rows in return value")
      val transpose: Boolean = A.isTranspose

      val nrhs = V.cols

      // allocate temporary solution matrix
      val Xtmp = DenseMatrix.zeros[Double](math.max(A.rows, A.cols), nrhs)
      Xtmp(0 until A.rows, 0 until nrhs) := V(0 until A.rows, 0 until nrhs)

      val newData: Array[Double] = A.data.clone()

      // query optimal workspace
      val queryWork = new Array[Double](1)
      val queryInfo = new intW(0)
      lapack.dgels(
        if (!transpose) "N" else "T",
        if (!transpose) A.rows else A.cols,
        if (!transpose) A.cols else A.rows,
        nrhs,
        newData,
        A.majorStride,
        Xtmp.data,
        math.max(1, math.max(A.rows, A.cols)),
        queryWork,
        -1,
        queryInfo
      )

      // allocate workspace
      val work: Array[Double] = {
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
        if (!transpose) A.rows else A.cols,
        if (!transpose) A.cols else A.rows,
        nrhs,
        newData,
        A.majorStride,
        Xtmp.data,
        math.max(1, math.max(A.rows, A.cols)),
        work,
        work.length,
        info
      )

      if (info.`val` < 0)
        throw new IllegalArgumentException

      // extract solution
      X := Xtmp(0 until X.rows, 0 until nrhs)

      X
    }
  }

  implicit object implOpSolveMatrixBy_DMD_DVD_eq_DVD
      extends OpSolveMatrixBy.Impl2[DenseMatrix[Double], DenseVector[Double], DenseVector[Double]] {
    override def apply(a: DenseMatrix[Double], b: DenseVector[Double]): DenseVector[Double] = {
      val rv: DenseMatrix[Double] = a \ new DenseMatrix[Double](b.size, 1, b.data, b.offset, b.stride, true)
      new DenseVector[Double](rv.data)
    }
  }

  // </editor-fold>

}

// TODO: fix expand to allow us to remove this code duplication
// TODO: Rename/collapse trait
trait DenseMatrixFloatMultiplyStuff extends DenseMatrixExpandedOps with DenseMatrixMultOps {

  // <editor-fold defaultstate="collapsed" desc=" OpMulMatrix implementations ">

  implicit val impl_OpMulMatrix_DM_DM_eq_DM_Float: OpMulMatrix.Impl2[DenseMatrix[Float], DenseMatrix[Float], DenseMatrix[Float]] = {
    (_a: DenseMatrix[Float], _b: DenseMatrix[Float]) => {
      require(_a.cols == _b.rows, "Dimension mismatch!")
      val rv = DenseMatrix.zeros[Float](_a.rows, _b.cols)

      if (_a.rows != 0 && _b.rows != 0  &&  _a.cols != 0  &&  _b.cols != 0) {
        // if we have a weird stride...
        val a: DenseMatrix[Float] =
          if (_a.majorStride < math.max(if (_a.isTranspose) _a.cols else _a.rows, 1)) _a.copy else _a
        val b: DenseMatrix[Float] =
          if (_b.majorStride < math.max(if (_b.isTranspose) _b.cols else _b.rows, 1)) _b.copy else _b
        blas.sgemm(
          transposeString(a),
          transposeString(b),
          rv.rows,
          rv.cols,
          a.cols,
          1.0f,
          a.data,
          a.offset,
          a.majorStride,
          b.data,
          b.offset,
          b.majorStride,
          0.0f,
          rv.data,
          0,
          rv.rows)
      }
      rv
    }
  }
  implicitly[BinaryRegistry[Matrix[Float], Matrix[Float], OpMulMatrix.type, Matrix[Float]]]
    .register(impl_OpMulMatrix_DM_DM_eq_DM_Float)
  implicitly[BinaryRegistry[DenseMatrix[Float], Matrix[Float], OpMulMatrix.type, DenseMatrix[Float]]]
    .register(impl_OpMulMatrix_DM_DM_eq_DM_Float)

  private def transposeString(a: DenseMatrix[Float]): String = {
    if (a.isTranspose) "T" else "N"
  }

  implicit object implOpMulMatrix_DMF_DVF_eq_DVF
      extends OpMulMatrix.Impl2[DenseMatrix[Float], DenseVector[Float], DenseVector[Float]] {
    def apply(a: DenseMatrix[Float], b: DenseVector[Float]): DenseVector[Float] = {

      require(a.cols == b.length, "Dimension mismatch!")
      val rv = DenseVector.zeros[Float](a.rows)
      blas.sgemv(
        transposeString(a),
        if (a.isTranspose) a.cols else a.rows,
        if (a.isTranspose) a.rows else a.cols,
        1.0f,
        a.data,
        a.offset,
        a.majorStride,
        b.data,
        b.offset,
        b.stride,
        0.0f,
        rv.data,
        rv.offset,
        rv.stride
      )
      rv
    }
  }

  implicit val implOpMulMatrix_DVF_DMF_eq_DMF
    : OpMulMatrix.Impl2[DenseVector[Float], DenseMatrix[Float], DenseMatrix[Float]] = {
    new OpMulMatrix.Impl2[DenseVector[Float], DenseMatrix[Float], DenseMatrix[Float]] {
      def apply(a: DenseVector[Float], b: DenseMatrix[Float]): DenseMatrix[Float] = {
        require(b.rows == 1)
        //        val adata =  if(a.stride != 1) {
        //          val v = DenseVector.zeros[Float](a.length)
        //          v := a
        //          v.data
        //        } else {
        //          a.data
        //        }
        val rv: DenseMatrix[Float] = DenseMatrix.zeros[Float](a.length, b.cols)
        blas.sgemm(
          "T",
          transposeString(b),
          rv.rows,
          rv.cols,
          1,
          1.0f,
          a.data,
          a.offset,
          a.stride,
          b.data,
          b.offset,
          b.majorStride,
          0.0f,
          rv.data,
          0,
          rv.rows)
        rv

      }
    }
  }

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" // <editor-fold defaultstate="collapsed" desc=" OpSolveMatrixBy implementations ">

  implicit object implOpSolveMatrixBy_DMF_DMF_eq_DMF
      extends OpSolveMatrixBy.Impl2[DenseMatrix[Float], DenseMatrix[Float], DenseMatrix[Float]] {

    override def apply(A: DenseMatrix[Float], V: DenseMatrix[Float]) = {
      require(A.rows == V.rows, "Non-conformant matrix sizes")

      if (A.size == 0) {
        DenseMatrix.zeros[Float](0, 0)
      } else if (A.rows == A.cols) {
        // square: LUSolve
        val X = DenseMatrix.zeros[Float](V.rows, V.cols)
        X := V
        LUSolve(X, A)
        X
      } else {
        // non-square: QRSolve
        val X = DenseMatrix.zeros[Float](A.cols, V.cols)
        QRSolve(X, A, V)
        X
      }
    }

    /** X := A \ X, for square A */
    def LUSolve(X: DenseMatrix[Float], A: DenseMatrix[Float]): DenseMatrix[Float] = {

      require(X.offset == 0)
      require(A.offset == 0)
      val piv = new Array[Int](A.rows)
      val newA: DenseMatrix[Float] = A.copy
      assert(!newA.isTranspose)

      val info: Int = {
        val info = new intW(0)
        lapack.sgesv(A.rows, X.cols, newA.data, newA.majorStride, piv, X.data, X.majorStride, info)
        info.`val`
      }

      if (info > 0)
        throw new MatrixSingularException()
      else if (info < 0)
        throw new IllegalArgumentException()

      X
    }

    /** X := A \ V, for arbitrary A */
    def QRSolve(X: DenseMatrix[Float], A: DenseMatrix[Float], V: DenseMatrix[Float]): DenseMatrix[Float] = {

      require(X.offset == 0)
      require(A.offset == 0)
      require(V.offset == 0)
      require(X.rows == A.cols, "Wrong number of rows in return value")
      require(X.cols == V.cols, "Wrong number of rows in return value")

      val transpose: Boolean = X.isTranspose

      val nrhs = V.cols

      // allocate temporary solution matrix
      val Xtmp = DenseMatrix.zeros[Float](math.max(A.rows, A.cols), nrhs)
      val M = if (!transpose) A.rows else A.cols
      Xtmp(0 until M, 0 until nrhs) := V(0 until M, 0 until nrhs)

      val newData: Array[Float] = A.data.clone()

      // query optimal workspace
      val queryWork = new Array[Float](1)
      val queryInfo = new intW(0)
      lapack.sgels(
        if (!transpose) "N" else "T",
        A.rows,
        A.cols,
        nrhs,
        newData,
        A.majorStride,
        Xtmp.data,
        math.max(1, math.max(A.rows, A.cols)),
        queryWork,
        -1,
        queryInfo)

      // allocate workspace
      val work: Array[Float] = {
        val lwork: Int = {
          if (queryInfo.`val` != 0)
            math.max(1, math.min(A.rows, A.cols) + math.max(math.min(A.rows, A.cols), nrhs))
          else
            math.max(queryWork(0).toInt, 1)
        }
        new Array[Float](lwork)
      }

      // compute factorization
      val info = new intW(0)
      lapack.sgels(
        if (!transpose) "N" else "T",
        A.rows,
        A.cols,
        nrhs,
        newData,
        A.majorStride,
        Xtmp.data,
        math.max(1, math.max(A.rows, A.cols)),
        work,
        work.length,
        info)

      if (info.`val` < 0)
        throw new IllegalArgumentException

      // extract solution
      val N = if (!transpose) A.cols else A.rows
      X(0 until N, 0 until nrhs) := Xtmp(0 until N, 0 until nrhs)

      X
    }
  }

  implicit object implOpSolveMatrixBy_DMF_DVF_eq_DVF
      extends OpSolveMatrixBy.Impl2[DenseMatrix[Float], DenseVector[Float], DenseVector[Float]] {

    override def apply(a: DenseMatrix[Float], b: DenseVector[Float]): DenseVector[Float] = {
      val rv: DenseMatrix[Float] = a \ new DenseMatrix[Float](b.size, 1, b.data, b.offset, b.stride, true)
      new DenseVector[Float](rv.data)
    }
  }

  // </editor-fold>
}

trait DenseMatrixExpandedOps extends MatrixOps with DenseMatrix_GenericOps with DenseMatrix_TraversalOps {

  // <editor-fold defaultstate="collapsed" desc=" implicit implementations for OpXXX.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]] ">
  // don't remove

  @expand
  @expand.valify
  implicit def dm_dm_UpdateOp[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](implicit
          @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) })
          op: Op.Impl2[T, T, T],
          @expand.sequence[Op]({ _ += _ }, { _ -= _ }, { _ :*= _ }, { _ :/= _ }, { _ := _ }, { _ %= _ }, { _ :^= _ })
          vecOp: Op.Impl2[T, T, T]): Op.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]] = {

    new Op.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]] {
      def apply(a: DenseMatrix[T], b: DenseMatrix[T]): Unit = {
        require(a.rows == b.rows, "Row dimension mismatch!")
        require(a.cols == b.cols, "Col dimension mismatch!")

        if ((a ne b) && a.overlaps(b)) {
          val ac = a.copy
          apply(ac, b)
          a := ac
          // gives a roughly 5-10x speedup
          // if a and b are both nicely and identically shaped, add them as though they were vectors
        } else if (a.isTranspose == b.isTranspose && a.isContiguous && b.isContiguous) {
          vecOp(new DenseVector(a.data, a.offset, 1, a.size), new DenseVector(b.data, b.offset, 1, b.size))
        } else {
          slowPath(a, b)
        }

      }

      private def slowPath(a: DenseMatrix[T], b: DenseMatrix[T]): Unit = {
        if (a.isTranspose) {
          apply(a.t, b.t)
        } else {
          val ad = a.data
          val bd = b.data
          cforRange2 (0 until a.cols, 0 until a.rows) { (c, r) =>
            ad(a.linearIndex(r, c)) = op(ad(a.linearIndex(r, c)), bd(b.linearIndex(r, c)))
          }
        }
      }

      implicitly[BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type]].register(this)
    }
  }

  @expand
  implicit def dm_dm_UpdateOp[
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpMod, OpPow) Op <: OpType,
      T: Field: Zero: ClassTag](
      implicit @expand.sequence[Op]({ f.+(_, _) }, { f.-(_, _) }, { f.*(_, _) }, { f./(_, _) }, { f.%(_, _) }, {
        f.pow(_, _)
      }) op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]] = {
    val f = implicitly[Field[T]]
    new Op.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]] {
      def apply(a: DenseMatrix[T], b: DenseMatrix[T]): Unit = {
        val ad = a.data
        val bd = b.data

        if ((a ne b) && a.overlaps(b)) {
          val ac = a.copy
          apply(ac, b)
          a := ac
        } else {
          cforRange2 (0 until a.cols, 0 until a.rows) { (c, r) =>
            ad(a.linearIndex(r, c)) = op(ad(a.linearIndex(r, c)), bd(b.linearIndex(r, c)))
          }
        }

      }

      implicitly[BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type]].register(this)
    }
  }
  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" implicit implementations for OpXXX.InPlaceImpl2[DenseMatrix[T], T] ">

  @expand
  @expand.valify
  implicit def dm_s_UpdateOp[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) }) op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[DenseMatrix[T], T] =
    new Op.InPlaceImpl2[DenseMatrix[T], T] {
      def apply(a: DenseMatrix[T], b: T): Unit = {

        if (a.isContiguous) {
          fastPath(a, b)
        } else {
          slowPath(a, b)
        }

      }

      def fastPath(a: DenseMatrix[T], b: T): Unit = {
        val ad: Array[T] = a.data
        cforRange(a.offset until (a.offset + a.size)) { i =>
          ad(i) = op(ad(i), b)
        }
      }

      def slowPath(a: DenseMatrix[T], b: T): Unit = {
        val ad = a.data
        if (!a.isTranspose) {
          cforRange2(0 until a.cols, 0 until a.rows) { (c, r) =>
            ad(a.linearIndex(r, c)) = op(ad(a.linearIndex(r, c)), b)
          }
        } else {
          cforRange2(0 until a.rows, 0 until a.cols) { (r, c) =>
            ad(a.linearIndex(r, c)) = op(ad(a.linearIndex(r, c)), b)
          }
        }
      }

      implicitly[BinaryUpdateRegistry[Matrix[T], T, Op.type]].register(this)
    }

  @expand
  implicit def opUpdate_DM_S[
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpMod, OpPow) Op <: OpType,
      T: Field: Zero: ClassTag](
      implicit @expand.sequence[Op]({ f.+(_, _) }, { f.-(_, _) }, { f.*(_, _) }, { f.*(_, _) }, { f./(_, _) }, {
        f.%(_, _)
      }, { f.pow(_, _) }) op: Op.Impl2[T, T, T]): Op.InPlaceImpl2[DenseMatrix[T], T] = {
    val f = implicitly[Field[T]]
    new Op.InPlaceImpl2[DenseMatrix[T], T] {
      override def apply(a: DenseMatrix[T], b: T) = {
        val ad: Array[T] = a.data

        cforRange2 (0 until a.cols, 0 until a.rows) { (c, r) =>
          ad(a.linearIndex(r, c)) = op(ad(a.linearIndex(r, c)), b)
        }
      }
      implicitly[BinaryUpdateRegistry[Matrix[T], T, Op.type]].register(this)
    }
  }
  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" implicit implementations for OpXXX.Impl2[DenseMatrix[T], T, DenseMatrix[T]] ">

  @expand
  @expand.valify
  implicit def op_DM_S[
      @expand.args(Int, Long, Float, Double) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpMod, OpDiv, OpPow) Op]
    : Op.Impl2[DenseMatrix[T], T, DenseMatrix[T]] = {
    val uop = implicitly[Op.InPlaceImpl2[DenseMatrix[T], T]]
    new Op.Impl2[DenseMatrix[T], T, DenseMatrix[T]] {
      override def apply(a: DenseMatrix[T], b: T) = {
        val c = copy(a)
        uop(c, b)
        c
      }
      implicitly[BinaryRegistry[Matrix[T], T, Op.type, Matrix[T]]].register(this)
    }
  }

  @expand
  implicit def op_DM_S[
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpMod, OpPow) Op <: OpType,
      T: Field: Zero: ClassTag]:
//  (implicit @expand.sequence[Op]({f.+(_,_)}, {f.-(_,_)}, {f.*(_,_)}, {f.*(_,_)}, {f./(_,_)}, {f.%(_,_)},{f.pow(_,_)}) op: Op.Impl2[T,T,T]):
  Op.Impl2[DenseMatrix[T], T, DenseMatrix[T]] = {
    val uop = implicitly[Op.InPlaceImpl2[DenseMatrix[T], T]]
    new Op.Impl2[DenseMatrix[T], T, DenseMatrix[T]] {
      override def apply(a: DenseMatrix[T], b: T) = {
        val c = copy(a)
        uop(c, b)
        c
      }
      implicitly[BinaryRegistry[Matrix[T], T, Op.type, Matrix[T]]].register(this)
    }
  }

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" implicit implementations for OpXXX.Impl2[T, DenseMatrix[T], DenseMatrix[T]] ">

  @expand
  @expand.valify
  implicit def s_dm_op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpMod, OpPow) Op <: OpType](implicit @expand.sequence[
    Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ * _ }, { _ / _ }, { _ % _ }, { _.pow(_) }) op: Op.Impl2[T, T, T])
    : Op.Impl2[T, DenseMatrix[T], DenseMatrix[T]] =
    new Op.Impl2[T, DenseMatrix[T], DenseMatrix[T]] {
      def apply(b: T, a: DenseMatrix[T]): DenseMatrix[T] = {
        val res: DenseMatrix[T] = DenseMatrix.zeros[T](a.rows, a.cols)
        val resd: Array[T] = res.data
        val ad: Array[T] = a.data
        var off = 0

        cforRange2 (0 until a.cols, 0 until a.rows) { (c, r) =>
          resd(off) = op(b, ad(a.linearIndex(r, c)))
          off += 1
        }

        res
      }

      implicitly[BinaryRegistry[T, Matrix[T], Op.type, Matrix[T]]].register(this)
    }

  implicit def s_dm_op[T, Op <: OpType, U](
      implicit opScalar: UFunc.UImpl2[Op, T, T, U],
      ct: ClassTag[U],
      zero: Zero[U]): UFunc.UImpl2[Op, T, DenseMatrix[T], DenseMatrix[U]] = {
    new UFunc.UImpl2[Op, T, DenseMatrix[T], DenseMatrix[U]] {
      def apply(b: T, a: DenseMatrix[T]): DenseMatrix[U] = {
        val res: DenseMatrix[U] = DenseMatrix.zeros[U](a.rows, a.cols)
        val resd: Array[U] = res.data
        val ad: Array[T] = a.data
        var off = 0

        cforRange2 (0 until a.cols, 0 until a.rows) { (c,r) =>
          resd(off) = opScalar(b, ad(a.linearIndex(r, c)))
          off += 1
        }

        res
      }
    }
  }

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" implicit implementations for OpXXX.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] ">

  @expand
  @expand.valify
  implicit def op_DM_DM[
      @expand.args(Int, Long, Float, Double) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpMod, OpDiv, OpPow) Op]
    : Op.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] = {
    val uop = implicitly[Op.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]]]
    new Op.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
      override def apply(a: DenseMatrix[T], b: DenseMatrix[T]): DenseMatrix[T] = {
        val c = copy(a)
        uop(c, b)

        c
      }

      implicitly[BinaryRegistry[Matrix[T], Matrix[T], Op.type, Matrix[T]]].register(this)
    }
  }

  // </editor-fold>

}

trait DenseMatrixOpsLowPrio extends MatrixOps {
}

trait DenseMatrixMultOps extends DenseMatrixExpandedOps with DenseMatrixOpsLowPrio {
  // <editor-fold defaultstate="collapsed" desc=" implicit implementations for BinaryRegistry ">

  @expand
  @expand.valify
  implicit def impl_OpMulMatrix_DM_V_eq_DV[@expand.args(Int, Long, Float, Double) T]
    : BinaryRegistry[DenseMatrix[T], Vector[T], OpMulMatrix.type, DenseVector[T]] =
    new BinaryRegistry[DenseMatrix[T], Vector[T], OpMulMatrix.type, DenseVector[T]] {
      override def bindingMissing(a: DenseMatrix[T], b: Vector[T]): DenseVector[T] = {

        // TODO: this could probably be much faster?
        require(a.cols == b.length)
        val res: DenseVector[T] = DenseVector.zeros[T](a.rows)
        cforRange2 (0 until a.cols, 0 until a.rows) { (c, r) =>
          val v = a(r, c)
          res(r) += v * b(c)
        }

        res
      }

      implicitly[BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix.type, Vector[T]]].register(this)
    }

  @expand
  @expand.valify
  implicit def impl_OpMulMatrix_DM_M_eq_DM[@expand.args(Int, Long, Float, Double) T]
    : BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix.type, DenseMatrix[T]] =
    new BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix.type, DenseMatrix[T]] {
      override def bindingMissing(a: DenseMatrix[T], b: Matrix[T]): DenseMatrix[T] = {
        // Martin Senne:
        // Accessing consequent areas in memory in the innermost loop ( a(i,l), a(i+1,l) ) is faster
        // than accessing ( b(c, j), b(c, j+1) as data layout in memory is column-like (Fortran), that is a(0,0), a(1,0), a(2,0), ...
        // Thus (adapted from dgemm in BLAS):
        //   - exchanged loop order
        //   - so to access consequent entries in the innermost loop and to hopefully avoid cache-misses
        val res: DenseMatrix[T] = DenseMatrix.zeros[T](a.rows, b.cols)
        require(a.cols == b.rows)

        val colsB = b.cols
        val colsA = a.cols
        val rowsA = a.rows
        // TODO: optimize in the case of isTranspose
        // TODO: consider a block impl

        cforRange (0 until colsB) { j =>
          cforRange (0 until colsA) { l =>
            val v = b(l, j)
            cforRange (0 until rowsA) { i =>
              res(i, j) += v * a(i, l)
            }
          }
        }
        res
      }

      implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]]].register(this)
    }

  // </editor-fold>

  // <editor-fold defaultstate="collapsed" desc=" implicit implementations for OpMulMatrix ">

  @expand
  @expand.valify
  implicit def impl_OpMulMatrix_DM_DM_eq_DM[@expand.args(Int, Long) T]
    : OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] =
    new OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
      // amazingly, the bigger these are, the better.
      val blockSizeRow = 2000
      val blockSizeInner = 2000
      val blockSizeCol = 2000

      def multBlock(
          M: Int,
          N: Int,
          K: Int,
          aTrans: Array[T],
          b: Array[T],
          res: DenseMatrix[T],
          resRowOff: Int,
          resColOff: Int): Unit = {
        val rd = res.data
        val rOff = res.offset + resRowOff + resColOff * res.majorStride

        cforRange2(0 until M, 0 until K) { (i, k) =>
          var sum: T = 0
          cforRange(0 until N) { (j) =>
            sum += aTrans(i * N + j) * b(k * N + j)
          }
          rd(rOff + i + k * res.majorStride) += sum
        }
      }

      override def apply(a: DenseMatrix[T], b: DenseMatrix[T]): DenseMatrix[T] = {
        val res: DenseMatrix[T] = DenseMatrix.zeros[T](a.rows, b.cols)
        require(a.cols == b.rows)

        val aTrans = new Array[T](math.min(blockSizeRow * blockSizeInner, a.size))
        val bBuf = new Array[T](math.min(blockSizeInner * blockSizeCol, b.size))

        val numRowBlocks = (a.rows + blockSizeRow - 1) / blockSizeRow
        val numInnerBlocks = (a.cols + blockSizeCol - 1) / blockSizeCol
        val numColBlocks = (b.cols + blockSizeInner - 1) / blockSizeInner

        cforRange(0 until numRowBlocks) { r =>
          val mBegin = r * blockSizeRow
          val mEnd = math.min(mBegin + blockSizeRow, a.rows)
          val M = mEnd - mBegin
          cforRange(0 until numInnerBlocks) { i =>
            val nBegin = i * blockSizeInner
            val nEnd = math.min(nBegin + blockSizeInner, a.cols)
            val N = nEnd - nBegin

            if (a.isTranspose) {
              cforRange(0 until M) { m =>
                System.arraycopy(a.data, a.offset + (m + mBegin) * a.majorStride + nBegin, aTrans, m * N, N)
              }
            } else {
              cforRange2(0 until N, 0 until M) { (n, m) =>
                aTrans(m * N + n) = a(m + mBegin, n + nBegin)
              }
            }

            cforRange(0 until numColBlocks) { c =>
              val oBegin = c * blockSizeCol
              val oEnd = math.min(oBegin + blockSizeCol, b.cols)
              val O = oEnd - oBegin

              cforRange2(0 until O, 0 until N) { (o, n) =>
                bBuf(o * N + n) = b(n + nBegin, o + oBegin)
              }

              multBlock(M, N, O, aTrans, bBuf, res, mBegin, oBegin)

            }
          }
        }

        res
      }
      implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]]].register(this)
      implicitly[BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix.type, DenseMatrix[T]]].register(this)
    }

  // </editor-fold>

}

trait DenseMatrix_SliceOps extends DenseMatrix_SliceOps_LowPrio {


  // <editor-fold defaultstate="collapsed" desc=" implicit implementations for OpSet ">

  class SetDMDMOp[@spec(Double, Int, Float, Long) V] extends OpSet.InPlaceImpl2[DenseMatrix[V], DenseMatrix[V]] {

    def apply(a: DenseMatrix[V], b: DenseMatrix[V]): Unit = {
      require(a.rows == b.rows, "Matrixs must have same number of rows")
      require(a.cols == b.cols, "Matrixs must have same number of columns")

      if (a.isTranspose == b.isTranspose && a.isContiguous && b.isContiguous) {
        System.arraycopy(b.data, b.offset, a.data, a.offset, a.size)
      } else if (a.isTranspose == b.isTranspose) {
        cforRange(0 until a.majorSize) { j =>
          System.arraycopy(b.data, b.offset + j * b.majorStride, a.data, a.offset + j * a.majorStride, a.minorSize)
        }
      } else {
        cacheObliviousTranspose(0, a.majorSize, 0, b.majorSize, a.data, a.offset, a.majorStride, b.data, b.offset, b.majorStride)
      }
    }

    def cacheObliviousTranspose(
        rBegin: Int,
        rEnd: Int,
        cBegin: Int,
        cEnd: Int,
        dst: Array[V],
        dstOff: Int,
        aMajorStride: Int,
        src: Array[V],
        srcOff: Int,
        bMajorStride: Int): Unit = {
      val r = rEnd - rBegin
      val c = cEnd - cBegin
      if (r <= 16 && c <= 16) {
        cforRange2(rBegin until rEnd, cBegin until cEnd) { (j, i) =>
          dst(dstOff + j * aMajorStride + i) = src(srcOff + i * bMajorStride + j)
        }
      } else if (r >= c) {
        cacheObliviousTranspose(rBegin, rBegin + (r / 2), cBegin, cEnd, dst, dstOff, aMajorStride, src, srcOff, bMajorStride)
        cacheObliviousTranspose(rBegin + (r / 2), rEnd, cBegin, cEnd, dst, dstOff,aMajorStride, src, srcOff,bMajorStride)
      } else {
        cacheObliviousTranspose(rBegin, rEnd, cBegin, cBegin + (c / 2), dst, dstOff,aMajorStride, src, srcOff,bMajorStride)
        cacheObliviousTranspose(rBegin, rEnd, cBegin + (c / 2), cEnd, dst, dstOff,aMajorStride, src, srcOff,bMajorStride)
      }
    }
  }

  class SetDMDVOp[@spec(Double, Int, Float, Long) V] extends OpSet.InPlaceImpl2[DenseMatrix[V], DenseVector[V]] {

    def apply(a: DenseMatrix[V], b: DenseVector[V]): Unit = {
      require(
        a.rows == b.length && a.cols == 1 || a.cols == b.length && a.rows == 1,
        "DenseMatrix must have same number of rows, or same number of columns, as DenseVector, and the other dim must be 1."
      )

      val ad: Array[V] = a.data
      val bd: Array[V] = b.data
      var boff = b.offset
      cforRange2 (0 until a.cols, 0 until a.rows) { (c, r) =>
        ad(a.linearIndex(r, c)) = bd(boff)
        boff += b.stride
      }
    }
  }

  class SetMSOp[@spec(Double, Int, Float, Long) V] extends OpSet.InPlaceImpl2[DenseMatrix[V], V] {

    def apply(a: DenseMatrix[V], b: V): Unit = {
      if (a.isContiguous) {
        ArrayUtil.fill(a.data, a.offset, a.size, b)
      } else {
        slowPath(a, b)
      }
    }

    private def slowPath(a: DenseMatrix[V], b: V): Unit = {
      // slow path when we don't have a trivial matrix
      val ad: Array[V] = a.data
      var aoff = a.offset
      cforRange(0 until a.majorSize) { big =>
        cforRange(0 until a.minorSize) { little =>
          ad(aoff + little) = b
        }
        aoff += a.majorStride
      }
    }
  }

  implicit def setDMDM[V]: OpSet.InPlaceImpl2[DenseMatrix[V], DenseMatrix[V]] = new SetDMDMOp[V]
  implicit def setDMDV[V]: OpSet.InPlaceImpl2[DenseMatrix[V], DenseVector[V]] = new SetDMDVOp[V]
  implicit def setDMS[V]: OpSet.InPlaceImpl2[DenseMatrix[V], V] = new SetMSOp[V]

  // </editor-fold>

}

trait LowPriorityDenseMatrix1 {

  /**
   * Returns a 1xnumCols DenseMatrix
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def canCollapseRows[V, R: ClassTag: Zero]
    : CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], R, Transpose[DenseVector[R]]] =
    new CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], R, Transpose[DenseVector[R]]] {
      def apply(from: DenseMatrix[V], axis: Axis._0.type)(f: (DenseVector[V]) => R): Transpose[DenseVector[R]] = {
        val result = DenseVector.zeros[R](from.cols)
        cforRange(0 until from.cols) { c =>
          result(c) = f(from(::, c))
        }
        result.t
      }
    }

  /**
   * Returns a numRows DenseVector
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def canCollapseCols[V, R: ClassTag: Zero]: CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], R, DenseVector[R]] =
    new CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], R, DenseVector[R]] {
      def apply(from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => R): DenseVector[R] = {
        val result = DenseVector.zeros[R](from.rows)
        val t = from.t
        cforRange(0 until from.rows) { r =>
          result(r) = f(t(::, r))
        }
        result
      }
    }

  // <editor-fold defaultstate="collapsed" desc=" implicit implementations for OpSet ">

  implicit def impl_OpSet_InPlace_DM_M[@spec(Double, Int, Float, Long) V]: OpSet.InPlaceImpl2[DenseMatrix[V], Matrix[V]] = {
    (a: DenseMatrix[V], b: Matrix[V]) => {
      require(a.rows == b.rows, "Matrixs must have same number of rows")
      require(a.cols == b.cols, "Matrixs must have same number of columns")

      // TODO: detect b is a DM and dispatch
      // TODO: check isTranspose?

      val ad = a.data
      cforRange (0 until a.cols) { c =>
        cforRange (0 until a.rows) { r =>
          ad(a.linearIndex(r, c)) = b(r, c)
        }
      }
    }
  }

  // TODO: I don't like this op. remove?
  implicit def impl_OpSet_InPlace_DM_V[@specialized(Int, Float, Long, Double) V]: OpSet.InPlaceImpl2[DenseMatrix[V], Vector[V]] = {
      (a: DenseMatrix[V], b: Vector[V]) => {
        require(
          a.rows == b.length && a.cols == 1 || a.cols == b.length && a.rows == 1,
          "DenseMatrix must have same number of rows, or same number of columns, as DenseVector, and the other dim must be 1."
        )
        val ad = a.data
        var i = 0
        var c = 0
        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            ad(a.linearIndex(r, c)) = b(i)
            r += 1
            i += 1
          }
          c += 1
        }
      }
  }

  // </editor-fold>

}

/**
 * Operators for comparisons among elements of a DenseMatrix
 *
 * @author dlwh
 **/
trait DenseMatrix_ComparisonOps extends DenseMatrixExpandedOps {

  @expand
  implicit def impl_Op_DM_DM_eq_DMBool[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpGT, OpGTE, OpLTE, OpLT, OpEq, OpNe) Op <: OpType](
      implicit @expand.sequence[Op]({ _ > _ }, { _ >= _ }, { _ <= _ }, { _ < _ }, { _ == _ }, { _ != _ })
      op: Op.Impl2[T, T, T]): Op.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[Boolean]] =
    new Op.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[Boolean]] {
      def apply(a: DenseMatrix[T], b: DenseMatrix[T]): DenseMatrix[Boolean] = {
        if (a.isTranspose) {
          apply(a.t, b.t).t
        } else {
          if (a.rows != b.rows)
            throw new ArrayIndexOutOfBoundsException(s"Rows don't match for operator $Op ${a.rows} ${b.rows}")
          if (a.cols != b.cols)
            throw new ArrayIndexOutOfBoundsException(s"Cols don't match for operator $Op ${a.cols} ${b.cols}")
          val result = DenseMatrix.zeros[Boolean](a.rows, a.cols)

          cforRange2(0 until a.cols, 0 until a.rows) { (j, i) =>
            result(i, j) = op(a(i, j), b(i, j))
          }

          result
        }
      }
    }

  @expand
  implicit def impl_Op_DM_M_eq_DMBool_Comparison[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpGT, OpGTE, OpLTE, OpLT, OpEq, OpNe) Op <: OpType](
      implicit @expand.sequence[Op]({ _ > _ }, { _ >= _ }, { _ <= _ }, { _ < _ }, { _ == _ }, { _ != _ })
      op: Op.Impl2[T, T, Boolean]): Op.Impl2[DenseMatrix[T], Matrix[T], DenseMatrix[Boolean]] =
    new Op.Impl2[DenseMatrix[T], Matrix[T], DenseMatrix[Boolean]] {
      def apply(a: DenseMatrix[T], b: Matrix[T]): DenseMatrix[Boolean] = {
        if (a.rows != b.rows)
          throw new ArrayIndexOutOfBoundsException(s"Rows don't match for operator $Op ${a.rows} ${b.rows}")
        if (a.cols != b.cols)
          throw new ArrayIndexOutOfBoundsException(s"Cols don't match for operator $Op ${a.cols} ${b.cols}")
        val result = DenseMatrix.zeros[Boolean](a.rows, a.cols)

        cforRange2(0 until a.cols, 0 until a.rows) { (j, i) =>
          result(i, j) = op(a(i, j), b(i, j))
        }

        result
      }
    }

  @expand
  implicit def impl_Op_DM_S_eq_DMBool_Comparison[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpGT, OpGTE, OpLTE, OpLT, OpEq, OpNe) Op <: OpType](
      implicit @expand.sequence[Op]({ _ > _ }, { _ >= _ }, { _ <= _ }, { _ < _ }, { _ == _ }, { _ != _ })
      op: Op.Impl2[T, T, Boolean]): Op.Impl2[DenseMatrix[T], T, DenseMatrix[Boolean]] =
    new Op.Impl2[DenseMatrix[T], T, DenseMatrix[Boolean]] {
      def apply(a: DenseMatrix[T], b: T): DenseMatrix[Boolean] = {
        if (a.isTranspose) {
          apply(a.t, b).t
        } else {
          val result = DenseMatrix.zeros[Boolean](a.rows, a.cols)

          cforRange2 (0 until a.cols, 0 until a.rows) { (j, i) =>
            result(i, j) = op(a(i, j), b)
          }

          result
        }
      }
    }

}
