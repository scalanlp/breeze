package breeze.linalg.operators

import com.github.fommil.netlib.BLAS.{getInstance=>blas}
import breeze.linalg._
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}
import breeze.macros.expand
import breeze.math.Complex
import scala.math.BigInt
import breeze.generic.UFunc2ZippingImplicits
import breeze.linalg.support.{CanCollapseAxis, CanSlice2}
import breeze.util.ArrayUtil
import breeze.storage.DefaultArrayValue
import scala.reflect.ClassTag

trait DenseMatrixMultiplyStuff extends DenseMatrixOps with DenseMatrixMultOps { this: DenseMatrix.type =>
  implicit object DenseMatrixDMulDenseMatrixD
    extends OpMulMatrix.Impl2[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] {
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
    extends OpMulMatrix.Impl2[DenseMatrix[Double], DenseVector[Double], DenseVector[Double]] {
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
    extends OpSolveMatrixBy.Impl2[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] {
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

  implicit object DenseMatrixCanSolveDenseVector extends OpSolveMatrixBy.Impl2[DenseMatrix[Double], DenseVector[Double], DenseVector[Double]] {
    override def apply(a : DenseMatrix[Double], b : DenseVector[Double]) = {
      val rv = a \ new DenseMatrix[Double](b.size, 1, b.data, b.offset, b.stride, true)
      new DenseVector[Double](rv.data)
    }
  }



  implicit val mulDVDM: OpMulMatrix.Impl2[DenseVector[Double], DenseMatrix[Double], DenseMatrix[Double]] = {
    new OpMulMatrix.Impl2[DenseVector[Double], DenseMatrix[Double], DenseMatrix[Double]] {
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
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):Op.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]] = new Op.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]] {
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
    implicitly[BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type]].register(this)
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dm_s_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):Op.InPlaceImpl2[DenseMatrix[T], T] = new Op.InPlaceImpl2[DenseMatrix[T], T] {
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
    implicitly[BinaryUpdateRegistry[Matrix[T], T, Op.type]].register(this)
  }


  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def op_DM_S[@expand.args(Int, Long, Float, Double, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpMod, OpDiv, OpPow) Op]: Op.Impl2[DenseMatrix[T], T, DenseMatrix[T]] = {
    val uop = implicitly[Op.InPlaceImpl2[DenseMatrix[T], T]]
    new Op.Impl2[DenseMatrix[T], T, DenseMatrix[T]] {
      override def apply(a : DenseMatrix[T], b: T) = {
        val c = copy(a)
        uop(c, b)
        c
      }
      implicitly[BinaryRegistry[Matrix[T], T, Op.type, Matrix[T]]].register(this)
    }
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def s_dm_op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):Op.Impl2[T, DenseMatrix[T], DenseMatrix[T]] = new Op.Impl2[T, DenseMatrix[T], DenseMatrix[T]] {
    def apply(b: T, a: DenseMatrix[T]) = {
      val res = DenseMatrix.zeros[T](a.rows,a.cols)
      val resd = res.data
      val ad = a.data
      var c = 0

      var off = 0
      while(c < a.cols) {
        var r = 0
        while(r < a.rows) {
          resd(off) = op(ad(a.linearIndex(r,c)), b)
          r += 1
          off += 1
        }
        c += 1
      }

      res
    }
    implicitly[BinaryRegistry[T, Matrix[T], Op.type, Matrix[T]]].register(this)
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def op_DM_DM[@expand.args(Int, Long, Float, Double, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMod, OpDiv, OpPow) Op]: Op.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] = {
    val uop = implicitly[Op.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]]]
    new Op.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
      override def apply(a : DenseMatrix[T], b: DenseMatrix[T]) = {
        val c = copy(a)
        uop(c, b)
        c
      }
      implicitly[BinaryRegistry[Matrix[T], Matrix[T], Op.type, Matrix[T]]].register(this)
    }
  }

}

trait DenseMatrixOpsLowPrio extends UFunc2ZippingImplicits[DenseMatrix] { this: DenseMatrixOps =>
  // LOL, if we explicitly annotate the type, then the implicit resolution thing will load this recursively.
  // If we don't, then everything works ok.
  @expand
  implicit def canMulM_V_def[@expand.args(Int, Float, Double, Long, Complex, BigInt) T, A, B](implicit bb :  B <:< Vector[T]) = (
    implicitly[OpMulMatrix.Impl2[DenseMatrix[T], Vector[T], DenseVector[T]]].asInstanceOf[breeze.linalg.operators.OpMulMatrix.Impl2[A, B, DenseVector[T]]]
    )

  // ibid.
  @expand
  implicit def canMulM_M_def[@expand.args(Int, Float, Double, Long, Complex, BigInt) T, B](implicit bb :  B <:< Matrix[T]) = (
    implicitly[OpMulMatrix.Impl2[DenseMatrix[T], Matrix[T], DenseMatrix[T]]].asInstanceOf[OpMulMatrix.Impl2[DenseMatrix[T], B, DenseMatrix[T]]]
    )
}

trait DenseMatrixMultOps extends DenseMatrixOps with DenseMatrixOpsLowPrio { this: DenseMatrix.type =>
  // I don't know why this import is necessary to make the DefaultArrayValue do the right thing.
  // If I remove the import breeze.storage.DefaultArrayValue._, everything breaks, for some reason.
  import breeze.math.Complex.ComplexDefaultArrayValue

  @expand
  @expand.valify
  implicit def op_DM_V[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]:BinaryRegistry[DenseMatrix[T], Vector[T], OpMulMatrix.type, DenseVector[T]] = new BinaryRegistry[DenseMatrix[T], Vector[T], OpMulMatrix.type, DenseVector[T]] {
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
    implicitly[BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix.type, Vector[T]]].register(this)
  }


  @expand
  @expand.valify
  implicit def op_DM_M[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]:BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix.type, DenseMatrix[T]] = new BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix.type, DenseMatrix[T]] {
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
    implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]]].register(this)
  }



  @expand
  @expand.valify
  implicit def op_DM_DM[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]:OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] = new OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
    implicitly[BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix.type, DenseMatrix[T]]].register(this)
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
    implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]]].register(this)
    implicitly[BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix.type, DenseMatrix[T]]].register(this)
  }

}

trait LowPriorityDenseMatrix extends LowPriorityDenseMatrix1 {

  implicit def canSliceWeirdRows[V]:CanSlice2[DenseMatrix[V], Seq[Int], ::.type, SliceMatrix[Int, Int, V]] = {
    new CanSlice2[DenseMatrix[V], Seq[Int], ::.type, SliceMatrix[Int, Int, V]] {
      def apply(from: DenseMatrix[V], slice: Seq[Int], slice2: ::.type): SliceMatrix[Int, Int, V] = {
        new SliceMatrix(from, slice.toIndexedSeq, (0 until from.cols))
      }
    }
  }

  class SetDMDMOp[@specialized(Int, Double, Float) V] extends OpSet.InPlaceImpl2[DenseMatrix[V], DenseMatrix[V]] {
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

  class SetDMDVOp[@specialized(Int, Double, Float) V] extends OpSet.InPlaceImpl2[DenseMatrix[V], DenseVector[V]] {
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


  class SetMSOp[@specialized(Int, Double, Float) V] extends OpSet.InPlaceImpl2[DenseMatrix[V], V] {
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

  implicit def setDMDM[V]: OpSet.InPlaceImpl2[DenseMatrix[V], DenseMatrix[V]] = new SetDMDMOp[V]
  implicit def setDMDV[V]: OpSet.InPlaceImpl2[DenseMatrix[V], DenseVector[V]] = new SetDMDVOp[V]
  implicit def setDMS[V]: OpSet.InPlaceImpl2[DenseMatrix[V], V] = new SetMSOp[V]
}

trait LowPriorityDenseMatrix1 {
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


  class SetMMOp[@specialized(Int, Double, Float) V] extends OpSet.InPlaceImpl2[DenseMatrix[V], Matrix[V]] {
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



  class SetDMVOp[@specialized(Int, Double, Float) V] extends OpSet.InPlaceImpl2[DenseMatrix[V], Vector[V]] {
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

  implicit def setMM[V]: OpSet.InPlaceImpl2[DenseMatrix[V], Matrix[V]] = new SetMMOp[V]
  implicit def setMV[V]: OpSet.InPlaceImpl2[DenseMatrix[V], Vector[V]] = new SetDMVOp[V]
}
