package breeze.linalg.operators

import com.github.fommil.netlib.BLAS.{getInstance=>blas}
import breeze.linalg._
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}
import breeze.macros.expand
import breeze.math.{Semiring, Complex}
import scala.math.BigInt
import breeze.linalg.support.{CanCollapseAxis, CanSlice2}
import breeze.util.ArrayUtil
import breeze.storage.DefaultArrayValue
import scala.reflect.ClassTag

trait DenseMatrixMultiplyStuff extends DenseMatrixOps with DenseMatrixMultOps { this: DenseMatrix.type =>

  implicit def dvTransTimesDM[T](implicit op:  OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]]):
  OpMulMatrix.Impl2[Transpose[DenseVector[T]], DenseMatrix[T], Transpose[DenseVector[T]]] = {
    new OpMulMatrix.Impl2[Transpose[DenseVector[T]], DenseMatrix[T], Transpose[DenseVector[T]]] {
      override def apply(v: Transpose[DenseVector[T]], v2: DenseMatrix[T]): Transpose[DenseVector[T]] = {
        (v.inner.asDenseMatrix * v2) apply (0, ::)
      }
    }
  }


  implicit object DenseMatrixDMulDenseMatrixD
    extends OpMulMatrix.Impl2[DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]] {
    def apply(_a : DenseMatrix[Double], _b : DenseMatrix[Double]): DenseMatrix[Double] = {
      require(_a.cols == _b.rows, "Dimension mismatch!")
      val rv: DenseMatrix[Double] = DenseMatrix.zeros[Double](_a.rows, _b.cols)

      if(_a.rows == 0 || _b.rows == 0 || _a.cols == 0 || _b.cols == 0) return rv

      // if we have a weird stride...
      val a: DenseMatrix[Double] = if(_a.majorStride < math.max(if(_a.isTranspose) _a.cols else _a.rows, 1)) _a.copy else _a
      val b: DenseMatrix[Double] = if(_b.majorStride < math.max(if(_b.isTranspose) _b.cols else _b.rows, 1)) _b.copy else _b

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
    def apply(a : DenseMatrix[Double], b : DenseVector[Double]): DenseVector[Double] = {
      require(a.cols == b.length, "Dimension mismatch!")
      val rv: DenseVector[Double] = DenseVector.zeros[Double](a.rows)
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
    override def apply(A : DenseMatrix[Double], V : DenseMatrix[Double]): DenseMatrix[Double] = {
      require(A.rows == V.rows, "Non-conformant matrix sizes")

      if (A.rows == A.cols) {
        // square: LUSolve
        val X: DenseMatrix[Double] = DenseMatrix.zeros[Double](V.rows, V.cols)
        X := V
        LUSolve(X,A)
        X
      } else {
        // non-square: QRSolve
        val X: DenseMatrix[Double] = DenseMatrix.zeros[Double](A.cols, V.cols)
        QRSolve(X,A,V)
        X
      }
    }

    /** X := A \ X, for square A */
    def LUSolve(X : DenseMatrix[Double], A : DenseMatrix[Double]): DenseMatrix[Double] = {
      require(X.offset == 0)
      require(A.offset == 0)
      val piv: Array[Int] = new Array[Int](A.rows)
      val newA: DenseMatrix[Double] = A.copy
      assert(!newA.isTranspose)

      val info: Int = {
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
    def QRSolve(X : DenseMatrix[Double], A : DenseMatrix[Double], V : DenseMatrix[Double]): DenseMatrix[Double] = {
      require(X.offset == 0)
      require(A.offset == 0)
      require(V.offset == 0)
      require(X.rows == A.cols, "Wrong number of rows in return value")
      require(X.cols == V.cols, "Wrong number of rows in return value")
      val transpose: Boolean = X.isTranspose

      val nrhs: Int = V.cols

      // allocate temporary solution matrix
      val Xtmp: DenseMatrix[Double] = DenseMatrix.zeros[Double](math.max(A.rows, A.cols), nrhs)
      val M: Int = if (!transpose) A.rows else A.cols
      Xtmp(0 until M,0 until nrhs) := V(0 until M, 0 until nrhs)

      val newData: Array[Double] = A.data.clone()

      // query optimal workspace
      val queryWork: Array[Double] = new Array[Double](1)
      val queryInfo = new intW(0)
      lapack.dgels(
        if (!transpose) "N" else "T",
        A.rows, A.cols, nrhs,
        newData, A.majorStride,
        Xtmp.data, math.max(1,math.max(A.rows,A.cols)),
        queryWork, -1, queryInfo)

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
        A.rows, A.cols, nrhs,
        newData, A.majorStride,
        Xtmp.data, math.max(1,math.max(A.rows,A.cols)),
        work, work.length, info)

      if (info.`val` < 0)
        throw new IllegalArgumentException

      // extract solution
      val N: Int = if (!transpose) A.cols else A.rows
      X(0 until N, 0 until nrhs) := Xtmp(0 until N, 0 until nrhs)

      X
    }
  }

  implicit object DenseMatrixCanSolveDenseVector extends OpSolveMatrixBy.Impl2[DenseMatrix[Double], DenseVector[Double], DenseVector[Double]] {
    override def apply(a : DenseMatrix[Double], b : DenseVector[Double]): DenseVector[Double] = {
      val rv: DenseMatrix[Double] = a \ new DenseMatrix[Double](b.size, 1, b.data, b.offset, b.stride, true)
      new DenseVector[Double](rv.data)
    }
  }



  implicit val mulDVDM: OpMulMatrix.Impl2[DenseVector[Double], DenseMatrix[Double], DenseMatrix[Double]] = {
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
        val rv: DenseMatrix[Double] = DenseMatrix.zeros[Double](a.length, b.cols)
        blas.dgemm("T", transposeString(b),
          rv.rows, rv.cols, 1,
          1.0, a.data, a.offset, a.stride, b.data, b.offset, b.majorStride,
          0.0, rv.data, 0, rv.rows)
        rv

      }
    }
  }



}



// TODO: fix expand to allow us to remove this code duplication
trait DenseMatrixFloatMultiplyStuff extends DenseMatrixOps with DenseMatrixMultOps { this: DenseMatrix.type =>
  implicit object DenseMatrixFMulDenseMatrixF
    extends OpMulMatrix.Impl2[DenseMatrix[Float], DenseMatrix[Float], DenseMatrix[Float]] {
    def apply(_a : DenseMatrix[Float], _b : DenseMatrix[Float]): DenseMatrix[Float] = {
      require(_a.cols == _b.rows, "Dimension mismatch!")
      val rv: DenseMatrix[Float] = DenseMatrix.zeros[Float](_a.rows, _b.cols)

      if(_a.rows == 0 || _b.rows == 0 || _a.cols == 0 || _b.cols == 0) return rv

      // if we have a weird stride...
      val a: DenseMatrix[Float] = if(_a.majorStride < math.max(if(_a.isTranspose) _a.cols else _a.rows, 1)) _a.copy else _a
      val b: DenseMatrix[Float] = if(_b.majorStride < math.max(if(_b.isTranspose) _b.cols else _b.rows, 1)) _b.copy else _b

      blas.sgemm(transposeString(a), transposeString(b),
        rv.rows, rv.cols, a.cols,
        1.0f, a.data, a.offset, a.majorStride,
        b.data, b.offset, b.majorStride,
        0.0f, rv.data, 0, rv.rows)
      rv
    }
  }


  private def transposeString(a: DenseMatrix[Float]): String = {
    if (a.isTranspose) "T" else "N"
  }

  implicit object DenseMatrixFMulDenseVectorF
    extends OpMulMatrix.Impl2[DenseMatrix[Float], DenseVector[Float], DenseVector[Float]] {
    def apply(a : DenseMatrix[Float], b : DenseVector[Float]): DenseVector[Float] = {
      require(a.cols == b.length, "Dimension mismatch!")
      val rv: DenseVector[Float] = DenseVector.zeros[Float](a.rows)
      blas.sgemv(transposeString(a),
        if(a.isTranspose) a.cols else a.rows, if(a.isTranspose) a.rows else a.cols,
        1.0f, a.data, a.offset, a.majorStride,
        b.data, b.offset, b.stride,
        0.0f, rv.data, rv.offset, rv.stride)
      rv
    }
  }

  implicit object DenseMatrixFloatCanSolveDenseMatrixFloat
    extends OpSolveMatrixBy.Impl2[DenseMatrix[Float], DenseMatrix[Float], DenseMatrix[Float]] {
    override def apply(A : DenseMatrix[Float], V : DenseMatrix[Float]): DenseMatrix[Float] = {
      require(A.rows == V.rows, "Non-conformant matrix sizes")

      if (A.rows == A.cols) {
        // square: LUSolve
        val X: DenseMatrix[Float] = DenseMatrix.zeros[Float](V.rows, V.cols)
        X := V
        LUSolve(X,A)
        X
      } else {
        // non-square: QRSolve
        val X: DenseMatrix[Float] = DenseMatrix.zeros[Float](A.cols, V.cols)
        QRSolve(X,A,V)
        X
      }
    }

    /** X := A \ X, for square A */
    def LUSolve(X : DenseMatrix[Float], A : DenseMatrix[Float]): DenseMatrix[Float] = {
      require(X.offset == 0)
      require(A.offset == 0)
      val piv: Array[Int] = new Array[Int](A.rows)
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
    def QRSolve(X : DenseMatrix[Float], A : DenseMatrix[Float], V : DenseMatrix[Float]): DenseMatrix[Float] = {
      require(X.offset == 0)
      require(A.offset == 0)
      require(V.offset == 0)
      require(X.rows == A.cols, "Wrong number of rows in return value")
      require(X.cols == V.cols, "Wrong number of rows in return value")
      val transpose: Boolean = X.isTranspose

      val nrhs: Int = V.cols

      // allocate temporary solution matrix
      val Xtmp: DenseMatrix[Float] = DenseMatrix.zeros[Float](math.max(A.rows, A.cols), nrhs)
      val M = if (!transpose) A.rows else A.cols
      Xtmp(0 until M,0 until nrhs) := V(0 until M, 0 until nrhs)

      val newData: Array[Float] = A.data.clone()

      // query optimal workspace
      val queryWork: Array[Float] = new Array[Float](1)
      val queryInfo = new intW(0)
      lapack.sgels(
        if (!transpose) "N" else "T",
        A.rows, A.cols, nrhs,
        newData, A.majorStride,
        Xtmp.data, math.max(1,math.max(A.rows,A.cols)),
        queryWork, -1, queryInfo)

      // allocate workspace
      val work: Array[Float] = {
        val lwork = {
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
        A.rows, A.cols, nrhs,
        newData, A.majorStride,
        Xtmp.data, math.max(1,math.max(A.rows,A.cols)),
        work, work.length, info)

      if (info.`val` < 0)
        throw new IllegalArgumentException

      // extract solution
      val N: Int = if (!transpose) A.cols else A.rows
      X(0 until N, 0 until nrhs) := Xtmp(0 until N, 0 until nrhs)

      X
    }
  }

  implicit object DenseMatrixFloatCanSolveDenseVectorFloat extends OpSolveMatrixBy.Impl2[DenseMatrix[Float], DenseVector[Float], DenseVector[Float]] {
    override def apply(a : DenseMatrix[Float], b : DenseVector[Float]): DenseVector[Float] = {
      val rv: DenseMatrix[Float] = a \ new DenseMatrix[Float](b.size, 1, b.data, b.offset, b.stride, true)
      new DenseVector[Float](rv.data)
    }
  }



  implicit val mulDVDMFloat: OpMulMatrix.Impl2[DenseVector[Float], DenseMatrix[Float], DenseMatrix[Float]] = {
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
        blas.sgemm("T", transposeString(b),
          rv.rows, rv.cols, 1,
          1.0f, a.data, a.offset, a.stride, b.data, b.offset, b.majorStride,
          0.0f, rv.data, 0, rv.rows)
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
            (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _}) op: Op.Impl2[T, T, T]):
  Op.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]] =
  new Op.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]] {
    def apply(a: DenseMatrix[T], b: DenseMatrix[T]): Unit = {
      val ad: Array[T] = a.data
      val bd: Array[T] = b.data
      var c = 0

      if(a.overlaps(b)) {
        val ac: DenseMatrix[T] = a.copy
        apply(ac,b)
        a := ac
      } else {
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = op(ad(a.linearIndex(r,c)), bd(b.linearIndex(r,c)))
            r += 1
          }
          c += 1
        }
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
           (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _}) op: Op.Impl2[T, T, T]):
  Op.InPlaceImpl2[DenseMatrix[T], T] =
  new Op.InPlaceImpl2[DenseMatrix[T], T] {
    def apply(a: DenseMatrix[T], b: T): Unit = {
      val ad: Array[T] = a.data
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

    val uop: Op.InPlaceImpl2[DenseMatrix[T], T] = implicitly[Op.InPlaceImpl2[DenseMatrix[T], T]]

    new Op.Impl2[DenseMatrix[T], T, DenseMatrix[T]] {
      override def apply(a : DenseMatrix[T], b: T): DenseMatrix[T] = {
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
            (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {_ % _}, {_ pow _}) op: Op.Impl2[T, T, T]):
  Op.Impl2[T, DenseMatrix[T], DenseMatrix[T]] =
  new Op.Impl2[T, DenseMatrix[T], DenseMatrix[T]] {
    def apply(b: T, a: DenseMatrix[T]): DenseMatrix[T] = {
      val res: DenseMatrix[T] = DenseMatrix.zeros[T](a.rows,a.cols)
      val resd: Array[T] = res.data
      val ad: Array[T] = a.data
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

    val uop: Op.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]] = implicitly[Op.InPlaceImpl2[DenseMatrix[T], DenseMatrix[T]]]

    new Op.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
      override def apply(a : DenseMatrix[T], b: DenseMatrix[T]): DenseMatrix[T] = {
        val c: DenseMatrix[T] = copy(a)
        uop(c, b)
        c
      }
      implicitly[BinaryRegistry[Matrix[T], Matrix[T], Op.type, Matrix[T]]].register(this)
    }
  }

}

trait DenseMatrixOpsLowPrio { this: DenseMatrixOps =>
  // LOL, if we explicitly annotate the type, then the implicit resolution thing will load this recursively.
  // If we don't, then everything works ok.
  @expand
  implicit def canMulM_V_def[@expand.args(Int, Float, Double, Long, Complex, BigInt) T, A, B](implicit bb :  B <:< Vector[T]):
  breeze.linalg.operators.OpMulMatrix.Impl2[A, B, DenseVector[T]] = (
    implicitly[OpMulMatrix.Impl2[DenseMatrix[T], Vector[T], DenseVector[T]]].asInstanceOf[breeze.linalg.operators.OpMulMatrix.Impl2[A, B, DenseVector[T]]]
  )

  // ibid.
  @expand
  implicit def canMulM_M_def[@expand.args(Int, Float, Double, Long, Complex, BigInt) T, B](implicit bb :  B <:< Matrix[T]):
  OpMulMatrix.Impl2[DenseMatrix[T], B, DenseMatrix[T]] = (
    implicitly[OpMulMatrix.Impl2[DenseMatrix[T], Matrix[T], DenseMatrix[T]]].asInstanceOf[OpMulMatrix.Impl2[DenseMatrix[T], B, DenseMatrix[T]]]
    )
}

trait DenseMatrixMultOps extends DenseMatrixOps with DenseMatrixOpsLowPrio { this: DenseMatrix.type =>
  // I don't know why this import is necessary to make the DefaultArrayValue do the right thing.
  // If I remove the import breeze.storage.DefaultArrayValue._, everything breaks, for some reason.
  import breeze.math.Complex.ComplexDefaultArrayValue

  @expand
  @expand.valify
  implicit def op_DM_V[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]: BinaryRegistry[DenseMatrix[T], Vector[T], OpMulMatrix.type, DenseVector[T]] = new BinaryRegistry[DenseMatrix[T], Vector[T], OpMulMatrix.type, DenseVector[T]] {
    override def bindingMissing(a: DenseMatrix[T], b: Vector[T]): DenseVector[T] = {

      // TODO: this could probably be much faster?
      require(a.cols == b.length)
      val res: DenseVector[T] = DenseVector.zeros[T](a.rows)
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
  implicit def op_DM_M[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]: BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix.type, DenseMatrix[T]] = new BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix.type, DenseMatrix[T]] {
    override def bindingMissing(a: DenseMatrix[T], b: Matrix[T]): DenseMatrix[T] = {
      // Martin Senne:
      // Accessing consequent areas in memory in the innermost loop ( a(i,l), a(i+1,l) ) is faster
      // than accessing ( b(c, j), b(c, j+1) as data layout in memory is column-like (Fortran), that is a(0,0), a(1,0), a(2,0), ...
      // Thus (adapted from dgemm in BLAS):
      //   - exchanged loop order
      //   - so to access consequent entries in the innermost loop and to hopefully avoid cache-misses
      val res: DenseMatrix[T] = DenseMatrix.zeros[T](a.rows, b.cols)
      require(a.cols == b.rows)

      val colsB: Int = b.cols
      val colsA: Int = a.cols
      val rowsA: Int = a.rows

      var j = 0
      while (j < colsB) {
        var l = 0;
        while (l < colsA) {

          val v = b(l, j)
          var i = 0
          while (i < rowsA) {
            res(i, j) += v * a(i,l)
            i += 1
          }
          l += 1
        }
        j += 1
      }
      res
    }
    implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]]].register(this)
  }


  implicit def op_DM_DM_Semiring[T:Semiring:ClassTag:DefaultArrayValue]: OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] =
  new OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
    implicit val ring = implicitly[Semiring[T]]
    override def apply(a: DenseMatrix[T], b: DenseMatrix[T]): DenseMatrix[T] = {

      val res: DenseMatrix[T] = DenseMatrix.zeros[T](a.rows, b.cols)
      require(a.cols == b.rows)

      val colsB: Int = b.cols
      val colsA: Int = a.cols
      val rowsA: Int = a.rows

      var j = 0
      while (j < colsB) {
        var l = 0;
        while (l < colsA) {

          val v = b(l, j)
          var i = 0
          while (i < rowsA) {
            res(i, j) = ring.+(res(i,j), ring.*(a(i,l), v))
            i += 1
          }
          l += 1
        }
        j += 1
      }
      res
    }

  }

  @expand
  @expand.valify
  implicit def op_DM_DM[@expand.args(Int, Long, Float, Double, BigInt, Complex) T]: OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] =
  new OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
    override def apply(a: DenseMatrix[T], b: DenseMatrix[T]): DenseMatrix[T] = {
      // Martin Senne:
      // Accessing consequent areas in memory in the innermost loop ( a(i,l), a(i+1,l) ) is faster
      // than accessing ( b(c, j), b(c, j+1) as data layout in memory is column-like (Fortran), that is a(0,0), a(1,0), a(2,0), ...
      // Thus (adapted from dgemm in BLAS):
      //   - exchanged loop order
      //   - so to access consequent entries in the innermost loop and to hopefully avoid cache-misses
      // Improved performance: DenseMatrix[Int] ( 1000 x 1000 now runs in 12sec than in 16sec )
      //    as comparison      DenseMatrix[Double] (1000 x 1000) takes ~1sec via BLAS.dgemm (native (0.8sec) and f2j (1sec) )
      // so there seems room for improvement ;)

      // TODO: @dlwh: Why does implicitly(DM, M, Op, DM) occur twice (top and bottom) and at different positions

      val res: DenseMatrix[T] = DenseMatrix.zeros[T](a.rows, b.cols)
      require(a.cols == b.rows)

      val colsB: Int = b.cols
      val colsA: Int = a.cols
      val rowsA: Int = a.rows

      var j = 0
      while (j < colsB) {
        var l = 0;
        while (l < colsA) {

          val v = b(l, j)
          var i = 0
          while (i < rowsA) {
            res(i, j) += v * a(i,l)
            i += 1
          }
          l += 1
        }
        j += 1
      }
      res
    }
    implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]]].register(this)
    implicitly[BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix.type, DenseMatrix[T]]].register(this)
  }

}

trait LowPriorityDenseMatrix extends LowPriorityDenseMatrix1 {

  implicit def canSliceWeirdRows[V]: CanSlice2[DenseMatrix[V], Seq[Int], ::.type, SliceMatrix[Int, Int, V]] = {
    new CanSlice2[DenseMatrix[V], Seq[Int], ::.type, SliceMatrix[Int, Int, V]] {
      def apply(from: DenseMatrix[V], slice: Seq[Int], slice2: ::.type): SliceMatrix[Int, Int, V] = {
        new SliceMatrix(from, slice.toIndexedSeq, (0 until from.cols))
      }
    }
  }

  class SetDMDMOp[@specialized(Int, Double, Float) V] extends OpSet.InPlaceImpl2[DenseMatrix[V], DenseMatrix[V]] {
    def apply(a: DenseMatrix[V], b: DenseMatrix[V]): Unit =  {
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
      val ad: Array[V] = a.data
      val bd: Array[V] = b.data
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
    def apply(a: DenseMatrix[V], b: DenseVector[V]): Unit = {
      require(a.rows == b.length && a.cols == 1 || a.cols == b.length && a.rows == 1, "DenseMatrix must have same number of rows, or same number of columns, as DenseVector, and the other dim must be 1.")
      val ad: Array[V] = a.data
      val bd: Array[V] = b.data
      var c: Int = 0
      var boff: Int = b.offset
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
    def apply(a: DenseMatrix[V], b: V): Unit = {
      if(a.data.length - a.offset == a.rows * a.cols) {
        ArrayUtil.fill(a.data, a.offset, a.size, b)
        return
      }

      // slow path when we don't have a trivial matrix
      val ad: Array[V] = a.data
      var c: Int = 0
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
  implicit def canCollapseRows[V, R:ClassTag:DefaultArrayValue]: CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], R, DenseMatrix[R]]  =
  new CanCollapseAxis[DenseMatrix[V], Axis._0.type, DenseVector[V], R, DenseMatrix[R]] {
    def apply(from: DenseMatrix[V], axis: Axis._0.type)(f: (DenseVector[V]) => R): DenseMatrix[R] = {
      val result: DenseMatrix[R] = DenseMatrix.zeros[R](1, from.cols)
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
  implicit def canCollapseCols[V, R:ClassTag:DefaultArrayValue] =
  new CanCollapseAxis[DenseMatrix[V], Axis._1.type, DenseVector[V], R, DenseVector[R]] {
    def apply(from: DenseMatrix[V], axis: Axis._1.type)(f: (DenseVector[V]) => R): DenseVector[R] = {
      val result: DenseVector[R] = DenseVector.zeros[R](from.rows)
      val t = from.t
      for(r <- 0 until from.rows) {
        result(r) = f(t(::, r))
      }
      result
    }
  }


  class SetMMOp[@specialized(Int, Double, Float) V] extends OpSet.InPlaceImpl2[DenseMatrix[V], Matrix[V]] {
    def apply(a: DenseMatrix[V], b: Matrix[V]): Unit = {
      require(a.rows == b.rows, "Matrixs must have same number of rows")
      require(a.cols == b.cols, "Matrixs must have same number of columns")

      // slow path when we don't have a trivial matrix
      val ad: Array[V] = a.data
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
    def apply(a: DenseMatrix[V], b: Vector[V]): Unit = {
      require(a.rows == b.length && a.cols == 1 || a.cols == b.length && a.rows == 1, "DenseMatrix must have same number of rows, or same number of columns, as DenseVector, and the other dim must be 1.")
      val ad: Array[V] = a.data
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
