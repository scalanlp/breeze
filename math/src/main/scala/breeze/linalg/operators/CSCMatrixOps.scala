package breeze.linalg
package operators

import breeze.generic.UFunc
import breeze.linalg.support.CanZipMapValues
import breeze.macros.expand
import breeze.linalg.{DenseMatrix, SparseVector}
import breeze.math._
import breeze.numerics.pow
import breeze.storage.Zero
import breeze.util.SerializableLogging
import scala.reflect.ClassTag
import java.util

/**
 * TODO
 *
 * @author dlwh
 **/
trait CSCMatrixOps extends CSCMatrixOps_Ring {  this: CSCMatrix.type =>
  // don't remove
  import breeze.math.PowImplicits._

  implicit def canMulSV_CSC_eq_CSC[T](implicit op: OpMulMatrix.Impl2[CSCMatrix[T],CSCMatrix[T],CSCMatrix[T]],zero: Zero[T]):
  OpMulMatrix.Impl2[SparseVector[T],CSCMatrix[T],CSCMatrix[T]] =
    new OpMulMatrix.Impl2[SparseVector[T],CSCMatrix[T],CSCMatrix[T]] {
      def apply(v: SparseVector[T], v2: CSCMatrix[T]): CSCMatrix[T] = {
        require(v2.rows == 1)
        val csc = new CSCMatrix[T](v.data,v.length,1,Array(0,v.activeSize),v.activeSize,v.index)
        op(csc,v2)
      }
    }

  implicit def canMulSVt_CSC_eq_SVt[T](implicit op: OpMulMatrix.Impl2[CSCMatrix[T],CSCMatrix[T],CSCMatrix[T]],zero: Zero[T],ct: ClassTag[T]):
  OpMulMatrix.Impl2[Transpose[SparseVector[T]],CSCMatrix[T],Transpose[SparseVector[T]]] =
    new OpMulMatrix.Impl2[Transpose[SparseVector[T]],CSCMatrix[T],Transpose[SparseVector[T]]] {
      def apply(v: Transpose[SparseVector[T]], v2: CSCMatrix[T]): Transpose[SparseVector[T]] = {
        require(v2.rows == v.inner.length)
        val csc = v.inner.asCSCMatrix()
        val cscr = op(csc,v2)
        val ind = Array.ofDim[Int](cscr.data.length)
        var i = 0
        var c = 1
        while (c < cscr.colPtrs.length) {
          if (cscr.colPtrs(c-1) != cscr.colPtrs(c)) {
            ind(i) = c-1
            i += 1
          }
          c += 1
        }
        new Transpose[SparseVector[T]](new SparseVector[T](ind,cscr.data,cscr.activeSize,cscr.cols))
      }
    }

  @expand
  @expand.valify
  implicit def csc_csc_BadUpdateOps[@expand.args(Int,Double,Float,Long) T,
  @expand.args(OpDiv,OpMod) Op <: OpType]
  (implicit @expand.sequence[Op]({_ / _},{_ % _}) op: Op.Impl2[T,T,T],
            @expand.sequence[T](0, 0.0, 0.0f, 0l)  zero: T):
  Op.InPlaceImpl2[CSCMatrix[T],CSCMatrix[T]] = {
    val mZero = implicitly[T](zero)
    new Op.InPlaceImpl2[CSCMatrix[T],CSCMatrix[T]] {
      def apply(a: CSCMatrix[T],b: CSCMatrix[T]): Unit = {
        logger.warn(s"This operation currently only acts on active values of the matrices.")
        require(a.rows == b.rows, "Matrices must have same dimensions.")
        require(a.cols == b.cols, "Matrices must have same dimensions.")
        val rows = a.rows
        val cols = a.cols
        if (a.activeSize == 0) {
          val newData = Array.ofDim[T](b.data.length)
          System.arraycopy(b.data,0,newData,0,b.data.length)
          var i = 0
          while (i < newData.length) {
            newData(i) = op(mZero,newData(i))
            i += 1
          }
          a.use(newData, b.colPtrs, util.Arrays.copyOf(b.rowIndices, b.rowIndices.length), b.activeSize)
        } else if (b.activeSize == 0) {
          val aData = a.data
          var i = 0
          while (i < aData.length) {
            aData(i) = op(aData(i),mZero)
            i += 1
          }
        } else {
          var ci = 0
          var apStop  = a.colPtrs(0)
          var bpStop  = b.colPtrs(0)
          while (ci < cols) {
            val ci1 = ci + 1
            var ap = apStop
            var bp = bpStop
            apStop = a.colPtrs(ci1)
            bpStop = b.colPtrs(ci1)
            while (ap < apStop || bp < bpStop) { //
            val ar = if (ap < apStop) a.rowIndices(ap) else rows
              val br = if (bp < bpStop) b.rowIndices(bp) else rows
              if (ar == br) {
                a.update(ar, ci, op(a.data(ap), b.data(bp)))
                ap += 1
                bp += 1
              } else if (ar < br) { // a is behind
                a.update(ar, ci, op(a.data(ap), mZero))
                ap += 1
              } else {
                a.update(br, ci, op(mZero, b.data(bp)))
                bp += 1
                ap += 1 // Because we just inserted a new value behind the pointer in A
                apStop = a.colPtrs(ci1)
              }
            }
            ci = ci1
          }
        }
      }

      implicitly[BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type]].register(this)
    }
  }

  @expand
  @expand.valify
  implicit def csc_csc_UpdateOp[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpSet) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {(a,b) => b}) op: Op.Impl2[T, T, T],
            @expand.sequence[T](0, 0.0, 0.0f, 0l)  zero: T):
  Op.InPlaceImpl2[CSCMatrix[T], CSCMatrix[T]] = {
    val mZero = implicitly[T](zero)
    new Op.InPlaceImpl2[CSCMatrix[T], CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: CSCMatrix[T]): Unit = {
        require(a.rows == b.rows, "Matrices must have same dimensions.")
        require(a.cols == b.cols, "Matrices must have same dimensions.")
        val rows = a.rows
        val cols = a.cols
        if (a.activeSize == 0) {
          val newData = Array.ofDim[T](b.data.length)
          System.arraycopy(b.data,0,newData,0,b.data.length)
          var i = 0
          while (i < newData.length) {
            newData(i) = op(mZero,newData(i))
            i += 1
          }
          a.use(newData, b.colPtrs, util.Arrays.copyOf(b.rowIndices, b.rowIndices.length), b.activeSize)
        } else if (b.activeSize == 0) {
          val aData = a.data
          var i = 0
          while (i < aData.length) {
            aData(i) = op(aData(i),mZero)
            i += 1
          }
        } else {
          var ci = 0
          var apStop  = a.colPtrs(0)
          var bpStop  = b.colPtrs(0)
          while (ci < cols) {
            val ci1 = ci + 1
            var ap = apStop
            var bp = bpStop
            apStop = a.colPtrs(ci1)
            bpStop = b.colPtrs(ci1)
            while (ap < apStop || bp < bpStop) { //
            val ar = if (ap < apStop) a.rowIndices(ap) else rows
              val br = if (bp < bpStop) b.rowIndices(bp) else rows
              if (ar == br) {
                a.update(ar, ci, op(a.data(ap), b.data(bp)))
                ap += 1
                bp += 1
              } else if (ar < br) { // a is behind
                a.update(ar, ci, op(a.data(ap), mZero))
                ap += 1
              } else {
                a.update(br, ci, op(mZero, b.data(bp)))
                bp += 1
                ap += 1 // Because we just inserted a new value behind the pointer in A
                apStop = a.colPtrs(ci1)
              }
            }
            ci = ci1
          }
        }
      }

      implicitly[BinaryUpdateRegistry[Matrix[T], Matrix[T], Op.type]].register(this)
    }
  }

  @expand
  @expand.valify
  implicit def implOps_CSCT_T_eq_CSCT[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpMulScalar, OpMulMatrix) Op<:OpType]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l)
  zero: T):
  Op.Impl2[CSCMatrix[T], T, CSCMatrix[T]] =

    new Op.Impl2[CSCMatrix[T], T, CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: T): CSCMatrix[T] = {
        val result: CSCMatrix.Builder[T] = new CSCMatrix.Builder[T](a.rows,a.cols,a.activeSize)

        var c = 0
        while (c < a.cols) {
          var ip = a.colPtrs(c)
          while (ip < a.colPtrs(c+1)) {
            val r = a.rowIndices(ip)
            result.add(r,c,a.data(ip) * b)
            ip += 1
          }
          c += 1
        }

        result.result(true, true)
      }
      implicitly[BinaryRegistry[Matrix[T], T, Op.type, Matrix[T]]].register(this)
    }

  @expand
  implicit def implOps_CSCT_T_eq_CSCT[@expand.args(OpMulScalar, OpMulMatrix) Op<:OpType, T:Field:ClassTag]:
  Op.Impl2[CSCMatrix[T], T, CSCMatrix[T]] = {
    val f = implicitly[Field[T]]
    new Op.Impl2[CSCMatrix[T], T, CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: T): CSCMatrix[T] = {
        val result: CSCMatrix.Builder[T] = new CSCMatrix.Builder[T](a.rows, a.cols, a.activeSize)

        var c = 0
        while (c < a.cols) {
          var ip = a.colPtrs(c)
          while (ip < a.colPtrs(c + 1)) {
            val r = a.rowIndices(ip)
            result.add(r, c, f.*(a.data(ip),b))
            ip += 1
          }
          c += 1
        }

        result.result(true, true)
      }

      implicitly[BinaryRegistry[Matrix[T], T, Op.type, Matrix[T]]].register(this)
    }
  }

  @expand
  @expand.valify
  implicit def canMulM_V[@expand.args(Int, Float, Double, Long) T]: BinaryRegistry[CSCMatrix[T], Vector[T],OpMulMatrix.type, Vector[T]] = new BinaryRegistry[CSCMatrix[T], Vector[T], OpMulMatrix.type, Vector[T]] {
    override def bindingMissing(a: CSCMatrix[T], b: Vector[T]) = {
      require(a.cols == b.length, "Dimension Mismatch!")

      val res = DenseVector.zeros[T](a.rows)
      var c = 0
      while(c < a.cols) {
        var rr = a.colPtrs(c)
        val rrlast = a.colPtrs(c+1)
        while (rr < rrlast) {
          val r = a.rowIndices(rr)
          res(r) += a.data(rr) * b(c)
          rr += 1
        }
        c += 1
      }

      res
    }
    implicitly[BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix.type, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  implicit def canMulM_DV[@expand.args(Int, Float, Double, Long) T]: BinaryRegistry[CSCMatrix[T], DenseVector[T], OpMulMatrix.type, DenseVector[T]] = new BinaryRegistry[CSCMatrix[T], DenseVector[T], OpMulMatrix.type, DenseVector[T]] {
    override def bindingMissing(a: CSCMatrix[T], b: DenseVector[T]) = {
      require(a.cols == b.length, "Dimension Mismatch!")

      val res = DenseVector.zeros[T](a.rows)
      var c = 0
      while(c < a.cols) {
        var rr = a.colPtrs(c)
        val rrlast = a.colPtrs(c+1)
        while (rr < rrlast) {
          val r = a.rowIndices(rr)
          res(r) += a.data(rr) * b(c)
          rr += 1
        }
        c += 1
      }

      res
    }
    implicitly[BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix.type, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  implicit def canMulM_SV[@expand.args(Int, Float, Double, Long) T]: BinaryRegistry[CSCMatrix[T], SparseVector[T], OpMulMatrix.type, SparseVector[T]] = new BinaryRegistry[CSCMatrix[T], SparseVector[T], OpMulMatrix.type, SparseVector[T]] {
    override def bindingMissing(a: CSCMatrix[T], b: SparseVector[T]) = {
      require(a.cols == b.length, "Dimension Mismatch!")
      val res = new VectorBuilder[T](a.rows, b.iterableSize min a.rows)
      var c = 0
      var lastOffset = 0
      while(c < a.cols) {
        var rr = a.colPtrs(c)
        val rrlast = a.colPtrs(c+1)
        if(rr < rrlast) {
          val newBOffset = util.Arrays.binarySearch(b.index, lastOffset, math.min(b.activeSize, c+1), c)
          if(newBOffset < 0) {
            lastOffset = ~newBOffset
          } else {
            while (rr < rrlast) {
              val r = a.rowIndices(rr)
              res.add(r, a.data(rr) * b.valueAt(newBOffset))
              rr += 1
            }
            lastOffset = newBOffset + 1
          }
        }
        c += 1
      }

      res.toSparseVector
    }
    implicitly[BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix.type, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  implicit def canMulM_DM[@expand.args(Int, Float, Double, Long) T]
  : breeze.linalg.operators.OpMulMatrix.Impl2[CSCMatrix[T], DenseMatrix[T], DenseMatrix[T]]= new breeze.linalg.operators.OpMulMatrix.Impl2[CSCMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
    def apply(a: CSCMatrix[T], b: DenseMatrix[T]) = {

      if(a.cols != b.rows) throw new RuntimeException("Dimension Mismatch!")

      val res = new DenseMatrix[T](a.rows, b.cols)
      var i = 0
      while (i < b.cols) {
        var j = 0
        while (j < a.cols) {
          val v = b(j, i)
          var k = a.colPtrs(j)
          while (k < a.colPtrs(j+1)) {
            res(a.rowIndices(k), i) += v * a.data(k)
            k += 1
          }
          j += 1
        }
        i += 1
      }


      res
    }
    implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]]].register(this)
  }

  @expand
  @expand.valify
  implicit def canMulDM_M[@expand.args(Int, Float, Double, Long) T]:breeze.linalg.operators.OpMulMatrix.Impl2[DenseMatrix[T], CSCMatrix[T], DenseMatrix[T]] = new breeze.linalg.operators.OpMulMatrix.Impl2[DenseMatrix[T], CSCMatrix[T], DenseMatrix[T]] {
    def apply(a: DenseMatrix[T], b: CSCMatrix[T]) = {
      if(a.cols != b.rows) throw new RuntimeException("Dimension Mismatch!")

      val res = new DenseMatrix[T](a.rows, b.cols)
      var i = 0
      while (i < b.cols) {
        var j = b.colPtrs(i)
        while (j < b.colPtrs(i+1)) {
          val dval = b.data(j)
          val ival = b.rowIndices(j)
          var k = 0
          while (k < a.rows) {
            res(k,i) += a(k,ival)*dval
            k += 1
          }
          j += 1
        }
        i += 1
      }

      res
    }
    implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]]].register(this)
    implicitly[BinaryRegistry[DenseMatrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]]].register(this)
  }

  @expand
  @expand.valify
  implicit def canMulM_M[@expand.args(Int, Float, Double, Long) T]: breeze.linalg.operators.OpMulMatrix.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] = new breeze.linalg.operators.OpMulMatrix.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] {
    def apply(a: CSCMatrix[T], b: CSCMatrix[T]) = {

      if(a.cols != b.rows) throw new RuntimeException("Dimension Mismatch!")

      var numnz = 0
      var i = 0
      while (i < b.cols) {
        var j = b.colPtrs(i)
        while (j < b.colPtrs(i+1)) {
          numnz += a.colPtrs(b.rowIndices(j)+1) - a.colPtrs(b.rowIndices(j))
          j += 1
        }
        i += 1
      }
      val res = new CSCMatrix.Builder[T](a.rows, b.cols, numnz)
      i = 0
      while (i < b.cols) {
        var j = b.colPtrs(i)
        while (j < b.colPtrs(i+1)) {
          val dval = b.data(j)
          var k = a.colPtrs(b.rowIndices(j))
          while (k < a.colPtrs(b.rowIndices(j)+1)) {
            res.add(a.rowIndices(k), i, a.data(k) * dval)
            k += 1
          }
          j += 1
        }
        i += 1
      }


      res.result()
    }
    implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix.type, Matrix[T]]].register(this)
  }

  // code based on that provided by sciss:
  // https://github.com/Sciss/breeze/blob/bb5cf8a1969545e1a7b0cd7ddde5f974be8301cd/math/src/main/scala/breeze/linalg/CSCMatrixExtraOps.scala
  @expand
  @expand.valify
  implicit def CSCMatrixCanMulM_M[@expand.args (Int, Float, Long, Double) A]: OpMulScalar.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] = new OpMulScalar.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] {

    final def apply(a: CSCMatrix[A], b: CSCMatrix[A]): CSCMatrix[A] = {
      val rows  = a.rows
      val cols  = a.cols
      require(rows == b.rows, "Matrices must have same number of rows!")
      require(cols == b.cols, "Matrices must have same number of cols!")

      if (cols == 0 || rows == 0) return CSCMatrix.zeros[A](rows, cols)

      val res     = new CSCMatrix.Builder[A](rows, cols, math.min(a.activeSize, b.activeSize))
      var ci      = 0             // column index [0 ... cols)
      var apStop  = a.colPtrs(0)  // pointer into row indices and data
      var bpStop  = b.colPtrs(0)  // pointer into row indices and data
      while (ci < cols) {
        val ci1 = ci + 1
        var ap  = apStop
        var bp  = bpStop
        apStop = a.colPtrs(ci1)
        bpStop = b.colPtrs(ci1)
        while (ap < apStop || bp < bpStop) {
          val ari = if (ap < apStop) a.rowIndices(ap) else rows // row index [0 ... rows)
          val bri = if (bp < bpStop) b.rowIndices(bp) else rows
          if (ari == bri) {           // column and row match, this cell goes into result matrix
          val v = a.data(ap) * b.data(bp)
            res.add(ari, ci, v)
            ap += 1
            bp += 1
          } else if (ari < bri) {     // next b row starts further down, therefore increase a pointer
            ap += 1
          } else /* ari > bri */ {    // next a row starts further down, therefore increase b pointer
            bp += 1
          }
        }
        ci = ci1
      }

      res.result()
    }

    implicitly[BinaryRegistry[Matrix[A], Matrix[A], OpMulScalar.type, Matrix[A]]].register(this)
  }
}

trait CSCMatrixOps_Ring extends CSCMatrixOpsLowPrio with SerializableLogging {
  this: CSCMatrixOps =>
  @expand
  implicit def csc_csc_BadUpdateOps[@expand.args(OpDiv,OpPow, OpMod) Op <: OpType,T:Field:ClassTag]
  (implicit @expand.sequence[Op]({f./(_,_)},{f.pow(_,_)},{f.%(_,_)}) op: Op.Impl2[T,T,T]):
  Op.InPlaceImpl2[CSCMatrix[T],CSCMatrix[T]] = {
    new Op.InPlaceImpl2[CSCMatrix[T],CSCMatrix[T]] {
      def apply(a: CSCMatrix[T],b: CSCMatrix[T]): Unit = {
        throw new UnsupportedOperationException(s"Performing a Matrix x Matrix operation of this kind will result in a non-sparse matrix, possibly full of NaNs.")
      }
    }
  }

  @expand
  implicit def csc_csc_UpdateOp[@expand.args(OpAdd, OpSub, OpMulScalar,  OpSet) Op <: OpType, T:Field:ClassTag]
  (implicit @expand.sequence[Op]({f.+(_,_)}, {f.-(_,_)}, {f.*(_,_)}, {(a,b) => b}) op: Op.Impl2[T, T, T]):
  Op.InPlaceImpl2[CSCMatrix[T], CSCMatrix[T]] = {
    val f = implicitly[Field[T]]
    new Op.InPlaceImpl2[CSCMatrix[T], CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: CSCMatrix[T]): Unit = {
        val rows  = a.rows
        val cols  = a.cols
        require(rows == b.rows, "Matrices must have same number of rows!")
        require(cols == b.cols, "Matrices must have same number of cols!")

        if (a.activeSize == 0) {
          val newData = Array.ofDim[T](b.data.length)
          var i = 0
          while (i < newData.length) {
            newData(i) = op(f.zero,b.data(i))
            i += 1
          }
          a.use(newData, b.colPtrs, util.Arrays.copyOf(b.rowIndices, b.rowIndices.length), b.activeSize)
        } else if (b.activeSize == 0) {
          val aData = a.data
          var i = 0
          while (i < aData.length) {
            aData(i) = op(aData(i),f.zero)
            i += 1
          }
        } else {
          var ci = 0
          var apStop  = a.colPtrs(0)
          var bpStop  = b.colPtrs(0)
          while (ci < cols) {
            val ci1 = ci + 1
            var ap = apStop
            var bp = bpStop
            apStop = a.colPtrs(ci1)
            bpStop = b.colPtrs(ci1)
            while (ap < apStop || bp < bpStop) { //
              val ar = if (ap < apStop) a.rowIndices(ap) else rows
              val br = if (bp < bpStop) b.rowIndices(bp) else rows
              if (ar == br) {
                a.update(ar, ci, op(a.data(ap), b.data(bp)))
                ap += 1
                bp += 1
              } else if (ar < br) { // a is behind
                a.update(ar, ci, op(a.data(ap), f.zero))
                ap += 1
              } else {
                a.update(br, ci, op(f.zero, b.data(bp)))
                bp += 1
                ap += 1 // Because we just inserted a new value behind the pointer in A
                apStop = a.colPtrs(ci1)
              }
            }
            ci = ci1
          }
        }
      }
    }
  }

  @expand
  implicit def csc_csc_Op[@expand.args(OpDiv, OpMod, OpPow) Op <: OpType, T:Field:ClassTag]:
  Op.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] = {
    val uop = implicitly[Op.InPlaceImpl2[CSCMatrix[T], CSCMatrix[T]]]

    new Op.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: CSCMatrix[T]): CSCMatrix[T] = {
        val c = copy(a)
        uop(c, b)
        c
      }
    }
  }

  @expand
  implicit def csc_T_UpdateOp[@expand.args(OpMulScalar, OpDiv, OpMod, OpPow) Op <: OpType, T:Field:ClassTag]
  (implicit @expand.sequence[Op]({f.*(_,_)}, {f./(_,_)}, {f.%(_,_)},{f.pow(_,_)}) op: Op.Impl2[T,T,T]):
  Op.InPlaceImpl2[CSCMatrix[T], T] = {
    val f = implicitly[Field[T]]
    new Op.InPlaceImpl2[CSCMatrix[T], T] {
      def apply(a: CSCMatrix[T], b: T): Unit = {
        var c = 0
        while (c < a.cols) {
          var ip = a.colPtrs(c)
          while (ip < a.colPtrs(c+1)) {
            val r = a.rowIndices(ip)
            a.update(r,c,op(a.data(ip),b))
            ip += 1
          }
          c += 1
        }
      }
      implicitly[BinaryUpdateRegistry[Matrix[T], T, Op.type]].register(this)
    }
  }

  @expand
  implicit def csc_T_Op[@expand.args(OpDiv, OpMod, OpPow) Op <: OpType, T:Field:ClassTag]
  (implicit @expand.sequence[Op]({f./(_,_)}, {f.%(_,_)},{f.pow(_,_)}) op: Op.Impl2[T,T,T]):
  Op.Impl2[CSCMatrix[T], T, CSCMatrix[T]] = {
    val uop = implicitly[Op.InPlaceImpl2[CSCMatrix[T], T]]
    new Op.Impl2[CSCMatrix[T], T, CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: T): CSCMatrix[T] = {
        val c = copy(a)
        uop(c, b)
        c
      }

      implicitly[BinaryRegistry[Matrix[T], T, Op.type, Matrix[T]]].register(this)
    }
  }

  implicit def csc_OpNeg[T:Ring:ClassTag]: OpNeg.Impl[CSCMatrix[T], CSCMatrix[T]] = {
    new OpNeg.Impl[CSCMatrix[T], CSCMatrix[T]] {
      val ring = implicitly[Ring[T]]
      def apply(a: CSCMatrix[T]): CSCMatrix[T] = {
        val result: CSCMatrix.Builder[T] = new CSCMatrix.Builder[T](a.rows, a.cols, a.activeSize)

        var c = 0
        while (c < a.cols) {
          var ip = a.colPtrs(c)
          while (ip < a.colPtrs(c + 1)) {
            val r = a.rowIndices(ip)
            result.add(r, c, ring.negate(a.data(ip)))
            ip += 1
          }
          c += 1
        }

        result.result(true, true)
      }
    }
  }

  implicit def cscScaleAdd[T: Semiring : ClassTag]: scaleAdd.InPlaceImpl3[CSCMatrix[T], T, CSCMatrix[T]] = {
    new scaleAdd.InPlaceImpl3[CSCMatrix[T], T, CSCMatrix[T]] {
      override def apply(a: CSCMatrix[T], s: T, b: CSCMatrix[T]): Unit = {
        val ring = implicitly[Semiring[T]]
        val rows = a.rows
        val cols = a.cols
        require(rows == b.rows, "Matrices must have same number of rows!")
        require(cols == b.cols, "Matrices must have same number of cols!")

        if (cols == 0 || rows == 0) return

        var ci = 0 // column index [0 ... cols)
        var apStop = a.colPtrs(0) // pointer into row indices and data
        var bpStop = b.colPtrs(0) // pointer into row indices and data
        while (ci < cols) {
          val ci1 = ci + 1
          var ap = apStop
          var bp = bpStop
          apStop = a.colPtrs(ci1)
          bpStop = b.colPtrs(ci1)
          while (ap < apStop || bp < bpStop) {
            val ari = if (ap < apStop) a.rowIndices(ap) else rows // row index [0 ... rows)
            val bri = if (bp < bpStop) b.rowIndices(bp) else rows
            if (ari == bri) {
              // column and row match, this cell goes into result matrix
              val v = ring.+(a.data(ap), ring.*(s, b.data(bp)))
              a.update(ari, ci, v)
              ap += 1
              bp += 1
            } else if (ari < bri) {
              // b is zero, so nothing is added to A
              ap += 1
            } else /* ari > bri */ {
              val v = ring.+(ring.zero, ring.*(s, b.data(bp)))
              a.update(bri, ci, v)
              bp += 1
              ap += 1
              apStop = a.colPtrs(ci1)
            }
          }
          ci = ci1
        }
      }
    }
  }

  implicit def canMulM_V_Semiring[T:Semiring:Zero:ClassTag]: BinaryRegistry[CSCMatrix[T], Vector[T],OpMulMatrix.type, Vector[T]] =
    new BinaryRegistry[CSCMatrix[T], Vector[T], OpMulMatrix.type, Vector[T]] {
    implicit val ring = implicitly[Semiring[T]]

    override def bindingMissing(a: CSCMatrix[T], b: Vector[T]) = {
      require(a.cols == b.length, "Dimension Mismatch!")

      val res = DenseVector.zeros[T](a.rows)
      var c = 0
      while(c < a.cols) {
        var rr = a.colPtrs(c)
        val rrlast = a.colPtrs(c+1)
        while (rr < rrlast) {
          val r = a.rowIndices(rr)
          res(r) = ring.+(res(r), ring.*(a.data(rr), b(c)))
          rr += 1
        }
        c += 1
      }
      res
    }
  }

  implicit def canMulM_SV_Semiring[T:Semiring:Zero:ClassTag]
  : BinaryRegistry[CSCMatrix[T], SparseVector[T], OpMulMatrix.type, SparseVector[T]] = new BinaryRegistry[CSCMatrix[T], SparseVector[T], OpMulMatrix.type, SparseVector[T]] {
    override def bindingMissing(a: CSCMatrix[T], b: SparseVector[T]) = {
      val ring = implicitly[Semiring[T]]
      require(a.cols == b.length, "Dimension Mismatch!")
      val res = new VectorBuilder[T](a.rows, b.iterableSize min a.rows)
      var c = 0
      var lastOffset = 0
      while(c < a.cols) {
        var rr = a.colPtrs(c)
        val rrlast = a.colPtrs(c+1)
        if(rr < rrlast) {
          val newBOffset = util.Arrays.binarySearch(b.index, lastOffset, math.min(b.activeSize, c+1), c)
          if(newBOffset < 0) {
            lastOffset = ~newBOffset
          } else {
            while (rr < rrlast) {
              val r = a.rowIndices(rr)
              res.add(r, ring.*(a.data(rr), b.valueAt(newBOffset)))
              rr += 1
            }
            lastOffset = newBOffset + 1
          }
        }
        c += 1
      }

      res.toSparseVector
    }
  }

  implicit def canMulM_DM_Semiring[T:Semiring:Zero:ClassTag]
  : OpMulMatrix.Impl2[CSCMatrix[T], DenseMatrix[T], DenseMatrix[T]]= new OpMulMatrix.Impl2[CSCMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
    def apply(a: CSCMatrix[T], b: DenseMatrix[T]) = {
      val ring = implicitly[Semiring[T]]
      require(a.cols == b.rows, "CSCMatrix Multiplication Dimension Mismatch")

      val res = new DenseMatrix[T](a.rows, b.cols)
      var i = 0
      while (i < b.cols) {
        var j = 0
        while (j < a.cols) {
          val v = b(j, i)
          var k = a.colPtrs(j)
          while (k < a.colPtrs(j+1)) {
            res(a.rowIndices(k), i) = ring.+(res(a.rowIndices(k), i), ring.*(v, a.data(k)))
            k += 1
          }
          j += 1
        }
        i += 1
      }


      res
    }
  }

  implicit def canMulDM_M_Semiring[T:Semiring:Zero:ClassTag]
  :OpMulMatrix.Impl2[DenseMatrix[T], CSCMatrix[T], DenseMatrix[T]] = {
    new OpMulMatrix.Impl2[DenseMatrix[T], CSCMatrix[T], DenseMatrix[T]] {
      def apply(a: DenseMatrix[T], b: CSCMatrix[T]) = {
        val ring = implicitly[Semiring[T]]
        require(a.cols == b.rows, "CSCMatrix Multiplication Dimension Mismatch")

        val res = new DenseMatrix[T](a.rows, b.cols)
        var i = 0
        while (i < b.cols) {
          var j = b.colPtrs(i)
          while (j < b.colPtrs(i+1)) {
            val dval = b.data(j)
            val ival = b.rowIndices(j)
            var k = 0
            while (k < a.rows) {
              res(k,i) = ring.+(res(k, i), ring.*(a(k,ival), dval))
              k += 1
            }
            j += 1
          }
          i += 1
        }

        res
      }
    }
  }


  implicit def canMulM_M_Semiring[T: Semiring : Zero : ClassTag]: OpMulMatrix.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] =
    new OpMulMatrix.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: CSCMatrix[T]) = {
        val ring = implicitly[Semiring[T]]
        require(a.cols == b.rows, "CSCMatrix Multiplication Dimension Mismatch")

        var numnz = 0
        var i = 0
        while (i < b.cols) {
          var j = b.colPtrs(i)
          while (j < b.colPtrs(i + 1)) {
            numnz += a.colPtrs(b.rowIndices(j) + 1) - a.colPtrs(b.rowIndices(j))
            j += 1
          }
          i += 1
        }
        val res = new CSCMatrix.Builder[T](a.rows, b.cols, numnz)
        i = 0
        while (i < b.cols) {
          var j = b.colPtrs(i)
          while (j < b.colPtrs(i + 1)) {
            val dval = b.data(j)
            var k = a.colPtrs(b.rowIndices(j))
            while (k < a.colPtrs(b.rowIndices(j) + 1)) {
              res.add(a.rowIndices(k), i, ring.*(a.data(k), dval))
              k += 1
            }
            j += 1
          }
          i += 1
        }
        res.result()
      }
    }

  implicit def zipMapVals[S, R: ClassTag : Semiring : Zero] = new CanZipMapValues[CSCMatrix[S], S, R, CSCMatrix[R]] {
    /** Maps all corresponding values from the two collections. */
    override def map(a: CSCMatrix[S], b: CSCMatrix[S], fn: (S, S) => R): CSCMatrix[R] = {
      logger.warn("Using CSCMatrix.zipMapVals. Note that this implementation currently ZipMaps over active values only, ignoring zeros.")
      val rows = a.rows
      val cols = a.cols
      require(rows == b.rows, "Matrices must have same number of rows!")
      require(cols == b.cols, "Matrices must have same number of cols!")

      if (a.activeSize == 0) {
        val newData = Array.ofDim[R](b.data.length)
        var i = 0
        while (i < b.data.length) {
          newData(i) = fn(a.zero,b.data(i))
          i += 1
        }
        new CSCMatrix[R](newData, rows, cols, util.Arrays.copyOf(b.colPtrs,b.colPtrs.length),
          b.activeSize, util.Arrays.copyOf(b.rowIndices, b.rowIndices.length))
      } else if (b.activeSize == 0) {
        val newData = Array.ofDim[R](a.data.length)
        var i = 0
        while (i < a.data.length) {
          newData(i) = fn(a.data(i),b.zero)
          i += 1
        }
        new CSCMatrix[R](newData, rows, cols, util.Arrays.copyOf(a.colPtrs,a.colPtrs.length),
          a.activeSize, util.Arrays.copyOf(a.rowIndices, a.rowIndices.length))

      } else {
        val builder = new CSCMatrix.Builder[R](a.rows, a.cols, a.activeSize)
        var ci = 0
        var apStop = a.colPtrs(0)
        var bpStop = b.colPtrs(0)
        while (ci < cols) {
          val ci1 = ci + 1
          var ap = apStop
          var bp = bpStop
          apStop = a.colPtrs(ci1)
          bpStop = b.colPtrs(ci1)
          while (ap < apStop || bp < bpStop) {
            val ar = if (ap < apStop) a.rowIndices(ap) else rows
            val br = if (bp < bpStop) b.rowIndices(bp) else rows

            if (ar == br) {
              builder.add(ar, ci, fn(a.data(ap), b.data(bp)))
              ap += 1
              bp += 1
            } else if (ar < br) {
              // a is behind
              builder.add(ar, ci, fn(a.data(ap), b.zero))
              ap += 1
            } else {
              builder.add(br, ci, fn(a.zero, b.data(bp)))
              bp += 1
            }
          }
          ci = ci1
        }
        builder.result
      }
    }
  }

  implicit def canAddM_S_Semiring[T: Semiring : ClassTag]: OpAdd.Impl2[CSCMatrix[T], T, CSCMatrix[T]] =
    new OpAdd.Impl2[CSCMatrix[T], T, CSCMatrix[T]] {
      val s = implicitly[Semiring[T]]
      val zero = s.zero

      override def apply(v: CSCMatrix[T], v2: T): CSCMatrix[T] = {
        if (v2 == zero) return copy(v)
        logger.warn("Adding a scalar to a CSCMatrix creates a dense CSCMatrix, which is inefficient under the current implementation.")
        val data = Array.fill[T](v.rows * v.cols)(v2)
        var c = 0
        while (c < v.cols) {
          var ip = v.colPtrs(c)
          while (ip < v.colPtrs(c + 1)) {
            val r = v.rowIndices(ip)
            data(r * v.cols + r) = s.+(v.data(ip),v2)
            ip += 1
          }
          c += 1
        }
        val colPtrs: Array[Int] = Array.tabulate[Int](v.cols + 1)((i: Int) => i * v.rows)
        val rowIndices: Array[Int] = Array.tabulate[Int](data.length)((i: Int) => i % v.rows)
        new CSCMatrix[T](data,v.rows,v.cols,colPtrs,data.length,rowIndices)
      }
    }


  implicit def canSubM_S_Ring[T: Ring : ClassTag]: OpSub.Impl2[CSCMatrix[T], T, CSCMatrix[T]] =
    new OpSub.Impl2[CSCMatrix[T], T, CSCMatrix[T]] {
      val s = implicitly[Ring[T]]
      val zero = s.zero

      def apply(v: CSCMatrix[T], v2: T): CSCMatrix[T] = {
        if (v2 == zero) return copy(v)
        logger.warn("Subtracting a scalar from a CSCMatrix creates a dense CSCMatrix, which is inefficient under the current implementation.")
        val data = Array.fill[T](v.rows * v.cols)(s.negate(v2))
        var c = 0
        while (c < v.cols) {
          var ip = v.colPtrs(c)
          while (ip < v.colPtrs(c + 1)) {
            val r = v.rowIndices(ip)
            data(r * v.cols + r) = s.-(v.data(ip),v2)
            ip += 1
          }
          c += 1
        }
        val colPtrs: Array[Int] = Array.tabulate[Int](v.cols + 1)((i: Int) => i * v.rows)
        val rowIndices: Array[Int] = Array.tabulate[Int](data.length)((i: Int) => i % v.rows)
        new CSCMatrix[T](data,v.rows,v.cols,colPtrs,data.length,rowIndices)
      }
    }


  implicit def canSetM_S_Ring[T: Ring : ClassTag]: OpSet.Impl2[CSCMatrix[T], T, CSCMatrix[T]] =
    new OpSet.Impl2[CSCMatrix[T], T, CSCMatrix[T]] {
      val r = implicitly[Ring[T]]
      val zero = r.zero
      def apply(v: CSCMatrix[T], v2: T): CSCMatrix[T] = {
        logger.warn("Setting CSCMatrix to scalar creates a dense CSCMatrix, which is inefficient under the current implementation.")
        if (v2 == zero)
          return CSCMatrix.zeros[T](v.rows,v.cols)
        val data: Array[T] = Array.fill[T](v.rows * v.cols)(v2)
        val colPtrs: Array[Int] = Array.tabulate[Int](v.cols + 1)((i: Int) => i * v.rows)
        val rowIndices: Array[Int] = Array.tabulate[Int](data.length)((i: Int) => i % v.rows)
        new CSCMatrix[T](data,v.rows,v.cols,colPtrs,v.rows*v.cols,rowIndices)
      }
    }

  implicit def CSCMatrixCanMulM_M_Semiring[A:Semiring:ClassTag:Zero]: OpMulScalar.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] =
    new OpMulScalar.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] {

    final def apply(a: CSCMatrix[A], b: CSCMatrix[A]): CSCMatrix[A] = {
      val ring = implicitly[Semiring[A]]
      val rows  = a.rows
      val cols  = a.cols
      require(rows == b.rows, "Matrices must have same number of rows!")
      require(cols == b.cols, "Matrices must have same number of cols!")

      if (cols == 0 || rows == 0) return CSCMatrix.zeros[A](rows, cols)

      val res     = new CSCMatrix.Builder[A](rows, cols, math.min(a.activeSize, b.activeSize))
      var ci      = 0             // column index [0 ... cols)
      var apStop  = a.colPtrs(0)  // pointer into row indices and data
      var bpStop  = b.colPtrs(0)  // pointer into row indices and data
      while (ci < cols) {
        val ci1 = ci + 1
        var ap  = apStop
        var bp  = bpStop
        apStop = a.colPtrs(ci1)
        bpStop = b.colPtrs(ci1)
        while (ap < apStop || bp < bpStop) {
          val ari = if (ap < apStop) a.rowIndices(ap) else rows  // row index [0 ... rows)
          val bri = if (bp < bpStop) b.rowIndices(bp) else rows
          if (ari == bri) {           // column and row match, this cell goes into result matrix
          val v = ring.*(a.data(ap), b.data(bp))
            res.add(ari, ci, v)
            ap += 1
            bp += 1
          } else if (ari < bri) {     // next b row starts further down, therefore increase a pointer
            ap += 1
          } else /* ari > bri */ {    // next a row starts further down, therefore increase b pointer
            bp += 1
          }
        }
        ci = ci1
      }

      res.result
    }

  }

  implicit def CSCMatrixCanAdd_M_M_Semiring[A:Semiring:Zero:ClassTag]: OpAdd.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] =
    new OpAdd.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] {
    val ring = implicitly[Semiring[A]]
    def apply(a: CSCMatrix[A], b: CSCMatrix[A]): CSCMatrix[A] = {
      val res = copy(a)
      var c = 0
      while(c < b.cols) {
        var rr = b.colPtrs(c)
        val rrlast = b.colPtrs(c+1)
        while (rr < rrlast) {
          val r = b.rowIndices(rr)
          res.update(r, c, ring.+(a(r,c),b.data(rr)))
          rr += 1
        }
        c += 1
      }
      res
    }
  }

  implicit def CSCMatrixCanSubM_M_Ring[A:Ring:Zero:ClassTag]: OpSub.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] = new OpSub.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] {
    val ring = implicitly[Ring[A]]
    def apply(a: CSCMatrix[A], b: CSCMatrix[A]): CSCMatrix[A] = {
      val bldr = CSCMatrix.Builder.fromMatrix(a)
      bldr.sizeHint(a.activeSize + b.activeSize)
      var c = 0
      while(c < b.cols) {
        var rr = b.colPtrs(c)
        val rrlast = b.colPtrs(c+1)
        while (rr < rrlast) {
          val r = b.rowIndices(rr)
          bldr.add(r, c, ring.negate(b.data(rr)))
          rr += 1
        }
        c += 1
      }
      bldr.result()
    }
  }


  protected def updateFromPure[T, Op<:OpType, Other](implicit op: UFunc.UImpl2[Op, CSCMatrix[T], Other, CSCMatrix[T]])
  : UFunc.InPlaceImpl2[Op, CSCMatrix[T], Other] = {
    new UFunc.InPlaceImpl2[Op, CSCMatrix[T], Other] {
      def apply(a: CSCMatrix[T], b: Other) {
        val result = op(a, b)
        a.use(result.data, result.colPtrs, result.rowIndices, result.activeSize)
      }
    }
  }

  implicit def canAddInPlaceM_S_Semiring[T: Semiring : ClassTag]: OpAdd.InPlaceImpl2[CSCMatrix[T], T] =
    updateFromPure(implicitly[OpAdd.Impl2[CSCMatrix[T], T, CSCMatrix[T]]])


  implicit def canSubInPlaceM_S_Ring[T: Ring : ClassTag]: OpSub.InPlaceImpl2[CSCMatrix[T], T] =
    updateFromPure(implicitly[OpSub.Impl2[CSCMatrix[T], T, CSCMatrix[T]]])

  implicit def canSetInPlaceM_S_Ring[T: Ring : ClassTag]: OpSet.InPlaceImpl2[CSCMatrix[T], T] =
    updateFromPure(implicitly[OpSet.Impl2[CSCMatrix[T], T, CSCMatrix[T]]])

}

/**
 * TODO
 *
 * @author dlwh
 **/
trait CSCMatrixOpsLowPrio extends SerializableLogging {
  this: CSCMatrixOps =>
  implicit def canMulM_V_def[T, A, B <: Vector[T]](implicit bb: B <:< Vector[T], op: OpMulMatrix.Impl2[CSCMatrix[T], Vector[T], Vector[T]]) =
    implicitly[OpMulMatrix.Impl2[CSCMatrix[T], Vector[T], Vector[T]]].asInstanceOf[breeze.linalg.operators.OpMulMatrix.Impl2[A, B, Vector[T]]]

  // ibid.
  implicit def canMulM_M_def[T, B <: Matrix[T]](implicit bb: B <:< Matrix[T], op: OpMulMatrix.Impl2[CSCMatrix[T], Matrix[T], CSCMatrix[T]]) =
    op.asInstanceOf[OpMulMatrix.Impl2[CSCMatrix[T], B, CSCMatrix[T]]]
}
