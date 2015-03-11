package breeze.linalg
package operators

import breeze.generic.UFunc
import breeze.linalg.support.{CanZipMapKeyValues, CanZipMapValues}
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
  implicit def csc_OpNeg[@expand.args(Int,Double,Float,Long) T]: OpNeg.Impl[CSCMatrix[T], CSCMatrix[T]] = {
    new OpNeg.Impl[CSCMatrix[T], CSCMatrix[T]] {
      def apply(a: CSCMatrix[T]): CSCMatrix[T] = {
        val acp = a.copy

        var c = 0
        while (c < acp.cols) {
          var ip = acp.colPtrs(c)
          while (ip < acp.colPtrs(c + 1)) {
            val r = acp.rowIndices(ip)
            acp.data(ip) = - acp.data(ip)
            ip += 1
          }
          c += 1
        }
        acp
      }
    }
  }

  @expand
  @expand.valify
  implicit def cscScaleAdd[@expand.args(Int,Double,Float,Long) T]: scaleAdd.InPlaceImpl3[CSCMatrix[T], T, CSCMatrix[T]] = {
    new scaleAdd.InPlaceImpl3[CSCMatrix[T], T, CSCMatrix[T]] {
      override def apply(a: CSCMatrix[T], s: T, b: CSCMatrix[T]): Unit = {
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val rows = a.rows
        val cols = a.cols

        if (cols == 0 || rows == 0) return

        val bldr = new CSCMatrix.Builder[T](rows,cols,max(a.activeSize,b.activeSize))
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
              bldr.add(ari, ci, a.data(ap) + (s * b.data(bp)))
              ap += 1
              bp += 1
            } else if (ari < bri) {
              // b is zero, so nothing is added to A
              bldr.add(ari, ci, a.data(ap))
              ap += 1
            } else /* ari > bri */ {
              bldr.add(bri, ci, s * b.data(bp))
              bp += 1
            }
          }
          ci = ci1
        }
        val res = bldr.result(true,true)
        a.use(res.data,res.colPtrs,res.rowIndices,res.activeSize)
      }
    }
  }

  @expand
  @expand.valify
  implicit def csc_csc_BadOps[@expand.args(Int,Double,Float,Long) T,
  @expand.args(OpPow,OpDiv,OpMod) Op <: OpType]
  (implicit @expand.sequence[Op]({_ pow _},{_ / _},{_ % _}) op: Op.Impl2[T,T,T],
            @expand.sequence[T](0, 0.0, 0.0f, 0l)  zero: T):
  Op.Impl2[CSCMatrix[T],CSCMatrix[T],CSCMatrix[T]] = {
    val mZero = implicitly[T](zero)
    def computeZeroOpOnRange(arr: Array[T],start: Int, end: Int) {
      var i = start
      while (i < end) {
        arr(i) = op(arr(i),mZero)
        i += 1
      }
    }
    new Op.Impl2[CSCMatrix[T],CSCMatrix[T],CSCMatrix[T]] {
      def apply(a: CSCMatrix[T],b: CSCMatrix[T]): CSCMatrix[T] = {
        val rows = a.rows
        val cols = a.cols
        require(rows == b.rows, "Matrices must have same number of rows!")
        require(cols == b.cols, "Matrices must have same number of cols!")

        val nData = Array.fill[T](rows*cols)(mZero)
        // fill in data from a
        var ci = 0
        var apStop = a.colPtrs(0)
        while (ci < cols) {
          val ci1 = ci + 1
          var ap = apStop
          apStop = a.colPtrs(ci1)
          while (ap < apStop) {
            val ar = a.rowIndices(ap)
            nData(ci * rows + ar) = a.data(ap)
            ap += 1
          }
          ci = ci1
        }

        // compute operations
        ci = 0
        var bpStop = b.colPtrs(0)
        while (ci < cols) {
          val ci1 = ci + 1
          var bp = bpStop
          bpStop = b.colPtrs(ci1)
          if (bp == bpStop) {
            // No data in column
            computeZeroOpOnRange(nData,ci * cols,ci1 * cols)
          } else {
            // data in column
            var rL = 0
            while (bp < bpStop) {
              val br = b.rowIndices(bp)
              val ndi = ci * rows + br
              if (rL < br-1)
                computeZeroOpOnRange(nData,ci * rows + rL, ndi)
              nData(ndi) = op(nData(ndi),b.data(bp))
              rL = br
              bp += 1
            }
          }
          ci = ci1
        }
        val colPtrs: Array[Int] = Array.tabulate[Int](cols + 1)((i: Int) => i * rows)
        val rowIndices: Array[Int] = Array.tabulate[Int](nData.length)((i: Int) => i % rows)
        new CSCMatrix[T](nData,rows,cols,colPtrs,nData.length,rowIndices)
      }
    }
  }

  @expand
  @expand.valify
  implicit def csc_csc_OpAdd[@expand.args(Int, Double, Float, Long) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l)  zero: T):
  OpAdd.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] = {
    new OpAdd.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: CSCMatrix[T]): CSCMatrix[T] = {
        require(a.rows == b.rows, "Matrix dimensions must match")
        require(a.cols == b.cols, "Matrix dimensions must match")
        val rows = a.rows
        val cols = a.cols
        if (cols == 0 || rows == 0) return CSCMatrix.zeros[T](rows, cols)

        if (a.activeSize == 0)
          b.copy
        else if (b.activeSize == 0)
          a.copy
        else {
          val bldr = new CSCMatrix.Builder[T](rows, cols, math.max(a.activeSize, b.activeSize))
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
                bldr.add(ari, ci, a.data(ap) + b.data(bp))
                ap += 1
                bp += 1
              } else if (ari < bri) {
                // next b row starts further down, therefore increase a pointer
                bldr.add(ari, ci, a.data(ap))
                ap += 1
              } else /* ari > bri */ {
                // next a row starts further down, therefore increase b pointer
                bldr.add(bri, ci, b.data(bp))
                bp += 1
              }
            }
            ci = ci1
          }

          bldr.result(true, true)
        }
      }
    }
  }


  @expand
  @expand.valify
  implicit def csc_dm_OpAdd[@expand.args(Int, Double, Float, Long) T]: OpAdd.Impl2[CSCMatrix[T], DenseMatrix[T], DenseMatrix[T]] = {
    new OpAdd.Impl2[CSCMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
      def apply(a: CSCMatrix[T], b: DenseMatrix[T]): DenseMatrix[T] = {
        require(a.rows == b.rows, "Matrix dimensions must match")
        require(a.cols == b.cols, "Matrix dimensions must match")
        val rows = a.rows
        val cols = a.cols
        if (cols == 0 || rows == 0) return DenseMatrix.zeros[T](rows, cols)


        val res = b.copy
        var ci = 0 // column index [0 ... cols)
        var apStop = a.colPtrs(0) // pointer into row indices and data
        while (ci < cols) {
          val ci1 = ci + 1
          var ap = apStop
          apStop = a.colPtrs(ci1)
          while (ap < apStop) {
            val ari = if (ap < apStop) a.rowIndices(ap) else rows // row index [0 ... rows)
            res(ari, ci) += a.data(ap)
            ap += 1
          }
          ci = ci1
        }

        res
      }
    }
  }

  @expand
  @expand.valify
  implicit def dm_csc_OpAdd[@expand.args(Int, Double, Float, Long) T]: OpAdd.Impl2[DenseMatrix[T], CSCMatrix[T], DenseMatrix[T]] = {
    new OpAdd.Impl2[DenseMatrix[T], CSCMatrix[T], DenseMatrix[T]] {
      def apply(a: DenseMatrix[T], b: CSCMatrix[T]): DenseMatrix[T] = {
        b + a
      }
    }
  }


  implicit def dm_csc_OpAdd_Semi[T:Semiring:ClassTag]: OpAdd.Impl2[DenseMatrix[T], CSCMatrix[T], DenseMatrix[T]] = {
    new OpAdd.Impl2[DenseMatrix[T], CSCMatrix[T], DenseMatrix[T]] {
      def apply(a: DenseMatrix[T], b: CSCMatrix[T]): DenseMatrix[T] = {
        b + a
      }
    }
  }

  implicit def csc_dm_Semi[T:Semiring:ClassTag]: OpAdd.Impl2[CSCMatrix[T], DenseMatrix[T], DenseMatrix[T]] = {
    new OpAdd.Impl2[CSCMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
      val semi = implicitly[Semiring[T]]
      def apply(a: CSCMatrix[T], b: DenseMatrix[T]): DenseMatrix[T] = {
        require(a.rows == b.rows, "Matrix dimensions must match")
        require(a.cols == b.cols, "Matrix dimensions must match")
        val rows = a.rows
        val cols = a.cols
        if (cols == 0 || rows == 0) return DenseMatrix.zeros[T](rows, cols)


        val res = b.copy
        var ci = 0 // column index [0 ... cols)
        var apStop = a.colPtrs(0) // pointer into row indices and data
        while (ci < cols) {
          val ci1 = ci + 1
          var ap = apStop
          apStop = a.colPtrs(ci1)
          while (ap < apStop) {
            val ari = if (ap < apStop) a.rowIndices(ap) else rows // row index [0 ... rows)
            res(ari, ci) = semi.+(res(ari, ci), a.data(ap))
            ap += 1
          }
          ci = ci1
        }

        res
      }
    }
  }

  // code based on that provided by sciss:
  // https://github.com/Sciss/breeze/blob/bb5cf8a1969545e1a7b0cd7ddde5f974be8301cd/math/src/main/scala/breeze/linalg/CSCMatrixExtraOps.scala
  @expand
  @expand.valify
  implicit def csc_csc_OpMulScalar[@expand.args(Int, Double, Float, Long) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l)  zero: T):
  OpMulScalar.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] = {
    new OpMulScalar.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: CSCMatrix[T]): CSCMatrix[T] = {
        val rows = a.rows
        val cols = a.cols
        require(rows == b.rows, "Matrices must have same number of rows!")
        require(cols == b.cols, "Matrices must have same number of cols!")

        if (cols == 0 || rows == 0) return CSCMatrix.zeros[T](rows, cols)

        if (a.activeSize == 0 || b.activeSize == 0)
          CSCMatrix.zeros[T](rows, cols)
        else {
          val res = new CSCMatrix.Builder[T](rows, cols, math.min(a.activeSize, b.activeSize))
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
                res.add(ari, ci, a.data(ap) * b.data(bp))
                ap += 1
                bp += 1
              } else if (ari < bri) {
                // next b row starts further down, therefore increase a pointer
                ap += 1
              } else /* ari > bri */ {
                // next a row starts further down, therefore increase b pointer
                bp += 1
              }
            }
            ci = ci1
          }
          res.result(true, true)
        }
      }
    }
  }

  @expand
  @expand.valify
  implicit def csc_csc_OpSub[@expand.args(Int, Double, Float, Long) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l)  zero: T):
  OpSub.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] = {
    new OpSub.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: CSCMatrix[T]): CSCMatrix[T] = {
        require(a.rows == b.rows, "Matrix dimensions must match")
        require(a.cols == b.cols, "Matrix dimensions must match")
        val rows = a.rows
        val cols = a.cols
        if (cols == 0 || rows == 0) return CSCMatrix.zeros[T](rows, cols)
        if (a.activeSize == 0)
          -b
        else if (b.activeSize == 0)
          a.copy
        else {
          val bldr = new CSCMatrix.Builder[T](rows, cols, math.max(a.activeSize, b.activeSize))
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
                val v = a.data(ap) - b.data(bp)
                bldr.add(ari, ci, v)
                ap += 1
                bp += 1
              } else if (ari < bri) {
                // next b row starts further down, therefore increase a pointer
                bldr.add(ari, ci, a.data(ap))
                ap += 1
              } else /* ari > bri */ {
                // next a row starts further down, therefore increase b pointer
                bldr.add(bri, ci, -b.data(bp))
                bp += 1
              }
            }
            ci = ci1
          }

          bldr.result(true, true)
        }

      }
    }
  }

  @expand
  @expand.valify
  implicit def implOps_CSCT_T_eq_CSCT[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpMulScalar, OpMulMatrix) Op<:OpType]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l) zero: T):
  Op.Impl2[CSCMatrix[T], T, CSCMatrix[T]] = {
    new Op.Impl2[CSCMatrix[T], T, CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: T): CSCMatrix[T] = {
        if (b == zero)
          return CSCMatrix.zeros[T](a.rows, a.cols)
        val data: Array[T] = Array.tabulate[T](a.data.length)(i => a.data(i) * b)
        new CSCMatrix[T](data, a.rows, a.cols, util.Arrays.copyOf(a.colPtrs, a.colPtrs.length), a.activeSize, util.Arrays.copyOf(a.rowIndices, a.rowIndices.length))
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

  // Update Ops
  protected def updateFromPure[T, Op<:OpType, Other](implicit op: UFunc.UImpl2[Op, CSCMatrix[T], Other, CSCMatrix[T]]): UFunc.InPlaceImpl2[Op, CSCMatrix[T], Other] = {
    new UFunc.InPlaceImpl2[Op, CSCMatrix[T], Other] {
      def apply(a: CSCMatrix[T], b: Other) {
        val result = op(a, b)
        a.use(result.data, result.colPtrs, result.rowIndices,result.activeSize)
      }
    }
  }

  @expand
  @expand.valify
  implicit def csc_T_InPlace[@expand.args(Int,Float,Double,Long) T, @expand.args(OpAdd, OpSub, OpDiv, OpPow, OpMod, OpMulScalar, OpMulMatrix) Op <: OpType]
  : Op.InPlaceImpl2[CSCMatrix[T],T] = updateFromPure(implicitly[Op.Impl2[CSCMatrix[T],T,CSCMatrix[T]]])

  @expand
  @expand.valify
  implicit def csc_csc_InPlace[@expand.args(Int,Float,Double,Long) T, @expand.args(OpAdd, OpSub, OpDiv, OpPow, OpMod, OpMulScalar) Op <: OpType]
  : Op.InPlaceImpl2[CSCMatrix[T],CSCMatrix[T]] = updateFromPure(implicitly[Op.Impl2[CSCMatrix[T],CSCMatrix[T],CSCMatrix[T]]])

}

trait CSCMatrixOps_Ring extends CSCMatrixOpsLowPrio with SerializableLogging {
  this: CSCMatrixOps =>

  implicit def csc_OpNeg[T:Ring:ClassTag]: OpNeg.Impl[CSCMatrix[T], CSCMatrix[T]] = {
    new OpNeg.Impl[CSCMatrix[T], CSCMatrix[T]] {
      val ring = implicitly[Ring[T]]
      def apply(a: CSCMatrix[T]): CSCMatrix[T] = {
        val acp = a.copy

        var c = 0
        while (c < acp.cols) {
          var ip = acp.colPtrs(c)
          while (ip < acp.colPtrs(c + 1)) {
            val r = acp.rowIndices(ip)
            acp.data(ip) = ring.negate(acp.data(ip))//(r, c, )
            ip += 1
          }
          c += 1
        }
        acp
      }
    }
  }

  implicit def cscScaleAdd[T: Semiring : ClassTag]: scaleAdd.InPlaceImpl3[CSCMatrix[T], T, CSCMatrix[T]] = {
    new scaleAdd.InPlaceImpl3[CSCMatrix[T], T, CSCMatrix[T]] {
      override def apply(a: CSCMatrix[T], s: T, b: CSCMatrix[T]): Unit = {
        val ring = implicitly[Semiring[T]]
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val rows = a.rows
        val cols = a.cols

        if (cols == 0 || rows == 0) return

        val bldr = new CSCMatrix.Builder[T](rows,cols,max(a.activeSize,b.activeSize))
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
              bldr.add(ari, ci, ring.+(a.data(ap), ring.*(s, b.data(bp))))
              ap += 1
              bp += 1
            } else if (ari < bri) {
              // b is zero, so nothing is added to A
              bldr.add(ari, ci, a.data(ap))
              ap += 1
            } else /* ari > bri */ {
              bldr.add(bri, ci, ring.*(s, b.data(bp)))
              bp += 1
            }
          }
          ci = ci1
        }
        val res = bldr.result(true,true)
        a.use(res.data,res.colPtrs,res.rowIndices,res.activeSize)
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

  implicit def zipMapVals[S, R: ClassTag : Semiring : Zero]: CanZipMapValues[CSCMatrix[S], S, R, CSCMatrix[R]] = new CanZipMapValues[CSCMatrix[S], S, R, CSCMatrix[R]] {
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

  implicit def zipMapKeyVals[S, R: ClassTag : Semiring : Zero]: CanZipMapKeyValues[CSCMatrix[S], (Int, Int), S, R, CSCMatrix[R]] = new CanZipMapKeyValues[CSCMatrix[S], (Int, Int), S, R, CSCMatrix[R]] {
    /** Maps all corresponding values from the two collections. */
    override def map(a: CSCMatrix[S], b: CSCMatrix[S], fn: ((Int, Int), S, S) => R): CSCMatrix[R] = {
      val rows = a.rows
      val cols = a.cols
      require(rows == b.rows, "Matrices must have same number of rows!")
      require(cols == b.cols, "Matrices must have same number of cols!")

      val builder = new CSCMatrix.Builder[R](rows, cols)
      for (c <- 0 until cols; r <- 0 until rows) {
        builder.add(r, c, fn((r, c), a(r, c), b(r, c)))
      }

      builder.result(true, true)
    }

    override def mapActive(a: CSCMatrix[S], b: CSCMatrix[S], fn: ((Int, Int), S, S) => R): CSCMatrix[R] = {
      // TODO: sparsify this
      val rows = a.rows
      val cols = a.cols
      require(rows == b.rows, "Matrices must have same number of rows!")
      require(cols == b.cols, "Matrices must have same number of cols!")

      val builder = new CSCMatrix.Builder[R](rows, cols)
      for (c <- 0 until cols; r <- 0 until rows) {
        builder.add(r, c, fn((r, c), a(r, c), b(r, c)))
      }

      builder.result(true, true)
    }
  }

  implicit def canAddM_S_Semiring[T: Semiring : ClassTag]: OpAdd.Impl2[CSCMatrix[T], T, CSCMatrix[T]] =
    new OpAdd.Impl2[CSCMatrix[T], T, CSCMatrix[T]] {
      val s = implicitly[Semiring[T]]
      val zero = s.zero

      override def apply(v: CSCMatrix[T], v2: T): CSCMatrix[T] = {
        if (v2 == zero) return copy(v)
        val data = Array.fill[T](v.rows * v.cols)(v2)
        var c = 0
        while (c < v.cols) {
          var ip = v.colPtrs(c)
          while (ip < v.colPtrs(c + 1)) {
            val r = v.rowIndices(ip)
            data(c * v.rows + r) = s.+(v.data(ip),v2)
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
        val data = Array.fill[T](v.rows * v.cols)(s.negate(v2))
        var c = 0
        while (c < v.cols) {
          var ip = v.colPtrs(c)
          while (ip < v.colPtrs(c + 1)) {
            val r = v.rowIndices(ip)
            data(c * v.rows + r) = s.-(v.data(ip),v2)
            ip += 1
          }
          c += 1
        }
        val colPtrs: Array[Int] = Array.tabulate[Int](v.cols + 1)((i: Int) => i * v.rows)
        val rowIndices: Array[Int] = Array.tabulate[Int](data.length)((i: Int) => i % v.rows)
        new CSCMatrix[T](data,v.rows,v.cols,colPtrs,data.length,rowIndices)
      }
    }


  implicit def canSetM_S_Semiring[T: Semiring : ClassTag]: OpSet.Impl2[CSCMatrix[T], T, CSCMatrix[T]] =
    new OpSet.Impl2[CSCMatrix[T], T, CSCMatrix[T]] {
      val r = implicitly[Semiring[T]]
      val zero = r.zero
      def apply(v: CSCMatrix[T], v2: T): CSCMatrix[T] = {
        if (v2 == zero)
          return CSCMatrix.zeros[T](v.rows,v.cols)
        val data: Array[T] = Array.fill[T](v.rows * v.cols)(v2)
        val colPtrs: Array[Int] = Array.tabulate[Int](v.cols + 1)((i: Int) => i * v.rows)
        val rowIndices: Array[Int] = Array.tabulate[Int](data.length)((i: Int) => i % v.rows)
        new CSCMatrix[T](data,v.rows,v.cols,colPtrs,v.rows*v.cols,rowIndices)
      }
    }

  @expand
  implicit def canMulM_S_Ring[@expand.args(OpMulMatrix,OpMulScalar) Op <: OpType, T:Ring:ClassTag]: Op.Impl2[CSCMatrix[T],T,CSCMatrix[T]] = {
    val r = implicitly[Ring[T]]
    new Op.Impl2[CSCMatrix[T], T, CSCMatrix[T]] {
      def apply(v: CSCMatrix[T], v2: T): CSCMatrix[T] = {
        if (v2 == r.zero)
          return CSCMatrix.zeros[T](v.rows,v.cols)
        val data: Array[T] = Array.tabulate[T](v.data.length)(i => r.*(v.data(i),v2))
        new CSCMatrix[T](data,v.rows,v.cols,util.Arrays.copyOf(v.colPtrs,v.colPtrs.length),v.activeSize,util.Arrays.copyOf(v.rowIndices,v.rowIndices.length))
      }
    }
  }

  implicit def CSCMatrixCanMulScalarM_M_Semiring[A: Semiring : ClassTag : Zero]: OpMulScalar.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] =
    new OpMulScalar.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] {
      val ring = implicitly[Semiring[A]]
      final def apply(a: CSCMatrix[A], b: CSCMatrix[A]): CSCMatrix[A] = {
        val rows = a.rows
        val cols = a.cols
        require(rows == b.rows, "Matrices must have same number of rows!")
        require(cols == b.cols, "Matrices must have same number of cols!")

        if (cols == 0 || rows == 0) return CSCMatrix.zeros[A](rows, cols)

        if (a.activeSize == 0 || b.activeSize == 0)
          CSCMatrix.zeros[A](rows, cols)
        else {
          val res = new CSCMatrix.Builder[A](rows, cols, math.min(a.activeSize, b.activeSize))
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
                res.add(ari, ci, ring.*(a.data(ap), b.data(bp)))
                ap += 1
                bp += 1
              } else if (ari < bri) {
                // next b row starts further down, therefore increase a pointer
                ap += 1
              } else /* ari > bri */ {
                // next a row starts further down, therefore increase b pointer
                bp += 1
              }
            }
            ci = ci1
          }
          res.result(true, true)
        }
      }
    }

  implicit def CSCMatrixCanAdd_M_M_Semiring[A:Semiring:Zero:ClassTag]: OpAdd.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] =
    new OpAdd.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] {
    val ring = implicitly[Semiring[A]]
    def apply(a: CSCMatrix[A], b: CSCMatrix[A]): CSCMatrix[A] = {
      require(a.rows == b.rows, "Matrix dimensions must match")
      require(a.cols == b.cols, "Matrix dimensions must match")
      val rows = a.rows
      val cols = a.cols
      if (cols == 0 || rows == 0) return CSCMatrix.zeros[A](rows, cols)

      if (a.activeSize == 0)
        b.copy
      else if (b.activeSize == 0)
        a.copy
      else {
        val bldr = new CSCMatrix.Builder[A](rows, cols, math.max(a.activeSize, b.activeSize))
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
              bldr.add(ari, ci, ring.+(a.data(ap), b.data(bp)))
              ap += 1
              bp += 1
            } else if (ari < bri) {
              // next b row starts further down, therefore increase a pointer
              bldr.add(ari, ci, a.data(ap))
              ap += 1
            } else /* ari > bri */ {
              // next a row starts further down, therefore increase b pointer
              bldr.add(bri, ci, b.data(bp))
              bp += 1
            }
          }
          ci = ci1
        }

        bldr.result(true, true)
      }
    }
  }

  implicit def CSCMatrixCanSubM_M_Ring[A:Ring:Zero:ClassTag]: OpSub.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] = new OpSub.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] {
    val ring = implicitly[Ring[A]]
    def apply(a: CSCMatrix[A], b: CSCMatrix[A]): CSCMatrix[A] = {
      require(a.rows == b.rows, "Matrix dimensions must match")
      require(a.cols == b.cols, "Matrix dimensions must match")
      val rows = a.rows
      val cols = a.cols
      if (cols == 0 || rows == 0) return CSCMatrix.zeros[A](rows, cols)
      if (a.activeSize == 0)
        -b
      else if (b.activeSize == 0)
        a.copy
      else {
        val bldr = new CSCMatrix.Builder[A](rows, cols, math.max(a.activeSize, b.activeSize))
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
              val v = ring.-(a.data(ap), b.data(bp))
              bldr.add(ari, ci, v)
              ap += 1
              bp += 1
            } else if (ari < bri) {
              // next b row starts further down, therefore increase a pointer
              bldr.add(ari, ci, a.data(ap))
              ap += 1
            } else /* ari > bri */ {
              // next a row starts further down, therefore increase b pointer
              bldr.add(bri, ci, ring.negate(b.data(bp)))
              bp += 1
            }
          }
          ci = ci1
        }

        bldr.result(true, true)
      }
    }
  }

  @expand
  implicit def csc_T_Op[@expand.args(OpDiv, OpMod, OpPow) Op <: OpType, T:Field:ClassTag]
  (implicit @expand.sequence[Op]({f./(_,_)}, {f.%(_,_)},{f.pow(_,_)}) op: Op.Impl2[T,T,T]):
  Op.Impl2[CSCMatrix[T], T, CSCMatrix[T]] = {
    val f = implicitly[Field[T]]
    new Op.Impl2[CSCMatrix[T], T, CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: T): CSCMatrix[T] = {
        if (b == f.zero) {
          // degenerate case, creates effectively dense matrix
          val default = op(f.zero, b)
          val data: Array[T] = Array.fill[T](a.rows * a.cols)(default)
          val colPtrs: Array[Int] = Array.tabulate[Int](a.cols + 1)((i: Int) => i * a.rows)
          val rowIndices: Array[Int] = Array.tabulate[Int](data.length)((i: Int) => i % a.rows)
          var c = 0
          while (c < a.cols) {
            var ip = a.colPtrs(c)
            while (ip < a.colPtrs(c + 1)) {
              val r = a.rowIndices(ip)
              data(c * a.rows + r) = op(a.data(ip), b)
              ip += 1
            }
            c += 1
          }
          new CSCMatrix[T](data, a.rows, a.cols, colPtrs, a.rows * a.cols, rowIndices)
        } else {
          val bldr = new CSCMatrix.Builder[T](a.rows, a.cols, a.activeSize)
          var c = 0
          while (c < a.cols) {
            var ip = a.colPtrs(c)
            while (ip < a.colPtrs(c + 1)) {
              val r = a.rowIndices(ip)
              bldr.add(r, c, op(a.data(ip), b))
              ip += 1
            }
            c += 1
          }
          bldr.result(true, true)
        }
      }
    }
  }

  @expand
  implicit def csc_csc_BadOp[@expand.args(OpDiv, OpMod, OpPow) Op <: OpType, T:Field:ClassTag]
  (implicit @expand.sequence[Op]({f./(_,_)},{f.%(_,_)}, {f.pow(_,_)}) op: Op.Impl2[T,T,T]):
  Op.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] = {
    val f = implicitly[Field[T]]
    def computeZeroOpOnRange(arr: Array[T],start: Int, end: Int) {
      var i = start
      while (i < end) {
        arr(i) = op(arr(i),f.zero)
        i += 1
      }
    }
    new Op.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: CSCMatrix[T]): CSCMatrix[T] = {
        val rows = a.rows
        val cols = a.cols
        require(rows == b.rows, "Matrices must have same number of rows!")
        require(cols == b.cols, "Matrices must have same number of cols!")

        val nData = Array.fill[T](rows*cols)(f.zero)
        // fill in data from a
        var ci = 0
        var apStop = a.colPtrs(0)
        while (ci < cols) {
          val ci1 = ci + 1
          var ap = apStop
          apStop = a.colPtrs(ci1)
          while (ap < apStop) {
            val ar = a.rowIndices(ap)
            nData(ci * rows + ar) = a.data(ap)
            ap += 1
          }
          ci = ci1
        }

        // compute operations
        ci = 0
        var bpStop = b.colPtrs(0)
        while (ci < cols) {
          val ci1 = ci + 1
          var bp = bpStop
          bpStop = b.colPtrs(ci1)
          if (bp == bpStop) {
            // No data in column
            computeZeroOpOnRange(nData,ci * cols,ci1 * cols)
          } else {
            // data in column
            var rL = 0
            while (bp < bpStop) {
              val br = b.rowIndices(bp)
              val ndi = ci * rows + br
              if (rL < br-1)
                computeZeroOpOnRange(nData,ci * rows + rL, ndi)
              nData(ndi) = op(nData(ndi),b.data(bp))
              rL = br
              bp += 1
            }
          }
          ci = ci1
        }
        val colPtrs: Array[Int] = Array.tabulate[Int](cols + 1)((i: Int) => i * rows)
        val rowIndices: Array[Int] = Array.tabulate[Int](nData.length)((i: Int) => i % rows)
        new CSCMatrix[T](nData,rows,cols,colPtrs,nData.length,rowIndices)
      }
    }
  }

  implicit def CSCMatrixCanSetM_M_Semiring[T:Semiring:ClassTag]: OpSet.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] = {
    val f = implicitly[Semiring[T]]
    new OpSet.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: CSCMatrix[T]): CSCMatrix[T] = {
        val rows  = a.rows
        val cols  = a.cols
        require(rows == b.rows, "Matrices must have same number of rows!")
        require(cols == b.cols, "Matrices must have same number of cols!")
        b.copy
      }
    }
  }

  protected def updateFromPure_CSC_T[T, Op<:OpType, Other](implicit op: UFunc.UImpl2[Op, CSCMatrix[T], Other, CSCMatrix[T]])
  : UFunc.InPlaceImpl2[Op, CSCMatrix[T], Other] = {
    new UFunc.InPlaceImpl2[Op, CSCMatrix[T], Other] {
      def apply(a: CSCMatrix[T], b: Other) {
        val result = op(a, b)
        a.use(result.data, result.colPtrs, result.rowIndices, result.activeSize)
      }
    }
  }
  protected def updateFromPure_CSC_CSC[T, Op<:OpType](implicit op: UFunc.UImpl2[Op, CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]])
  : UFunc.InPlaceImpl2[Op, CSCMatrix[T], CSCMatrix[T]] = {
    new UFunc.InPlaceImpl2[Op, CSCMatrix[T], CSCMatrix[T]] {
      def apply(a: CSCMatrix[T], b: CSCMatrix[T]) {
        val result = op(a, b)
        a.use(result.data, result.colPtrs, result.rowIndices, result.activeSize)
      }
    }
  }

  @expand
  implicit def csc_csc_UpdateOp[@expand.args(OpAdd, OpSub, OpMulScalar, OpSet, OpDiv,OpPow, OpMod) Op <: OpType, T:Field:ClassTag]
  :Op.InPlaceImpl2[CSCMatrix[T], CSCMatrix[T]] = updateFromPure_CSC_CSC(implicitly[Op.Impl2[CSCMatrix[T],CSCMatrix[T],CSCMatrix[T]]])

//  implicit def canAddInPlaceM_S_Semiring[T: Semiring : ClassTag]: OpAdd.InPlaceImpl2[CSCMatrix[T], T] =
//    updateFromPure_CSC_T(implicitly[OpAdd.Impl2[CSCMatrix[T], T, CSCMatrix[T]]])
//
//
//  implicit def canSubInPlaceM_S_Ring[T: Ring : ClassTag]: OpSub.InPlaceImpl2[CSCMatrix[T], T] =
//    updateFromPure_CSC_T(implicitly[OpSub.Impl2[CSCMatrix[T], T, CSCMatrix[T]]])
//
//  implicit def canSetInPlaceM_S_Ring[T: Ring : ClassTag]: OpSet.InPlaceImpl2[CSCMatrix[T], T] =
//    updateFromPure_CSC_T(implicitly[OpSet.Impl2[CSCMatrix[T], T, CSCMatrix[T]]])

  @expand
  implicit def csc_T_UpdateOp[@expand.args(OpMulMatrix, OpSet, OpSub, OpAdd, OpMulScalar, OpDiv, OpMod, OpPow) Op <: OpType, T:Field:ClassTag]
  : Op.InPlaceImpl2[CSCMatrix[T],T] = {
    updateFromPure_CSC_T(implicitly[Op.Impl2[CSCMatrix[T], T, CSCMatrix[T]]])
  }
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
