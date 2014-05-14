package breeze.linalg
package operators

import breeze.macros.expand
import breeze.linalg.{DenseMatrix, SparseVector}
import breeze.math._
import breeze.storage.DefaultArrayValue
import scala.reflect.ClassTag
import java.util

/**
 * TODO
 *
 * @author dlwh
 **/
trait CSCMatrixOps extends CSCMatrixOpsLowPrio {  this: CSCMatrix.type =>
  // don't remove
  import breeze.math.PowImplicits._

  @expand
  @expand.valify
  implicit def canMulM_V[@expand.args(Int, Float, Double, Long, Complex) T]: BinaryRegistry[CSCMatrix[T], Vector[T],OpMulMatrix.type, Vector[T]] = new BinaryRegistry[CSCMatrix[T], Vector[T], OpMulMatrix.type, Vector[T]] {
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
  implicit def canMulM_DV[@expand.args(Int, Float, Double, Long, Complex) T]: BinaryRegistry[CSCMatrix[T], DenseVector[T], OpMulMatrix.type, DenseVector[T]] = new BinaryRegistry[CSCMatrix[T], DenseVector[T], OpMulMatrix.type, DenseVector[T]] {
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
  implicit def canMulM_SV[@expand.args(Int, Float, Double, Long, Complex) T]: BinaryRegistry[CSCMatrix[T], SparseVector[T], OpMulMatrix.type, SparseVector[T]] = new BinaryRegistry[CSCMatrix[T], SparseVector[T], OpMulMatrix.type, SparseVector[T]] {
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
  implicit def canMulM_DM[@expand.args(Int, Float, Double, Long, Complex) T]
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
  implicit def canMulM_M[@expand.args(Int, Float, Double, Long, Complex) T]: breeze.linalg.operators.OpMulMatrix.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] = new breeze.linalg.operators.OpMulMatrix.Impl2[CSCMatrix[T], CSCMatrix[T], CSCMatrix[T]] {
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
        while (ap < apStop && bp < bpStop) {
          val ari = a.rowIndices(ap)  // row index [0 ... rows)
          val bri = b.rowIndices(bp)
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

      res.result(true, true)
    }

    implicitly[BinaryRegistry[Matrix[A], Matrix[A], OpMulScalar.type, Matrix[A]]].register(this)
  }

  implicit def CSCMatrixCanAdd_M_M[A:Semiring:DefaultArrayValue:ClassTag]: OpAdd.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] = new OpAdd.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] {
    def apply(a: CSCMatrix[A], b: CSCMatrix[A]): CSCMatrix[A] = {
      val bldr = CSCMatrix.Builder.fromMatrix(a)
      bldr.sizeHint(a.activeSize + b.activeSize)
      var c = 0
      while(c < b.cols) {
        var rr = b.colPtrs(c)
        val rrlast = b.colPtrs(c+1)
        while (rr < rrlast) {
          val r = b.rowIndices(rr)
          bldr.add(r, c, b.data(rr))
          rr += 1
        }
        c += 1
      }
      bldr.result()
    }
  }

  implicit def CSCMatrixCanSubM_M[A:Ring:DefaultArrayValue:ClassTag]: OpSub.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] = new OpSub.Impl2[CSCMatrix[A], CSCMatrix[A], CSCMatrix[A]] {
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

}

/**
 * TODO
 *
 * @author dlwh
 **/
trait CSCMatrixOpsLowPrio { this: CSCMatrixOps =>
  @expand
  implicit def canMulM_V_def[@expand.args(Int, Float, Double, Long, Complex) T, A, B](implicit bb :  B <:< Vector[T]) = (
    implicitly[OpMulMatrix.Impl2[CSCMatrix[T], Vector[T], Vector[T]]].asInstanceOf[breeze.linalg.operators.OpMulMatrix.Impl2[A, B, Vector[T]]]
    )


}


