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
import breeze.linalg.operators._
import breeze.storage.DefaultArrayValue
import java.util
import breeze.util.{ Terminal, ArrayUtil }
import scala.collection.mutable
import breeze.math.{ Complex, Semiring }
import breeze.generic.CanMapValues
import scala.reflect.ClassTag
import breeze.macros.{expandArgs, expand}
import scala.math.BigInt

/**
 * A compressed sparse column matrix, as used in Matlab and CSparse, etc.
 *
 * Most implementations based on "Direct Methods for Sparse Linear Systems"
 * by Timothy A. Davis
 * @author dlwh
 */
// TODO: maybe put columns in own array of sparse vectors, making slicing easier?
class CSCMatrix[@specialized(Int, Float, Double) V:DefaultArrayValue] private[linalg] (private var _data: Array[V],
                                                                               val rows: Int,
                                                                               val cols: Int,
                                                                               val colPtrs: Array[Int], // len cols + 1
                                                                               private var used : Int,
                                                                               private var _rowIndices: Array[Int]) // len >= used
  extends Matrix[V] with MatrixLike[V, CSCMatrix[V]] {

  def rowIndices = _rowIndices
  def data = _data


  def apply(row: Int, col: Int): V = {
    if(row >= rows || col >= cols || row < 0 || col < 0)
      throw new IndexOutOfBoundsException()
    val ind = locate(row, col)
    if(ind < 0) zero
    else data(ind)
  }

  def update(row: Int, col: Int, v: V) {
    if(row >= rows || col >= cols || row < 0 || col < 0)
      throw new IndexOutOfBoundsException()
    val ind = locate(row, col)
    if(ind >= 0) data(ind) = v
    else if (v != zero) {
      val insertPos = ~ind
      used += 1

      if (used > data.length) {
        // need to grow array
        val newLength = {
          if      (data.length == 0)     { 4 }
          else if (data.length < 0x0400) { data.length * 2 }
          else if (data.length < 0x0800) { data.length + 0x0400 }
          else if (data.length < 0x1000) { data.length + 0x0800 }
          else if (data.length < 0x2000) { data.length + 0x1000 }
          else if (data.length < 0x4000) { data.length + 0x2000 }
          else { data.length + 0x4000 }
        }

        // allocate new arrays
        val newIndex = util.Arrays.copyOf(rowIndices, newLength)
        val newData  = ArrayUtil.copyOf(data, newLength)

        // copy existing data into new arrays
        System.arraycopy(_rowIndices, insertPos, newIndex, insertPos + 1, used - insertPos - 1)
        System.arraycopy(data,  insertPos, newData,  insertPos + 1, used - insertPos - 1)

        // update pointers
        _rowIndices = newIndex
        _data = newData
      } else if (used - insertPos > 1) {
        // need to make room for new element mid-array
        System.arraycopy(_rowIndices, insertPos, _rowIndices, insertPos + 1, used - insertPos - 1)
        System.arraycopy(data,  insertPos, data,  insertPos + 1, used - insertPos - 1)
      }

      // assign new value
      rowIndices(insertPos) = row
      data(insertPos) = v
      for(c <- (col+1) to cols) {
        colPtrs(c) += 1
      }
    }
  }

  def reserve(nnz: Int) {
    if(nnz >= used && nnz != rowIndices.length)  {
      _rowIndices = util.Arrays.copyOf(rowIndices, nnz)
      _data = ArrayUtil.copyOf(data, nnz)
    }
  }

  def compact() {
    reserve(used)
  }

  def activeKeysIterator: Iterator[(Int, Int)] = {
    for(c <- Iterator.range(0, cols); rr <- Iterator.range(colPtrs(c),colPtrs(c+1))) yield (rowIndices(rr), c)
  }

  def activeIterator: Iterator[((Int, Int), V)] = {
    for(c <- Iterator.range(0, cols); rr <- Iterator.range(colPtrs(c),colPtrs(c+1))) yield (rowIndices(rr), c) -> data(rr)
  }

  def activeValuesIterator: Iterator[V] = data.iterator.take(used)

  def activeSize: Int = used

  def repr: CSCMatrix[V] = this

  private def locate(row: Int, col: Int):Int = {
    val start = colPtrs(col)
    val end = colPtrs(col+1)
    util.Arrays.binarySearch(rowIndices, start, end, row)
  }


  private def zero = implicitly[DefaultArrayValue[V]].value


  override def toString(maxLines: Int, maxWidth: Int): String = {
    val buf = new StringBuilder()
    buf ++= ("%d x %d CSCMatrix".format(rows, cols))
    activeIterator.take(maxLines - 1).foreach { case ((r,c),v) =>
      buf += '\n'
      buf ++= "(%d,%d) ".format(r,c)
      buf ++= v.toString
    }
    buf.toString()
  }


  override def equals(p1: Any): Boolean = p1 match {
    case m:Matrix[V] if m.rows == rows && m.cols == cols => valuesIterator.sameElements(m.valuesIterator)
    case _ => false
  }

  override def toString: String = toString(maxLines = Terminal.terminalHeight - 3)
}

object CSCMatrix extends MatrixConstructors[CSCMatrix]  with CSCMatrixOps {
  def zeros[@specialized(Int, Float, Double) V:ClassTag:DefaultArrayValue](rows: Int, cols: Int, initialNonzero: Int) = {
    new CSCMatrix[V](new Array(initialNonzero), rows, cols, new Array(cols + 1), 0, new Array(initialNonzero))
  }

  def zeros[@specialized(Int, Float, Double) V: ClassTag : DefaultArrayValue](rows: Int, cols: Int): CSCMatrix[V] = zeros(rows, cols, 0)

  def create[@specialized(Int, Float, Double) V: DefaultArrayValue](rows: Int, cols: Int, data: Array[V]): CSCMatrix[V] = {
    val z = implicitly[DefaultArrayValue[V]].value
    implicit val man = ClassTag[V](data.getClass.getComponentType.asInstanceOf[Class[V]])
    val res = zeros(rows, cols, data.length)
    var i = 0
    for(c <- 0 until cols; r <- 0 until rows) {
      val v = data(i)
      i += 1
      if ( v != z) {
        res(r, c) = v
      }
    }
    // TODO: res.compact()
    res
  }

  implicit def canMapValues[V, R:ClassTag:DefaultArrayValue:Semiring] = {
    val z = implicitly[DefaultArrayValue[R]].value
    new CanMapValues[CSCMatrix[V],V,R,CSCMatrix[R]] {
      override def map(from : CSCMatrix[V], fn : (V=>R)) = {
        val fz = fn(from.zero)
        val fzIsNotZero = fz != z
        val builder = new Builder[R](from.rows, from.cols, from.activeSize)
        var j = 0
        while(j < from.cols) {
          var ip = from.colPtrs(j)
          var lastI = 0
          while(ip < from.colPtrs(j+1)) {
            val i = from.rowIndices(ip)
            while(fzIsNotZero && lastI < i) {
              builder.add(lastI, j, fz)
              lastI += 1
            }
            lastI += 1
            val v = from.data(ip)
            val r = fn(v)
            if (r != z) {
              builder.add(i, j, r)
            }
            ip += 1
          }

          while(fzIsNotZero && lastI < from.rows) {
            builder.add(lastI, j, fz)
              lastI += 1
          }
          j += 1
        }

        builder.result()
      }

      override def mapActive(from : CSCMatrix[V], fn : (V=>R)) = {
        var zeroSeen = false
        def ff(v: V) = { val r = fn(v); if (r == z) zeroSeen = true; r}
        val newData = from.data.map(ff)
        val r = new CSCMatrix[R](newData, from.rows, from.cols, from.colPtrs.clone(), from.activeSize, from.rowIndices.clone)
        if(zeroSeen) r.compact()
        r
      }
    }
  }
  
  implicit def canTranspose[V:ClassTag:DefaultArrayValue]: CanTranspose[CSCMatrix[V], CSCMatrix[V]] = {
    new CanTranspose[CSCMatrix[V], CSCMatrix[V]] {
      def apply(from: CSCMatrix[V]) = {
        val transposedMtx = CSCMatrix.zeros[V](from.cols, from.rows)
        
        var j = 0
        while(j < from.cols) {
          var ip = from.colPtrs(j)
          while(ip < from.colPtrs(j+1)) {
            val i = from.rowIndices(ip)
            transposedMtx(j, i) = from.data(ip)
            ip += 1
          }
          j += 1
        }
        transposedMtx
      }
    }
  }
    
  implicit def canTransposeComplex: CanTranspose[CSCMatrix[Complex], CSCMatrix[Complex]] = {
    new CanTranspose[CSCMatrix[Complex], CSCMatrix[Complex]] {
      def apply(from: CSCMatrix[Complex]) = {
        val transposedMtx = CSCMatrix.zeros[Complex](from.cols, from.rows)
        
        var j = 0
        while(j < from.cols) {
          var ip = from.colPtrs(j)
          while(ip < from.colPtrs(j+1)) {
            val i = from.rowIndices(ip)
            transposedMtx(j, i) = from.data(ip).conjugate
            ip += 1
          }
          j += 1
        }
        transposedMtx
      }
    }
  }
    

  /**
   * This is basically an unsorted coordinate matrix.
   * @param initNnz initial number of nonzero entries
   */
  class Builder[@specialized(Int, Float, Double) T:ClassTag:Semiring:DefaultArrayValue](rows: Int, cols: Int, initNnz: Int = 16) {
    private def ring = implicitly[Semiring[T]]
    def add(r: Int, c: Int, v: T) {
      rs += r
      cs += c
      vs += v
    }

    private val rs = new mutable.ArrayBuilder.ofInt()
    rs.sizeHint(initNnz)
    private val cs = new mutable.ArrayBuilder.ofInt()
    cs.sizeHint(initNnz)
    private val vs = mutable.ArrayBuilder.make[T]()
    vs.sizeHint(initNnz)

    def result:CSCMatrix[T] = result(false, false)

    def result(keysAlreadyUnique: Boolean = false, keysAlreadySorted: Boolean = false):CSCMatrix[T] = {
      val rs = this.rs.result()
      val cs = this.cs.result()
      val vs = this.vs.result()
      // at most this many nnz
      val nnz = rs.length

      val order: Array[Int] = if(keysAlreadySorted) {
        VectorBuilder.range(cs.length)
      } else {
        sortedIndices(rs, cs)
      }

      val outRows = new Array[Int](nnz)
      val outCols = new Array[Int](cols+1)
      val outData = new Array[T](nnz)

      if (cs.length > 0) {
        outRows(0) = rs(order(0))
        outData(0) = vs(order(0))
      }
      var outDataIndex = 0
      var i = 1
      var lastCol = cs(order(0))
      while (i < nnz) {
        val colsEqual = cs(order(i)) == lastCol
        if (colsEqual && rs(order(i)) == rs(order(i-1))) {
          // TODO: might need to codegen to make this fast.
          outData(outDataIndex) = ring.+(outData(outDataIndex), vs(order(i)))
        } else {
          outDataIndex += 1
          outRows(outDataIndex) = rs(order(i))
          outData(outDataIndex) = vs(order(i))
        }

        // we need the outCols array to point to the
        if(!colsEqual) {
          while(lastCol < cs(order(i))) {
            outCols(lastCol+1) = outDataIndex
            lastCol += 1
          }
        }

        i += 1
      }
      outDataIndex += 1

      while(lastCol < cols) {
        outCols(lastCol+1) = outDataIndex
        lastCol += 1
      }

      val out = new CSCMatrix[T](outData, rows, cols, outCols, outDataIndex, outRows)
      if(!keysAlreadyUnique)
        out.compact()
      out
    }

    // returns indices of a lexicgraphic sort of cs then rs
    private def sortedIndices(rs: Array[Int], cs: Array[Int]) = {
      Array.range(0, rs.length).sortWith { (i, j) =>
        (cs(i) < cs(j))  || (cs(i) == cs(j) && rs(i) < rs(j))
      }
    }
  }
}


trait CSCMatrixOpsLowPrio { this: CSCMatrixOps =>
  @expand
  implicit def canMulM_V_def[@expandArgs(Int, Float, Double, Long, Complex, BigInt) T, A, B](implicit bb :  B <:< Vector[T]):BinaryOp[A, B, OpMulMatrix, DenseVector[T]] = (
    implicitly[BinaryOp[CSCMatrix[T], Vector[T], OpMulMatrix, DenseVector[T]]].asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseVector[T]]]
    )
}

trait CSCMatrixOps extends CSCMatrixOpsLowPrio {  this: CSCMatrix.type =>

  @expand
  @expand.valify
  implicit def canMulM_V[@expandArgs(Int, Float, Double, Long, Complex, BigInt) T]: BinaryRegistry[CSCMatrix[T], Vector[T],OpMulMatrix, Vector[T]] = new BinaryRegistry[CSCMatrix[T], Vector[T], OpMulMatrix, Vector[T]] {
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
//    implicitly[BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  implicit def canMulM_DV[@expandArgs(Int, Float, Double, Long, Complex, BigInt) T]: BinaryRegistry[CSCMatrix[T], DenseVector[T],OpMulMatrix, DenseVector[T]] = new BinaryRegistry[CSCMatrix[T], DenseVector[T], OpMulMatrix, DenseVector[T]] {
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
    //    implicitly[BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  implicit def canMulM_SV[@expandArgs(Int, Float, Double, Long, Complex, BigInt) T]: BinaryRegistry[CSCMatrix[T], SparseVector[T],OpMulMatrix, SparseVector[T]] = new BinaryRegistry[CSCMatrix[T], SparseVector[T], OpMulMatrix, SparseVector[T]] {
    override def bindingMissing(a: CSCMatrix[T], b: SparseVector[T]) = {
      require(a.cols == b.length, "Dimension Mismatch!")
      val res = new VectorBuilder[T](a.rows, b.iterableSize min a.rows)
      var c = 0
      var lastOffset = 0
      while(c < a.cols) {
        var rr = a.colPtrs(c)
        val rrlast = a.colPtrs(c+1)
        if(rr < rrlast) {
          val newBOffset = util.Arrays.binarySearch(b.index, lastOffset, math.max(b.activeSize, c+1), c)
          println(c, b.index.toIndexedSeq, lastOffset, b.activeSize, newBOffset, ~newBOffset)
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
    //    implicitly[BinaryRegistry[Matrix[T], Vector[T], OpMulMatrix, Vector[T]]].register(this)
  }



  @expand
  @expand.valify
  implicit def canMulM_DM[@expandArgs(Int, Float, Double, Long, Complex, BigInt) T]
  : BinaryOp[CSCMatrix[T], DenseMatrix[T], breeze.linalg.operators.OpMulMatrix, DenseMatrix[T]]= new BinaryOp[CSCMatrix[T], DenseMatrix[T], breeze.linalg.operators.OpMulMatrix, DenseMatrix[T]] {
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
//    implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix, Matrix[T]]].register(this)
  }

  @expand
  @expand.valify
  implicit def canMulDM_M[@expandArgs(Int, Float, Double, Long, Complex, BigInt) T]:BinaryOp[DenseMatrix[T], CSCMatrix[T], breeze.linalg.operators.OpMulMatrix, DenseMatrix[T]] = new BinaryOp[DenseMatrix[T], CSCMatrix[T], breeze.linalg.operators.OpMulMatrix, DenseMatrix[T]] {
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
//    implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix, Matrix[T]]].register(this)
  }


  @expand
  @expand.valify
  implicit def canMulM_M[@expandArgs(Int, Float, Double, Long, Complex, BigInt) T]: BinaryOp[CSCMatrix[T], CSCMatrix[T], breeze.linalg.operators.OpMulMatrix, CSCMatrix[T]] = new BinaryOp[CSCMatrix[T], CSCMatrix[T], breeze.linalg.operators.OpMulMatrix, CSCMatrix[T]] {
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
//    implicitly[BinaryRegistry[Matrix[T], Matrix[T], OpMulMatrix, Matrix[T]]].register(this)
  }


  // code based on that provided by sciss:
  // https://github.com/Sciss/breeze/blob/bb5cf8a1969545e1a7b0cd7ddde5f974be8301cd/math/src/main/scala/breeze/linalg/CSCMatrixExtraOps.scala
  @expand
  @expand.valify
  implicit def CSCMatrixCanMulM_M[@expandArgs (Int, Float, Long, Double) A]: BinaryOp[CSCMatrix[A], CSCMatrix[A], OpMulScalar, CSCMatrix[A]] = new BinaryOp[CSCMatrix[A], CSCMatrix[A], OpMulScalar, CSCMatrix[A]] {

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
  }

}
