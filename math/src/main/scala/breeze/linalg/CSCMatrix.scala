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
import breeze.storage.DefaultArrayValue
import java.util
import breeze.util.{Terminal, ArrayUtil}
import collection.mutable
import breeze.math.Semiring

/**
 * A compressed sparse column matrix, as used in Matlab and CSparse, etc.
 *
 * Most implementations based on "Direct Methods for Sparse Linear Systems"
 * by Timothy A. Davis
 * @author dlwh
 */
// TODO: maybe put columns in own array of sparse vectors, making slicing easier?
// TODO: constructor should be private, but it doesn't work. stupid specialization.
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

object CSCMatrix extends MatrixConstructors[CSCMatrix] with CSCMatrixOps_Int with CSCMatrixOps_Float with CSCMatrixOps_Double  {
  def zeros[@specialized(Int, Float, Double) V:ClassManifest:DefaultArrayValue](rows: Int, cols: Int, initialNonzero: Int = 0) = {
    new CSCMatrix[V](new Array(initialNonzero), rows, cols, new Array(cols + 1), 0, new Array(initialNonzero))
  }

  def zeros[@specialized(Int, Float, Double) V: ClassManifest : DefaultArrayValue](rows: Int, cols: Int): CSCMatrix[V] = zeros(rows, cols, 0)

  def create[@specialized(Int, Float, Double) V: DefaultArrayValue](rows: Int, cols: Int, data: Array[V]): CSCMatrix[V] = {
    val z = implicitly[DefaultArrayValue[V]].value
    implicit val man = ClassManifest.fromClass(data.getClass.getComponentType.asInstanceOf[Class[V]])
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


  /**
   * This is basically an unsorted coordinate matrix.
   * @param initNnz initial number of nonzero entries
   */
  class Builder[@specialized(Int, Float, Double) T:ClassManifest:Semiring:DefaultArrayValue](rows: Int, cols: Int, initNnz: Int = 16) {
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
    private val vs = implicitly[ClassManifest[T]].newArrayBuilder()
    vs.sizeHint(initNnz)

    def result():CSCMatrix[T] = {
      val rs = this.rs.result()
      val cs = this.cs.result()
      val vs = this.vs.result()
      // at most this many nnz
      val nnz = rs.length

      val order = sortedIndices(rs, cs)

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
