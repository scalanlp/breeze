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

import scala.{specialized=>spec}
import breeze.storage.{DefaultArrayValue, Storage}
import breeze.util.Terminal
import support.LiteralRow
import util.Random
import breeze.generic.CanMapValues
import breeze.math.Semiring
import breeze.linalg.operators.{OpSet, BinaryUpdateOp}
import scala.reflect.ClassTag

/**
 *
 * @author dlwh
 */
trait MatrixLike[@spec(Int, Float, Double) E, +Self <: Matrix[E]] extends Tensor[(Int, Int), E] with TensorLike[(Int, Int), E, Self] {
  def map[E2, That](fn: E=>E2)(implicit canMapValues: CanMapValues[Self, E, E2, That]):That = values map fn

}

trait Matrix[@spec(Int, Float, Double) E] extends MatrixLike[E, Matrix[E]] {

  final def apply(i: (Int, Int)) = apply(i._1, i._2)
  final def update(i: (Int, Int), e: E) {
    update(i._1, i._2, e)
  }

  def apply(i: Int, j: Int):E
  def update(i: Int, j: Int, e: E)

  def size = rows * cols
  def rows: Int
  def cols: Int

  def keySet: Set[(Int, Int)] = new Set[(Int, Int)] {
    def contains(elem: (Int, Int)): Boolean = elem._1 >= 0 && elem._1 < rows && elem._2 >= 0 && elem._2 < cols

    def +(elem: (Int, Int)): Set[(Int, Int)] = Set() ++ iterator + elem
    def -(elem: (Int, Int)): Set[(Int, Int)] = Set() ++ iterator - elem

    def iterator: Iterator[(Int, Int)] = for{ j <- Iterator.range(0, cols); i <- Iterator.range(0, rows)} yield (i, j)
  }

  def iterator = for(i <- Iterator.range(0, rows); j <- Iterator.range(0, cols)) yield (i -> j) -> apply(i, j)

  def valuesIterator = for(i <- Iterator.range(0, rows); j <- Iterator.range(0, cols)) yield apply(i, j)

  def keysIterator = for(i <- Iterator.range(0, rows); j <- Iterator.range(0, cols)) yield (i -> j)

  def toString(maxLines : Int = Terminal.terminalHeight - 3,
               maxWidth : Int = Terminal.terminalWidth) : String = {
    val showRows = if (rows > maxLines) maxLines - 1 else rows
    def colWidth(col : Int) =
      if (showRows > 0) (0 until showRows).map(row => this(row,col).toString.length+2).max else 0

    val colWidths = new scala.collection.mutable.ArrayBuffer[Int]
    var col = 0
    while (col < cols && colWidths.sum < maxWidth) {
      colWidths += colWidth(col)
      col += 1
    }

    // make space for "... (K total)"
    if (colWidths.size < cols) {
      while (colWidths.sum + cols.toString.length + 12 >= maxWidth) {
        if (colWidths.isEmpty) {
          return "%d x %d matrix".format(rows, cols)
        }
        colWidths.remove(colWidths.length - 1)
      }
    }

    val newline = Terminal.newline

    val rv = new scala.StringBuilder
    for (row <- 0 until showRows; col <- 0 until colWidths.length) {
      val cell = this(row,col).toString
      rv.append(cell)
      rv.append(" " * (colWidths(col) - cell.length))
      if (col == colWidths.length - 1) {
        if (col < cols - 1) {
          rv.append("...")
          if (row == 0) {
            rv.append(" (")
            rv.append(cols)
            rv.append(" total)")
          }
        }
        if (row + 1 < showRows) {
          rv.append(newline)
        }
      }
    }

    if (rows > showRows) {
      rv.append(newline)
      rv.append("... (")
      rv.append(rows)
      rv.append(" total)")
    }

    rv.toString
  }

  override def toString : String = toString(Terminal.terminalHeight, Terminal.terminalWidth)

  def toDenseMatrix(implicit cm: ClassTag[E], dfv: DefaultArrayValue[E]) = {
    DenseMatrix.tabulate(rows, cols){ (i,j) => apply(i, j)}
  }

}

object Matrix extends MatrixConstructors[Matrix]
                      with MatrixGenericOps
                      with MatrixMultOps_Double
                      with MatrixMultOps_Float
                      with MatrixMultOps_Int
                      with MatrixMultOps_Complex {
  def zeros[@specialized(Int, Float, Double) V: ClassTag:DefaultArrayValue](rows: Int, cols: Int): Matrix[V] = DenseMatrix.zeros(rows, cols)

  def create[@specialized(Int, Float, Double) V:DefaultArrayValue](rows: Int, cols: Int, data: Array[V]): Matrix[V] = DenseMatrix.create(rows, cols, data)

  // slicing
}

trait MatrixGenericOps { this: Matrix.type =>
  class SetMMOp[@specialized(Int, Double, Float) V, MM](implicit subtype: MM<:<Matrix[V]) extends BinaryUpdateOp[Matrix[V], MM, OpSet] {
    def apply(a: Matrix[V], b: MM) {
      require(a.rows == b.rows, "Row dimension mismatch!")
      require(a.cols == b.cols, "Col dimension mismatch!")
      val bb = subtype(b)
      // TODO: might make sense to have a "am I sparse?" check and use activeIterator instead?
      for(i <- 0 until a.rows; j <- 0 until a.cols) {
        a(i,j) = bb(i, j)
      }

    }
  }
  implicit def setDMDV[V, MM](implicit st: MM<:<Matrix[V]) = new SetMMOp[V, MM]
}

trait MatrixConstructors[Vec[T]<:Matrix[T]] {
  def zeros[@specialized(Int, Float, Double) V:ClassTag:DefaultArrayValue](rows: Int, cols: Int):Vec[V]
  def create[@specialized(Int, Float, Double) V:DefaultArrayValue](rows: Int, cols: Int, data: Array[V]):Vec[V]

  /**
   * Creates a matrix of all ones.
   * @param rows
   * @param cols
   * @tparam V
   * @return
   */
  def ones[@specialized(Int, Float, Double) V:ClassTag:DefaultArrayValue:Semiring](rows: Int, cols: Int):Vec[V] = {
    fill(rows,cols)(implicitly[Semiring[V]].one)
  }

  def fill[@spec(Double, Int, Float) V:ClassTag:DefaultArrayValue](rows: Int, cols: Int)(v: =>V):Vec[V] = create(rows, cols, Array.fill(rows * cols)(v))
  def tabulate[@spec(Double, Int, Float) V:ClassTag:DefaultArrayValue](rows: Int, cols: Int)(f: (Int,Int)=>V):Vec[V]= {
    val z = zeros(rows, cols)
    for(c <- 0 until cols; r <- 0 until rows) {
      z(r, c) = f(r, c)
    }
    z
  }

  def rand(rows: Int, cols: Int, rand: Random = new Random()) = {
    fill(rows, cols)(rand.nextDouble())
  }

  /** Static constructor for a literal matrix. */
  def apply[R,@specialized(Int, Float, Double) V](rows : R*)(implicit rl : LiteralRow[R,V], man : ClassTag[V], df: DefaultArrayValue[V]) = {
    val nRows = rows.length
    val ns = rl.length(rows(0))
    val rv = zeros(nRows, ns)
    finishLiteral(rv, rl, rows)
    rv
  }

  // This method only exists because of trouble in Scala-specialization land.
  // basically, we turn off specialization for this loop, since it's not going to be fast anyway.
  private def finishLiteral[V, R](rv: Matrix[V], rl : LiteralRow[R,V], rows: Seq[R]) {
    for ((row,i) <- rows.zipWithIndex) {
      rl.foreach(row, {(j, v) => rv(i,j) = v})
    }
  }

}
