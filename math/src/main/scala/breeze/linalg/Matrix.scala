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

import breeze.linalg.immutable

import scala.{specialized=>spec}
import breeze.storage.Zero
import breeze.util.Terminal
import breeze.linalg.support._
import breeze.math._
import breeze.linalg.operators._
import scala.reflect.ClassTag
import scala.annotation.unchecked.uncheckedVariance
import breeze.stats.distributions.Rand

/**
 *
 * @author dlwh
 */
trait MatrixLike[@spec(Double, Int, Float, Long) V, +Self  <: immutable.Matrix[V]] extends Tensor[(Int, Int), V] with TensorLike[(Int, Int), V, Self] {
  def map[V2, That](fn: V=>V2)(implicit canMapValues: CanMapValues[Self @uncheckedVariance , V, V2, That]):That = values map fn

}

trait Matrix[@spec(Double, Int, Float, Long) V] extends MatrixLike[V, immutable.Matrix[V]] {

  final def apply(i: (Int, Int)) = apply(i._1, i._2)
  final def update(i: (Int, Int), e: V): Unit = {
    update(i._1, i._2, e)
  }

  def apply(i: Int, j: Int): V
  def update(i: Int, j: Int, e: V): Unit

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
      if (showRows > 0) (0 until showRows).map(row => if(this(row,col)!=null) this(row,col).toString.length+2 else 3).max else 0

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
      val cell = if (this(row,col)!=null) this(row,col).toString else "--"
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

  def toDenseMatrix(implicit cm: ClassTag[V], zero: Zero[V]) = {
    DenseMatrix.tabulate(rows, cols){ (i,j) => apply(i, j)}
  }

  def copy: immutable.Matrix[V]

  def flatten(view: View=View.Prefer): Vector[V]

}

object Matrix extends MatrixConstructors[immutable.Matrix]
                      with MatrixGenericOps
                      with MatrixOpsLowPrio
                      with MatrixOps
                      with MatrixMultOps {

  def zeros[@spec(Double, Int, Float, Long) V: ClassTag:Zero](rows: Int, cols: Int): immutable.Matrix[V] = DenseMatrix.zeros(rows, cols)

  def create[@spec(Double, Int, Float, Long) V:Zero](rows: Int, cols: Int, data: Array[V]): immutable.Matrix[V] = DenseMatrix.create(rows, cols, data)

  private[linalg] def zeroRows[V:ClassTag](cols: Int):immutable.Matrix[V] = emptyMatrix(0, cols)
  private[linalg] def zeroCols[V:ClassTag](rows: Int):immutable.Matrix[V] = emptyMatrix(rows, 0)


  private[linalg] def emptyMatrix[V:ClassTag](_rows: Int, _cols: Int):immutable.Matrix[V] = new immutable.Matrix[V] {
    def activeIterator: Iterator[((Int, Int), V)] = Iterator.empty

    def activeValuesIterator: Iterator[V] = Iterator.empty

    def activeKeysIterator: Iterator[(Int, Int)] = Iterator.empty

    def apply(i: Int, j: Int): V = throw new IndexOutOfBoundsException("Empty matrix!")

    def update(i: Int, j: Int, e: V) {
      throw new IndexOutOfBoundsException("Empty matrix!")
    }

    def rows: Int = _rows

    def cols: Int = _cols

    def copy: immutable.Matrix[V] = this

    def activeSize: Int = 0

    def repr: immutable.Matrix[V] = this

    def flatten(view: View) = Vector[V]()
  }
}


trait MatrixConstructors[Mat[T]<:immutable.Matrix[T]] {
  def zeros[@spec(Double, Int, Float, Long) V:ClassTag:Zero](rows: Int, cols: Int):Mat[V]
  def create[@spec(Double, Int, Float, Long) V:Zero](rows: Int, cols: Int, data: Array[V]):Mat[V]

  /**
   * Creates a matrix of all ones.
   * @param rows
   * @param cols
   * @tparam V
   * @return
   */
  def ones[@spec(Double, Int, Float, Long) V:ClassTag:Zero:Semiring](rows: Int, cols: Int):Mat[V] = {
    fill(rows,cols)(implicitly[Semiring[V]].one)
  }

  def fill[@spec(Double, Int, Float, Long) V:ClassTag:Zero](rows: Int, cols: Int)(v: =>V):Mat[V] = create(rows, cols, Array.fill(rows * cols)(v))
  def tabulate[@spec(Double, Int, Float, Long) V:ClassTag:Zero](rows: Int, cols: Int)(f: (Int,Int)=>V):Mat[V]= {
    val z = zeros(rows, cols)
    for(c <- 0 until cols; r <- 0 until rows) {
      z(r, c) = f(r, c)
    }
    z
  }

  def rand[T:ClassTag:Zero](rows: Int, cols: Int, rand: Rand[T] = Rand.uniform): Mat[T] = {
    fill(rows, cols)(rand.draw())
  }

  // @specialized() R because of https://issues.scala-lang.org/browse/SI-8886
  /** Static constructor for a literal matrix. */
  def apply[@specialized(/* Don't remove until SI-8886 is closed*/) R,
            @spec(Double, Int, Float, Long) V](rows : R*)(implicit rl : LiteralRow[R,V], man : ClassTag[V], zero: Zero[V]) = {
    val nRows = rows.length
    val ns = rl.length(rows(0))
    val rv = zeros(nRows, ns)
    finishLiteral(rv, rl, rows)
    rv
  }

  implicit def canCreateZeros[T:ClassTag:Zero]: CanCreateZeros[Mat[T],(Int,Int)] =
    new CanCreateZeros[Mat[T],(Int,Int)] {
      def apply(dims: (Int,Int)): Mat[T] = {
        zeros[T](dims._1,dims._2)
      }
    }

  implicit def canTabulate[T:ClassTag:Zero] = new CanTabulate[(Int,Int),Mat[T],T] {
    def apply(d: (Int, Int), f: ((Int, Int)) => T): Mat[T] = tabulate[T](d._1,d._2)((r: Int, c: Int) => f((r,c)))
  }


  // This method only exists because of trouble in Scala-specialization land.
  // basically, we turn off specialization for this loop, since it's not going to be fast anyway.
  private def finishLiteral[V, R](rv: immutable.Matrix[V], rl : LiteralRow[R,V], rows: Seq[R]) {
    for ((row,i) <- rows.zipWithIndex) {
      rl.foreach(row, {(j, v) => rv(i,j) = v})
    }
  }

}
