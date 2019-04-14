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

import breeze.linalg.support.CanTraverseValues.ValuesVisitor

import scala.{specialized => spec}
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
trait MatrixLike[@spec(Double, Int, Float, Long) V, +Self <: Matrix[V]]
    extends Tensor[(Int, Int), V]
    with TensorLike[(Int, Int), V, Self] {
  def map[V2, That](fn: V => V2)(implicit canMapValues: CanMapValues[Self @uncheckedVariance, V, V2, That]): That =
    values.map(fn)

}

trait Matrix[@spec(Double, Int, Float, Long) V] extends MatrixLike[V, Matrix[V]] {

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

    def iterator: Iterator[(Int, Int)] = for { j <- Iterator.range(0, cols); i <- Iterator.range(0, rows) } yield (i, j)
  }

  def iterator = for (i <- Iterator.range(0, rows); j <- Iterator.range(0, cols)) yield (i -> j) -> apply(i, j)

  def valuesIterator = for (i <- Iterator.range(0, rows); j <- Iterator.range(0, cols)) yield apply(i, j)

  def keysIterator = for (i <- Iterator.range(0, rows); j <- Iterator.range(0, cols)) yield (i -> j)

  def toString(maxLines: Int = Terminal.terminalHeight - 3, maxWidth: Int = Terminal.terminalWidth): String = {
    val showRows = if (rows > maxLines) maxLines - 1 else rows

    def colWidth(col: Int) =
      if (showRows > 0)
        (0 until showRows).map(row => if (this(row, col) != null) this(row, col).toString.length + 2 else 3).max
      else 0

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
      val cell = if (this(row, col) != null) this(row, col).toString else "--"
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

  override def toString: String = toString(Terminal.terminalHeight, Terminal.terminalWidth)

  def toDenseMatrix(implicit cm: ClassTag[V], zero: Zero[V]) = {
    DenseMatrix.tabulate(rows, cols) { (i, j) =>
      apply(i, j)
    }
  }

  def copy: Matrix[V]

  def flatten(view: View = View.Prefer): Vector[V]

  override def equals(p1: Any): Boolean = p1 match {
    case x: Matrix[_] =>
      this.rows == x.rows && this.cols == x.cols &&
        keysIterator.forall(k => this(k) == x(k))
    case _ =>
      return false
  }

}

object Matrix
    extends MatrixConstructors[Matrix]
    with LowPriorityMatrix
    with MatrixGenericOps
    with MatrixOpsLowPrio
    with MatrixOps
    with MatrixMultOps {

  def zeros[@spec(Double, Int, Float, Long) V: ClassTag: Zero](rows: Int, cols: Int): Matrix[V] =
    DenseMatrix.zeros(rows, cols)

  def create[@spec(Double, Int, Float, Long) V: Zero](rows: Int, cols: Int, data: Array[V]): Matrix[V] =
    DenseMatrix.create(rows, cols, data)

  private[linalg] def zeroRows[V: ClassTag](cols: Int): Matrix[V] = emptyMatrix(0, cols)
  private[linalg] def zeroCols[V: ClassTag](rows: Int): Matrix[V] = emptyMatrix(rows, 0)

  private[linalg] def emptyMatrix[V: ClassTag](_rows: Int, _cols: Int): Matrix[V] = new Matrix[V] {
    def activeIterator: Iterator[((Int, Int), V)] = Iterator.empty

    def activeValuesIterator: Iterator[V] = Iterator.empty

    def activeKeysIterator: Iterator[(Int, Int)] = Iterator.empty

    def apply(i: Int, j: Int): V = throw new IndexOutOfBoundsException("Empty matrix!")

    def update(i: Int, j: Int, e: V) {
      throw new IndexOutOfBoundsException("Empty matrix!")
    }

    def rows: Int = _rows

    def cols: Int = _cols

    def copy: Matrix[V] = this

    def activeSize: Int = 0

    def repr: Matrix[V] = this

    def flatten(view: View) = Vector[V]()
  }

  implicit def canTraverseKeyValuePairs[V]: CanTraverseKeyValuePairs[Matrix[V], (Int, Int), V] = {
    new CanTraverseKeyValuePairs[Matrix[V], (Int, Int), V] {
      def isTraversableAgain(from: Matrix[V]): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: Matrix[V], fn: CanTraverseKeyValuePairs.KeyValuePairsVisitor[(Int, Int), V]): Unit = {
        from.iterator.foreach((fn.visit _).tupled)
      }

    }
  }

  implicit def canTraverseValues[V]: CanTraverseValues[Matrix[V], V] = {
    new CanTraverseValues[Matrix[V], V] {
      def isTraversableAgain(from: Matrix[V]): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: Matrix[V], fn: ValuesVisitor[V]): Unit = {
        from.valuesIterator.foreach(fn.visit)
      }
    }
  }
}

trait MatrixConstructors[Mat[T] <: Matrix[T]] {
  def zeros[@spec(Double, Int, Float, Long) V: ClassTag: Zero](rows: Int, cols: Int): Mat[V]
  def create[@spec(Double, Int, Float, Long) V: Zero](rows: Int, cols: Int, data: Array[V]): Mat[V]

  /**
   * Creates a matrix of all ones.
   * @param rows
   * @param cols
   * @tparam V
   * @return
   */
  def ones[@spec(Double, Int, Float, Long) V: ClassTag: Zero: Semiring](rows: Int, cols: Int): Mat[V] = {
    fill(rows, cols)(implicitly[Semiring[V]].one)
  }

  def fill[@spec(Double, Int, Float, Long) V: ClassTag: Zero](rows: Int, cols: Int)(v: => V): Mat[V] =
    create(rows, cols, Array.fill(rows * cols)(v))
  def tabulate[@spec(Double, Int, Float, Long) V: ClassTag: Zero](rows: Int, cols: Int)(f: (Int, Int) => V): Mat[V] = {
    val z = zeros(rows, cols)
    for (c <- 0 until cols; r <- 0 until rows) {
      z(r, c) = f(r, c)
    }
    z
  }

  def rand[T: ClassTag: Zero](rows: Int, cols: Int, rand: Rand[T] = Rand.uniform): Mat[T] = {
    fill(rows, cols)(rand.draw())
  }

  /** Static constructor for a literal matrix. */
  def apply[R, @spec(Double, Int, Float, Long) V](
      rows: R*)(implicit rl: LiteralRow[R, V], man: ClassTag[V], zero: Zero[V]): Mat[V] = {
    val nRows = rows.length
    val ns = rows.headOption match {
      case None => 0
      case Some(firstRow) => rl.length(firstRow)
    }
    val rv = zeros(nRows, ns)
    finishLiteral(rv, rl, rows)
    rv
  }

  implicit def canCreateZeros[T: ClassTag: Zero]: CanCreateZeros[Mat[T], (Int, Int)] =
    new CanCreateZeros[Mat[T], (Int, Int)] {
      def apply(dims: (Int, Int)): Mat[T] = {
        zeros[T](dims._1, dims._2)
      }
    }

  // This method only exists because of trouble in Scala-specialization land.
  // basically, we turn off specialization for this loop, since it's not going to be fast anyway.
  private def finishLiteral[V, R](rv: Matrix[V], rl: LiteralRow[R, V], rows: Seq[R]) {
    for ((row, i) <- rows.zipWithIndex) {
      rl.foreach(row, { (j, v) =>
        rv(i, j) = v
      })
    }
  }
}

trait LowPriorityMatrix {
  implicit def canSliceTensorBooleanRows[V: Semiring: ClassTag]
    : CanSlice2[Matrix[V], Tensor[Int, Boolean], ::.type, SliceMatrix[Int, Int, V]] = {
    new CanSlice2[Matrix[V], Tensor[Int, Boolean], ::.type, SliceMatrix[Int, Int, V]] {
      def apply(from: Matrix[V], rows: Tensor[Int, Boolean], cols: ::.type): SliceMatrix[Int, Int, V] = {
        val cols = 0 until from.cols
        new SliceMatrix(from, SliceUtils.mapRowSeq(rows.findAll(_ == true), from.rows), cols)
      }
    }
  }

  implicit def canSliceTensorBooleanCols[V: Semiring: ClassTag]
    : CanSlice2[Matrix[V], ::.type, Tensor[Int, Boolean], SliceMatrix[Int, Int, V]] = {
    new CanSlice2[Matrix[V], ::.type, Tensor[Int, Boolean], SliceMatrix[Int, Int, V]] {
      def apply(from: Matrix[V], rows: ::.type, cols: Tensor[Int, Boolean]): SliceMatrix[Int, Int, V] = {
        val rows = 0 until from.rows
        new SliceMatrix(from, rows, SliceUtils.mapColumnSeq(cols.findAll(_ == true), from.cols))
      }
    }
  }

  implicit def canSliceTensorBooleanRowsAndCol[V: Semiring: ClassTag]
    : CanSlice2[Matrix[V], Tensor[Int, Boolean], Int, SliceVector[(Int, Int), V]] = {
    new CanSlice2[Matrix[V], Tensor[Int, Boolean], Int, SliceVector[(Int, Int), V]] {
      def apply(from: Matrix[V], sliceRows: Tensor[Int, Boolean], sliceCol: Int): SliceVector[(Int, Int), V] = {
        val rows = SliceUtils.mapRowSeq(sliceRows.findAll(_ == true), from.rows)
        val col = SliceUtils.mapColumn(sliceCol, from.cols)
        new SliceVector(from, slices = rows.map(row => (row, col)))
      }
    }
  }

  implicit def canSliceRowAndTensorBooleanCols[V: Semiring: ClassTag]
    : CanSlice2[Matrix[V], Int, Tensor[Int, Boolean], Transpose[SliceVector[(Int, Int), V]]] = {
    new CanSlice2[Matrix[V], Int, Tensor[Int, Boolean], Transpose[SliceVector[(Int, Int), V]]] {
      def apply(
          from: Matrix[V],
          sliceRow: Int,
          sliceCols: Tensor[Int, Boolean]): Transpose[SliceVector[(Int, Int), V]] = {
        val row = SliceUtils.mapRow(sliceRow, from.rows)
        val cols = SliceUtils.mapColumnSeq(sliceCols.findAll(_ == true), from.cols)
        new SliceVector(from, slices = cols.map(col => (row, col))).t
      }
    }
  }

  implicit def canSliceTensorBooleanRowsAndCols[V: Semiring: ClassTag]
    : CanSlice2[Matrix[V], Tensor[Int, Boolean], Tensor[Int, Boolean], SliceMatrix[Int, Int, V]] = {
    new CanSlice2[Matrix[V], Tensor[Int, Boolean], Tensor[Int, Boolean], SliceMatrix[Int, Int, V]] {
      def apply(
          from: Matrix[V],
          sliceRows: Tensor[Int, Boolean],
          sliceCols: Tensor[Int, Boolean]): SliceMatrix[Int, Int, V] = {
        val rows = SliceUtils.mapRowSeq(sliceRows.findAll(_ == true), from.rows)
        val cols = SliceUtils.mapColumnSeq(sliceCols.findAll(_ == true), from.cols)
        new SliceMatrix(from, rows, cols)
      }
    }
  }

  implicit def canSliceTensorBooleanRowsAndWeirdCols[V: Semiring: ClassTag]
    : CanSlice2[Matrix[V], Tensor[Int, Boolean], Seq[Int], SliceMatrix[Int, Int, V]] = {
    new CanSlice2[Matrix[V], Tensor[Int, Boolean], Seq[Int], SliceMatrix[Int, Int, V]] {
      def apply(from: Matrix[V], sliceRows: Tensor[Int, Boolean], sliceCols: Seq[Int]): SliceMatrix[Int, Int, V] = {
        val rows = SliceUtils.mapRowSeq(sliceRows.findAll(_ == true), from.rows)
        val cols = SliceUtils.mapColumnSeq(sliceCols, from.cols)
        new SliceMatrix(from, rows, cols)
      }
    }
  }

  implicit def canSliceWeirdRowsAndTensorBooleanCols[V: Semiring: ClassTag]
    : CanSlice2[Matrix[V], Seq[Int], Tensor[Int, Boolean], SliceMatrix[Int, Int, V]] = {
    new CanSlice2[Matrix[V], Seq[Int], Tensor[Int, Boolean], SliceMatrix[Int, Int, V]] {
      def apply(from: Matrix[V], sliceRows: Seq[Int], sliceCols: Tensor[Int, Boolean]): SliceMatrix[Int, Int, V] = {
        val rows = SliceUtils.mapRowSeq(sliceRows, from.rows)
        val cols = SliceUtils.mapColumnSeq(sliceCols.findAll(_ == true), from.cols)
        new SliceMatrix(from, rows, cols)
      }
    }
  }
}
