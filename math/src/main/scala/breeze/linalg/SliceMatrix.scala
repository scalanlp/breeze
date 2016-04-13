package breeze.linalg

import breeze.linalg.operators.OpSet
import breeze.linalg.support.CanTraverseKeyValuePairs.KeyValuePairsVisitor
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.linalg.support._
import breeze.math.Semiring
import breeze.storage.Zero

import scala.reflect.ClassTag

class SliceMatrix[@specialized(Int) K1,
                  @specialized(Int) K2,
                  @specialized(Double, Int, Float, Long) V:Semiring:ClassTag](val tensor: Tensor[(K1, K2),V],
                                                                              val slice1: IndexedSeq[K1],
                                                                              val slice2: IndexedSeq[K2])
  extends Matrix[V] with MatrixLike[V, SliceMatrix[K1, K2, V]] {

  def apply(i: Int, j: Int): V = tensor(slice1(i)->slice2(j))

  def update(i: Int, j: Int, e: V) {tensor(slice1(i)->slice2(j)) = e}

  def rows: Int = slice1.length

  def cols: Int = slice2.length

  def activeValuesIterator: Iterator[V] = valuesIterator
  def activeIterator: Iterator[((Int, Int), V)] = iterator
  def activeKeysIterator: Iterator[(Int, Int)] = keysIterator

  def activeSize: Int = size

  def repr: SliceMatrix[K1, K2, V] = this

  def copy: Matrix[V] = {
    if (rows == 0) Matrix.zeroRows[V](cols)
    else if (cols == 0) Matrix.zeroCols[V](rows)
    else {
//      val v = apply(0,0)
      val result = new DenseMatrix[V](rows, cols, new Array[V](size))
      result := (this:Matrix[V])
      result
    }
  }

  def flatten(view: View=View.Copy): Vector[V] = {
    view match {
      case View.Require => throw new UnsupportedOperationException("Cannot make Vector as view of SliceMatrix.")
      case View.Copy =>
        val vb = new VectorBuilder[V](rows*cols,activeSize)
        val ai = activeIterator
        while (ai.hasNext) {
          val ((r,c),v) = ai.next()
          vb.add(c*rows+r,v)
        }
        vb.toVector
      case View.Prefer => flatten(View.Copy)
    }
  }
}

object SliceMatrix extends LowPrioritySliceMatrix with SliceMatrixOps {

  implicit def canMapKeyValuePairs[K1, K2, V, V2: ClassTag: Zero]: CanMapKeyValuePairs[SliceMatrix[K1, K2, V], (Int, Int), V, V2, DenseMatrix[V2]] = {
    new CanMapKeyValuePairs[SliceMatrix[K1, K2, V], (Int, Int), V, V2, DenseMatrix[V2]] {
      override def map(from: SliceMatrix[K1, K2, V], fn: ((Int, Int), V) => V2): DenseMatrix[V2] = {
        DenseMatrix.tabulate(from.rows, from.cols)((i, j) => fn((i, j), from(i, j)))
      }

      override def mapActive(from: SliceMatrix[K1, K2, V], fn: ((Int, Int), V) => V2): DenseMatrix[V2] = {
        map(from, fn)
      }
    }
  }

  implicit def canMapValues[K1, K2, @specialized(Int, Float, Double) V, @specialized(Int, Float, Double) V2: ClassTag: Zero]: CanMapValues[SliceMatrix[K1, K2, V], V, V2, DenseMatrix[V2]] = {
    new CanMapValues[SliceMatrix[K1, K2, V], V, V2, DenseMatrix[V2]] {
      override def apply(from: SliceMatrix[K1, K2, V], fn: (V) => V2): DenseMatrix[V2] = {
        DenseMatrix.tabulate(from.rows, from.cols)((i, j) => fn(from(i, j)))
      }

    }
  }

  implicit def canCreateZerosLike[K1, K2, V: ClassTag : Zero]: CanCreateZerosLike[SliceMatrix[K1, K2, V], DenseMatrix[V]] = {
    new CanCreateZerosLike[SliceMatrix[K1, K2, V], DenseMatrix[V]] {
      def apply(v1: SliceMatrix[K1, K2, V]): DenseMatrix[V] = {
        DenseMatrix.zeros[V](v1.rows, v1.cols)
      }
    }
  }

  implicit def canIterateValues[K1, K2, V]: CanTraverseValues[SliceMatrix[K1, K2, V], V] =

    new CanTraverseValues[SliceMatrix[K1, K2, V], V] {

      def isTraversableAgain(from: SliceMatrix[K1, K2, V]): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: SliceMatrix[K1, K2, V], fn: ValuesVisitor[V]): Unit = {
        from.activeValuesIterator foreach {
          fn.visit(_)
        }
      }

    }

  implicit def canIterateKeyValuePairs[K1, K2, V]: CanTraverseKeyValuePairs[SliceMatrix[K1, K2, V], (Int, Int), V] = {
    new CanTraverseKeyValuePairs[SliceMatrix[K1, K2, V], (Int, Int), V] {
      /** Traverses all values from the given collection. */
      override def traverse(from: SliceMatrix[K1, K2, V], fn: KeyValuePairsVisitor[(Int, Int), V]): Unit = {
        from.iterator foreach {
          case (k, v) => fn.visit(k, v)
        }

      }

      def isTraversableAgain(from: SliceMatrix[K1, K2, V]): Boolean = true

    }
  }


  implicit def canTransformValues[K1, K2, V]: CanTransformValues[SliceMatrix[K1, K2, V], V] = {
    new CanTransformValues[SliceMatrix[K1, K2, V], V] {
      def transform(from: SliceMatrix[K1, K2, V], fn: (V) => V) {
        for (j <- 0 until from.cols; i <- 0 until from.rows) {
          from(i, j) = fn(from(i, j))
        }
      }

      def transformActive(from: SliceMatrix[K1, K2, V], fn: (V) => V) {
        transform(from, fn)
      }
    }
  }

  // slices
  implicit def canSliceRow[V: Semiring : ClassTag]: CanSlice2[SliceMatrix[Int, Int, V], Int, ::.type, SliceVector[(Int, Int), V]] = {
    new CanSlice2[SliceMatrix[Int, Int, V], Int, ::.type, SliceVector[(Int, Int), V]] {
      def apply(from: SliceMatrix[Int, Int, V], sliceRow: Int, ignored: ::.type): SliceVector[(Int, Int), V] = {
        val row = SliceUtils.mapRow(sliceRow, from.rows)
        val cols = 0 until from.cols
        new SliceVector(from, cols.map(col => (row, col)))
      }
    }
  }

  implicit def canSliceCol[V: Semiring : ClassTag]: CanSlice2[SliceMatrix[Int, Int, V], ::.type, Int, SliceVector[(Int, Int), V]] = {
    new CanSlice2[SliceMatrix[Int, Int, V], ::.type, Int, SliceVector[(Int, Int), V]] {
      def apply(from: SliceMatrix[Int, Int, V], ignored: ::.type, sliceCol: Int): SliceVector[(Int, Int), V] = {
        val col = SliceUtils.mapColumn(sliceCol, from.cols)
        val rows = 0 until from.rows
        new SliceVector(from, rows.map(row => (row, col)))
      }
    }
  }
}

trait LowPrioritySliceMatrix {
  // Note: can't have a separate implicit for Range and Seq since they will be ambiguous as both will return a
  // SliceMatrix which differs from dense matrix where a Range will return another DenseMatrix and only a seq will
  // return a SliceMatrix
  implicit def canSliceWeirdRows[V: Semiring : ClassTag]: CanSlice2[SliceMatrix[Int, Int, V], Seq[Int], ::.type, SliceMatrix[Int, Int, V]] = {
    new CanSlice2[SliceMatrix[Int, Int, V], Seq[Int], ::.type, SliceMatrix[Int, Int, V]] {
      def apply(from: SliceMatrix[Int, Int, V], rows: Seq[Int], ignored: ::.type): SliceMatrix[Int, Int, V] = {
        val cols = 0 until from.cols
        new SliceMatrix(from, SliceUtils.mapRowSeq(rows, from.rows), cols)
      }
    }
  }

  // Note: can't have a separate implicit for Range and Seq since they will be ambiguous as both will return a
  // SliceMatrix which differs from dense matrix where a Range will return another DenseMatrix and only a seq will
  // return a SliceMatrix
  implicit def canSliceWeirdCols[V: Semiring : ClassTag]: CanSlice2[SliceMatrix[Int, Int, V], ::.type, Seq[Int], SliceMatrix[Int, Int, V]] = {
    new CanSlice2[SliceMatrix[Int, Int, V], ::.type, Seq[Int], SliceMatrix[Int, Int, V]] {
      def apply(from: SliceMatrix[Int, Int, V], ignored: ::.type, cols: Seq[Int]): SliceMatrix[Int, Int, V] = {
        val rows = 0 until from.rows
        new SliceMatrix(from, rows, SliceUtils.mapColumnSeq(cols, from.cols))
      }
    }
  }
}


trait SliceMatrixOps {
  // todo: all all the other ops (can this be done with some macro magic?
  implicit def opSetInPlace[K1, K2, V]: OpSet.InPlaceImpl2[SliceMatrix[K1, K2, V], V] = new SMOpSetInPlace[K1, K2, V]

  class SMOpSetInPlace[@specialized(Int) K1,
                @specialized(Int) K2,
                @specialized(Double, Int, Float, Long) V] extends OpSet.InPlaceImpl2[SliceMatrix[K1, K2, V], V] {
    def apply(a: SliceMatrix[K1, K2, V], b: V): Unit = a.keysIterator.foreach(k => a.update(k, b))
  }
}
