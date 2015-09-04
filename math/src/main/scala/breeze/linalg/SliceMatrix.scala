package breeze.linalg

import breeze.linalg.immutable
import breeze.math.Semiring

import scala.reflect.ClassTag

class SliceMatrix[@specialized(Int) K1,
                  @specialized(Int) K2,
                  @specialized(Double, Int, Float, Long) V:Semiring:ClassTag](val tensor: Tensor[(K1, K2),V],
                                                      val slice1: IndexedSeq[K1], val slice2: IndexedSeq[K2]) extends immutable.Matrix[V] {

  def apply(i: Int, j: Int): V = tensor(slice1(i)->slice2(j))

  def update(i: Int, j: Int, e: V) {tensor(slice1(i)->slice2(j)) = e}

  def rows: Int = slice1.length

  def cols: Int = slice2.length

  def activeValuesIterator: Iterator[V] = valuesIterator
  def activeIterator: Iterator[((Int, Int), V)] = iterator
  def activeKeysIterator: Iterator[(Int, Int)] = keysIterator

  def activeSize: Int = size

  def repr: immutable.Matrix[V] = this

  def copy: immutable.Matrix[V] = {
    if (rows == 0) Matrix.zeroRows[V](cols)
    else if (cols == 0) Matrix.zeroCols[V](rows)
    else {
//      val v = apply(0,0)
      val result = new DenseMatrix[V](rows, cols, new Array[V](size))
      result := (this:immutable.Matrix[V])
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

