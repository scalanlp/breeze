package breeze.linalg

import scala.reflect.ClassTag

class SliceMatrix[@specialized(Int) K1,
                  @specialized(Int) K2,
                  @specialized(Int, Double, Float) V](val tensor: Tensor[(K1, K2),V],
                                                      val slice1: IndexedSeq[K1], val slice2: IndexedSeq[K2]) extends Matrix[V] {

  def apply(i: Int, j: Int): V = tensor(slice1(i)->slice2(j))

  def update(i: Int, j: Int, e: V) {tensor(slice1(i)->slice2(j)) = e}

  def rows: Int = slice1.length

  def cols: Int = slice2.length

  def activeValuesIterator: Iterator[V] = valuesIterator
  def activeIterator: Iterator[((Int, Int), V)] = iterator
  def activeKeysIterator: Iterator[(Int, Int)] = keysIterator

  def activeSize: Int = size

  def repr: Matrix[V] = this

  def copy: Matrix[V] = {
    if (rows == 0) Matrix.zeroRows[V](cols)
    else if (cols == 0) Matrix.zeroCols[V](rows)
    else {
      val v = apply(0,0)
      implicit val man = ClassTag[V](v.getClass)
      val result = new DenseMatrix[V](rows, cols, new Array[V](size))
      result := (this:Matrix[V])
      result
    }
  }
}

