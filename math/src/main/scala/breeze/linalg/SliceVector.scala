package breeze.linalg

import scala.reflect.ClassTag

/**
 * A SliceVector is a vector that is a view of another underlying tensor. For instance:
 * {{{
 * val m = DenseMatrix(...)
 * m( (1,2), (3,4), (4,5))
 * }}}
 *
 * will give a SliceVector such that apply/update at index 0 will map to m(1,2), index 1 to m(3,4), etc.
 * @author dlwh
 */
class SliceVector[@specialized(Int) K, @specialized(Int, Double, Float) V:ClassTag](val tensor: QuasiTensor[K,V],
                                                                           val slices: IndexedSeq[K]) extends Vector[V] {
  def apply(i: Int): V = tensor(slices(i))

  def update(i: Int, v: V) {tensor(slices(i)) = v}

  def copy: Vector[V] = DenseVector( (slices map (tensor.apply _)):_*)

  def length: Int = slices.length

  def activeSize: Int = slices.length

  def repr: Vector[V] = this

  def activeKeysIterator: Iterator[Int] = keysIterator

  def activeIterator: Iterator[(Int, V)] = iterator

  def activeValuesIterator: Iterator[V] = valuesIterator
}

