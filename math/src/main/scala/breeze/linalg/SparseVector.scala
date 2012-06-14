package breeze.linalg

import breeze.storage.SparseStorage


/**
 *
 * @author dlwh
 */
final class SparseVector[@specialized(Double,Int,Float) E](var index: Array[Int],
                                                           var data: Array[E],
                                                           var used: Int,
                                                           protected var default: E,
                                                           val offset: Int,
                                                           val stride: Int,
                                                           val length: Int)(implicit protected val manElem: ClassManifest[E]) extends Vector[E] with SparseStorage[E] with VectorLike[E, SparseVector[E]] {
  def repr = this

  def activeIterator = activeKeysIterator zip activeValuesIterator

  def activeValuesIterator = valuesIterator

  def activeKeysIterator = index.iterator.take(used)
}

