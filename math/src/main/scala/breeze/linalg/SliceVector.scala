package breeze.linalg

/**
 *
 * @author dlwh
 */
class SliceVector[@specialized(Int) K, @specialized(Int, Double, Float) V:ClassManifest](val tensor: QuasiTensor[K,V],
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

