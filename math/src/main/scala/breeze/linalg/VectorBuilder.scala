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
import operators._

import scala.{specialized => spec}
import support._
import breeze.util.{ArrayUtil, ReflectionUtil, Sorting}
import breeze.math.{Field, MutableVectorSpace, Ring, Semiring}
import breeze.storage.Zero

import scala.reflect.ClassTag
import breeze.macros.expand
import breeze.generic.UFunc.{InPlaceImpl2, UImpl2}

/**
 * A VectorBuilder is basically an unsorted Sparse Vector. Two parallel
 * arrays are maintained, one of indices, and another of values.
 * The indices are not sorted. Moreover, <B> indices are not unique in
 * the index array</b>. Furthermore, apply(i) and update(i, v) are linear in the number
 * of active values in the array.
 *
 * + and - are linear operations: they just append to the end.
 * Component wise multiply, divide, and dot product are also linear,
 * but require creating a HashVector copy. (TODO: maybe a SparseVector?)
 *
 * In general, these should never be used, except for building, or for doing feature
 * vector type things where you just need a sparse vector with a fast dot product
 * with a "real" vector.
 *
 * @author dlwh
 */
@SerialVersionUID(1)
class VectorBuilder[@spec(Double, Int, Float, Long) E](
    private var _index: Array[Int],
    private var _data: Array[E],
    private var used: Int,
    var length: Int)(implicit ring: Semiring[E])
    extends NumericOps[VectorBuilder[E]]
    with Serializable {

  def this(length: Int, initialNonZero: Int = 0)(implicit ring: Semiring[E], man: ClassTag[E]) = {
    this(new Array[Int](initialNonZero), new Array[E](initialNonZero), 0, length)
  }

  def this()(implicit ring: Semiring[E], man: ClassTag[E], zero: Zero[E]) = this(-1)

  def size = length

  def data = _data
  def index = _index
  def activeSize = used

  def repr = this

  def contains(i: Int) = _index.contains(i)

  def apply(i: Int) = {
    boundsCheck(i)

    var off = 0
    var acc = ring.zero
    while (off < used) {
      if (_index(off) == i) acc = ring.+(acc, _data(off))
      off += 1
    }

    acc
  }

  private def boundsCheck(i: Int): Unit = {
    if (length >= 0 && (i < 0 || i >= size))
      throw new scala.IndexOutOfBoundsException(s"$i not in [0,$size)")
  }

  def update(i: Int, v: E): Unit = {
    boundsCheck(i)
    var marked = false
    var off = 0
    while (off < used) {
      if (_index(off) == i) {
        if (!marked)
          _data(off) = v
        else _data(off) = ring.zero
        marked = true
      }

      off += 1
    }
  }

  def add(i: Int, v: E): Unit = {
    boundsCheck(i)

    if (_data.length <= used) {
      reallocate(math.max(_data.length * 2, 1))
    }

    _data(used) = v
    _index(used) = i
    used += 1
  }

  def activeIterator = toHashVector.activeIterator

  def activeValuesIterator = toHashVector.activeValuesIterator

  def activeKeysIterator = toHashVector.activeKeysIterator

  // TODO: allow this to vary
  /** This is always assumed to be equal to 0, for now. */
  def default = ring.zero

  def isActive(rawIndex: Int) = rawIndex < used && rawIndex > 0

  override def toString = {
    index.iterator.zip(data.iterator).take(used).mkString(s"VectorBuilder($length)(", ", ", ")")
  }

  def copy: VectorBuilder[E] = {
    new VectorBuilder[E](ArrayUtil.copyOf(index, index.length), ArrayUtil.copyOf(data, index.length), activeSize, size)
  }

  def zerosLike: VectorBuilder[E] = {
    new VectorBuilder[E](new Array[Int](0), ArrayUtil.newArrayLike(data, 0), 0, size)
  }

  def reserve(nnz: Int): Unit = {
    if (nnz < _data.length) {
      reallocate(nnz)
    }
  }

  private def reallocate(nnz: Int): Unit = {
    _index = ArrayUtil.copyOf(_index, nnz)
    _data = ArrayUtil.copyOf(data, nnz)
  }

  def toHashVector: HashVector[E] = {
    requirePositiveLength()
    implicit val man: ClassTag[E] = ReflectionUtil.elemClassTagFromArray(data)
    val hv = HashVector.zeros[E](length)
    var i = 0
    while (i < used) {
      hv(index(i)) = ring.+(hv(index(i)), data(i))
      i += 1
    }
    hv
  }

  private def requirePositiveLength(): Unit = {
    if (size < 0) {
      throw new scala.UnsupportedOperationException("Can't make a vector with a negative length!")
    }
  }

  def toDenseVector: DenseVector[E] = {
    requirePositiveLength()
    implicit val man: ClassTag[E] = ReflectionUtil.elemClassTagFromArray(data)
    val hv = DenseVector.zeros[E](length)
    var i = 0
    while (i < used) {
      hv(index(i)) = ring.+(hv(index(i)), data(i))
      i += 1
    }
    hv
  }

  def toSparseVector: SparseVector[E] = toSparseVector()

  def toSparseVector(alreadySorted: Boolean = false, keysAlreadyUnique: Boolean = false): SparseVector[E] = {
    requirePositiveLength()
    val index = this.index
    val values = this.data
    if (alreadySorted && keysAlreadyUnique) {
      return new SparseVector(index, values, used, length)
    }

    val outIndex = ArrayUtil.copyOf(index, used)
    val outValues = ArrayUtil.copyOf(values, used)
    if (!alreadySorted) {
      Sorting.indirectSort(outIndex, outValues, 0, used)
    }

    if (outIndex.length > 0) {
      if (outIndex(used - 1) >= length) {
        throw new IndexOutOfBoundsException("Index " + index(used - 1) + " exceeds dimension " + length)
      } else if (outIndex(0) < 0) {
        throw new IndexOutOfBoundsException("Index " + outIndex(0) + " is less than 0")
      }
    }
    var i = 1
    var out = 0
    if (!keysAlreadyUnique) {
      while (i < used) {
        if (outIndex(out) == outIndex(i)) {
          outValues(out) = ring.+(outValues(out), outValues(i))
        } else {
          out += 1
          outIndex(out) = outIndex(i)
          outValues(out) = outValues(i)
        }
        i += 1
      }
      if (outIndex.length > 0)
        out += 1
    } else {
      out = used
    }

    new SparseVector(outIndex, outValues, out, length)
  }

  def compact(): Unit = {
    val ah = toSparseVector
    clear()
    reallocate(ah.activeSize)
    var i = 0
    while (i < ah.iterableSize) {
      if (ah.isActive(i)) {
        add(ah.index(i), ah.data(i))
      }
      i += 1
    }
  }

  def clear(): Unit = {
    used = 0
  }

  override def equals(p1: Any): Boolean =
    (this eq p1.asInstanceOf[AnyRef]) || (p1 match {
      case vb: VectorBuilder[_] =>
        this.length == vb.length && vb.toHashVector == this.toHashVector
      case _ => false
    })

  /**
   * Sets the underlying sparse array to use this data
   * @param index must be a sorted list of indices
   * @param data values corresponding to the index
   * @param activeSize number of active elements. The first activeSize will be used.
   */
  def use(index: Array[Int], data: Array[E], activeSize: Int): Unit = {
    require(activeSize >= 0, "activeSize must be non-negative")
    require(data.length >= activeSize, "activeSize must be no greater than array length...")
    _data = data
    _index = index
    used = activeSize
  }

  /**
   * same as data(i). Gives the value at the underlying offset.
   * @param i index into the data array
   * @return
   */
  def valueAt(i: Int): E = data(i)

  /**
   * Gives the logical index from the physical index.
   * @param i
   * @return
   */
  def indexAt(i: Int): Int = index(i)

  /**
   * Only gives true if isActive would return true for all i. (May be false anyway)
   * @return
   */
  def allVisitableIndicesActive: Boolean = true

  // TODO: profile threshold
  def toVector: Vector[E] = {
    requirePositiveLength()
    if (size < 40 || activeSize > size / 4) {
      toDenseVector
    } else {
      toSparseVector
    }

  }
}

object VectorBuilder extends VectorBuilderOps {

  def zeros[@spec(Double, Int, Float, Long) V: ClassTag: Semiring: Zero](size: Int, initialNonZero: Int = 16) =
    new VectorBuilder(size, initialNonZero)
  def apply[@spec(Double, Int, Float, Long) V: Semiring: Zero](values: Array[V]) =
    new VectorBuilder(Array.range(0, values.length), values, values.length, values.length)

  def apply[V: ClassTag: Semiring: Zero](values: V*): VectorBuilder[V] = apply(values.toArray)
  def fill[@spec(Double, Int, Float, Long) V: ClassTag: Semiring: Zero](size: Int)(v: => V): VectorBuilder[V] =
    apply(Array.fill(size)(v))
  def tabulate[@spec(Double, Int, Float, Long) V: ClassTag: Semiring: Zero](size: Int)(f: Int => V): VectorBuilder[V] =
    apply(Array.tabulate(size)(f))

  def apply[V: ClassTag: Semiring: Zero](length: Int)(values: (Int, V)*) = {
    val r = zeros[V](length)
    for ((i, v) <- values) {
      r.add(i, v)
    }
    r
  }

  // implicits
  class CanCopyBuilder[@spec(Double, Int, Float, Long) V: ClassTag: Semiring: Zero] extends CanCopy[VectorBuilder[V]] {
    def apply(v1: VectorBuilder[V]) = {
      v1.copy
    }
  }

  class CanZerosBuilder[@spec(Double, Int, Float, Long) V: ClassTag: Semiring: Zero]
      extends CanCreateZerosLike[VectorBuilder[V], VectorBuilder[V]] {
    def apply(v1: VectorBuilder[V]) = {
      v1.zerosLike
    }
  }

  implicit def canCopyBuilder[@spec(Double, Int, Float, Long) V: ClassTag: Semiring: Zero]: CanCopyBuilder[V] = {
    new CanCopyBuilder[V]
  }
  implicit def canZerosBuilder[@spec(Double, Int, Float, Long) V: ClassTag: Semiring: Zero]: CanZerosBuilder[V] = {
    new CanZerosBuilder[V]
  }

  implicit def canZeroBuilder[@spec(Double, Int, Float, Long) V: Semiring: Zero: ClassTag]
    : CanCreateZeros[VectorBuilder[V], Int] = {
    new CanCreateZeros[VectorBuilder[V], Int] {
      def apply(d: Int): VectorBuilder[V] = zeros(d)
    }
  }
}
