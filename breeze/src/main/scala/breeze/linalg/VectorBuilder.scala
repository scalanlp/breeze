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
import scala.{specialized=>spec}
import support._
import breeze.util.{Sorting, ArrayUtil}
import breeze.math.{Field, MutableVectorSpace, Semiring, Ring}
import breeze.storage.DefaultArrayValue
import scala.reflect.ClassTag
import breeze.macros.expand
import breeze.generic.UFunc.{UImpl2, InPlaceImpl2}


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
class VectorBuilder[@spec(Double,Int, Float) E](private var _index: Array[Int],
                                                private var _data: Array[E],
                                                private var used: Int,
                                                var length: Int)
                                               (implicit ring: Semiring[E],
                                                dfv: DefaultArrayValue[E]) extends NumericOps[VectorBuilder[E]] with Serializable {

  def this(length: Int, initialNonZero: Int = 0)(implicit ring: Semiring[E],
                                                 man: ClassTag[E],
                                                 dfv: DefaultArrayValue[E]) = this(new Array[Int](0), new Array[E](0), 0, length)



  def size = length


  def data  = _data
  def index = _index
  def activeSize = used


  def repr = this

  def contains(i: Int) = _index.contains(i)

  def apply(i: Int) = {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")

    var off = 0
    var acc = ring.zero
    while(off < used) {
      if(_index(off) == i) acc = ring.+(acc, _data(off))
      off += 1
    }

    acc
  }

  def update(i: Int, v: E) {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    var marked = false
    var off = 0
    while(off < used) {
      if(_index(off) == i) {
        if(!marked)
          _data(off) = v
        else _data(off) = ring.zero
        marked = true
      }

      off += 1
    }
  }

  def add(i: Int, v: E) {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")

    if(_data.length <= used) {
      _data = ArrayUtil.copyOf(_data, math.max(_data.length * 2, 1))
      _index = ArrayUtil.copyOf(_index, math.max(_index.length * 2, 1))
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
   (index.iterator zip data.iterator).take(used).mkString(s"VectorBuilder($length)(",", ", ")")
  }

  def copy: VectorBuilder[E] = {
    new VectorBuilder[E](ArrayUtil.copyOf(index, index.length), ArrayUtil.copyOf(data, index.length), activeSize, size)
  }

  def zerosLike: VectorBuilder[E] = {
    new VectorBuilder[E](new Array[Int](0), ArrayUtil.newArrayLike(data, 0), 0, size)
  }

  def reserve(nnz: Int) {
    if(nnz < _data.length) {
      _data = ArrayUtil.copyOf(_data, nnz)
      _index = ArrayUtil.copyOf(_index, nnz)
    }
  }

  def toHashVector: HashVector[E] = {
    implicit val man = ClassTag[E](_data.getClass.getComponentType.asInstanceOf[Class[E]])
    val hv = HashVector.zeros[E](length)
    var i = 0
    while(i < used) {
      hv(index(i)) = ring.+(hv(index(i)),data(i))
      i += 1
    }
    hv
  }

  def toDenseVector: DenseVector[E] = {
    implicit val man = ClassTag[E](_data.getClass.getComponentType.asInstanceOf[Class[E]])
    val hv = DenseVector.zeros[E](length)
    var i = 0
    while(i < used) {
      hv(index(i)) = ring.+(hv(index(i)),data(i))
      i += 1
    }
    hv
  }

  def toSparseVector:SparseVector[E] = toSparseVector(alreadySorted=false)

  def toSparseVector(alreadySorted: Boolean = false, keysAlreadyUnique: Boolean = false): SparseVector[E] = {
    val index = this.index
    val values = this.data
    if(alreadySorted && keysAlreadyUnique) {
      return new SparseVector(index, values, used, length)
    }

    val outIndex = new Array[Int](index.length)
    val outValues = ArrayUtil.newArrayLike(values, values.length)

    val ord = if(!alreadySorted) sortedIndices(index) else VectorBuilder.range(used)
    if(ord.length > 0) {
      outIndex(0) = index(ord(0))
      outValues(0) = values(ord(0))
      if(index(ord.last) >= length)
        throw new RuntimeException("Index " + index(ord.last) + " exceeds dimension " + length)
      else if (outIndex(0) < 0)
        throw new RuntimeException("Index " + outIndex(0) + " is less than 0!")
    }
    var i   = 1
    var out = 0
    if(keysAlreadyUnique) {
      while(i < ord.length) {
        out += 1
        outIndex(out) = index(ord(i))
        outValues(out) = values(ord(i))
        i += 1
      }
    } else {
      while(i < ord.length) {
        if(outIndex(out) == index(ord(i))) {
          outValues(out) = ring.+(outValues(out), values(ord(i)))
        } else {
          out += 1
          outIndex(out) = index(ord(i))
          outValues(out) = values(ord(i))
        }
        i += 1
      }
    }

    if(ord.length > 0)
      out += 1

    require(ord.length == 0 || length > outIndex.last, "Index out of bounds in constructing sparse vector.")
    new SparseVector(outIndex, outValues, out, length)
  }

  private def sortedIndices(indices: Array[Int]) = {
    val arr = VectorBuilder.range(used)
    Sorting.indexSort(arr, 0, used, indices)
    arr
  }


  def compact() {
    val ah = toSparseVector
    clear()
    reserve(ah.activeSize)
    var i = 0
    while(i < ah.iterableSize) {
      if(ah.isActive(i)) {
        add(ah.index(i), ah.data(i))
      }
      i += 1
    }
  }

  def clear() {
    used = 0
    _index = new Array[Int](0)
    _data = ArrayUtil.newArrayLike(data, 0)
  }


  override def equals(p1: Any): Boolean = (this eq p1.asInstanceOf[AnyRef]) || (p1 match {
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
  def use(index: Array[Int], data: Array[E], activeSize: Int) {
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

  def toVector = {
    if(size < 40 || activeSize > size / 2) {
      toDenseVector
    } else {
      toSparseVector
    }

  }
}

object VectorBuilder extends VectorBuilderOps {

  def zeros[@spec(Double, Float, Int) V: ClassTag:Semiring:DefaultArrayValue](size: Int, initialNonzero: Int = 16) = new VectorBuilder(size, initialNonzero)
  def apply[@spec(Double, Float, Int) V:Semiring:DefaultArrayValue](values: Array[V]) = new VectorBuilder(Array.range(0,values.length), values, values.length, values.length)

  def apply[V:ClassTag:Semiring:DefaultArrayValue](values: V*):VectorBuilder[V] = apply(values.toArray)
  def fill[@spec(Double, Int, Float) V:ClassTag:Semiring:DefaultArrayValue](size: Int)(v: =>V):VectorBuilder[V] = apply(Array.fill(size)(v))
  def tabulate[@spec(Double, Int, Float) V:ClassTag:Semiring:DefaultArrayValue](size: Int)(f: Int=>V):VectorBuilder[V]= apply(Array.tabulate(size)(f))

  def apply[V:ClassTag:Semiring:DefaultArrayValue](length: Int)(values: (Int, V)*) = {
    val r = zeros[V](length)
    for( (i, v) <- values) {
      r.add(i, v)
    }
    r
  }


  // implicits
  class CanCopyBuilder[@spec(Int, Float, Double) V:ClassTag:Semiring:DefaultArrayValue] extends CanCopy[VectorBuilder[V]] {
    def apply(v1: VectorBuilder[V]) = {
      v1.copy
    }
  }

  class CanZerosBuilder[@spec(Int, Float, Double) V:ClassTag:Semiring:DefaultArrayValue] extends CanCreateZerosLike[VectorBuilder[V], VectorBuilder[V]] {
    def apply(v1: VectorBuilder[V]) = {
      v1.zerosLike
    }
  }

  implicit def canCopyBuilder[@spec(Int, Float, Double) V: ClassTag: Semiring:DefaultArrayValue] = new CanCopyBuilder[V]
  implicit def canZerosBuilder[@spec(Int, Float, Double) V: ClassTag: Semiring:DefaultArrayValue] = new CanZerosBuilder[V]

  implicit def negFromScale[@spec(Int, Float, Double)  V](implicit scale: OpMulScalar.Impl2[VectorBuilder[V], V, VectorBuilder[V]], field: Ring[V]) = {
    new OpNeg.Impl[VectorBuilder[V], VectorBuilder[V]] {
      override def apply(a : VectorBuilder[V]) = {
        scale(a, field.negate(field.one))
      }
    }
  }


  // private stuff


  // Sigh, Array.range is slow.
  private[linalg] def range(length: Int) = {
    val result = new Array[Int](length)
    var i = 0
    while(i < length) {
      result(i) = i
      i += 1
    }
    result
  }

}


