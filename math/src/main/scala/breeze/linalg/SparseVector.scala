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
import operators.{UnaryOp, OpNeg, BinaryOp, OpMulScalar}
import scala.{specialized=>spec}
import breeze.storage.{DefaultArrayValue}
import support.{CanZipMapValues, CanMapKeyValuePairs, CanCopy, CanSlice}
import breeze.util.ArrayUtil
import breeze.generic.{CanMapValues, URFunc, UReduceable}
import breeze.math.{Semiring, Ring, TensorSpace}
import breeze.collection.mutable.SparseArray


/**
 * A Binary-search backed vector
 * @author dlwh
 */
@SerialVersionUID(1)
class SparseVector[@spec(Double,Int, Float) E](val array: SparseArray[E])
                                                    (implicit value: DefaultArrayValue[E])
                                                    extends StorageVector[E]
                                                    with VectorLike[E, SparseVector[E]] with Serializable {

  def this(index: Array[Int], data: Array[E], activeSize: Int, length: Int)(implicit value: DefaultArrayValue[E])  = this(new SparseArray(index, data, activeSize, length, value.value))
  def this(index: Array[Int], data: Array[E], length: Int)(implicit value: DefaultArrayValue[E])  = this(index, data, index.length, length)

  def data  = array.data
  def index = array.index
  def activeSize = array.activeSize
  def used = activeSize
  def length = array.length

  def repr = this

  def contains(i: Int) = array.contains(i)

  def apply(i: Int) = {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    array(i)
  }

  def update(i: Int, v: E) {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    array(i) = v
  }

  def activeIterator = activeKeysIterator zip activeValuesIterator

  def activeValuesIterator = data.iterator.take(activeSize)

  def activeKeysIterator = index.iterator.take(activeSize)

  // TODO: allow this to vary
  /** This is always assumed to be equal to 0, for now. */
  def default = value.value

  override def equals(p1: Any) = p1 match {
    case x: Vector[_] =>
        this.length == x.length &&
          (valuesIterator sameElements x.valuesIterator)
    case _ => false
  }

  def isActive(rawIndex: Int) = array.isActive(rawIndex)

  override def toString = {
    activeIterator.mkString("SparseVector(",", ", ")")
  }

  override def ureduce[Final](f: URFunc[E, Final]) = {
    f(data, activeSize)
  }

  def copy: SparseVector[E] = {
    new SparseVector[E](ArrayUtil.copyOf(index, index.length), ArrayUtil.copyOf(data, index.length), activeSize, size)
  }

  def reserve(nnz: Int) {
    array.reserve(nnz)
  }

  def compact() {
    array.compact()
  }

  /**
   * Sets the underlying sparse array to use this data
   * @param index must be a sorted list of indices
   * @param data values corresponding to the index
   * @param activeSize number of active elements. The first activeSize will be used.
   */
  def use(index: Array[Int], data: Array[E], activeSize: Int) {
    require(activeSize <= size, "Can't have more elements in the array than length!")
    require(activeSize >= 0, "activeSize must be non-negative")
    require(data.length >= activeSize, "activeSize must be no greater than array length...")
    array.use(index, data, activeSize)
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
}

object SparseVector extends SparseVectorOps_Int with SparseVectorOps_Float with SparseVectorOps_Double {
  def zeros[@spec(Double, Float, Int) V: ClassManifest:DefaultArrayValue](size: Int) = new SparseVector(Array.empty, Array.empty[V], 0, size)
  def apply[@spec(Double, Float, Int) V:DefaultArrayValue](values: Array[V]) = new SparseVector(Array.range(0,values.length), values, values.length, values.length)

  def apply[V:ClassManifest:DefaultArrayValue](values: V*):SparseVector[V] = apply(values.toArray)
  def fill[@spec(Double, Int, Float) V:ClassManifest:DefaultArrayValue](size: Int)(v: =>V):SparseVector[V] = apply(Array.fill(size)(v))
  def tabulate[@spec(Double, Int, Float) V:ClassManifest:DefaultArrayValue](size: Int)(f: Int=>V):SparseVector[V]= apply(Array.tabulate(size)(f))

  def apply[V:ClassManifest:DefaultArrayValue](length: Int)(values: (Int, V)*) = {
    val r = zeros[V](length)
    for( (i, v) <- values) {
      r(i) = v
    }
    r
  }


  /**
   * Helper class for building a sparse vector.
   * Sorts things when you want the result
   * @param dim
   */
  class Builder[@spec(Int, Float, Double) V:ClassManifest:DefaultArrayValue:Semiring](var dim: Int) {
    private val index = ClassManifest.Int.newArrayBuilder()
    private val values = implicitly[ClassManifest[V]].newArrayBuilder()
    private def ring = implicitly[Semiring[V]]


    def add(i: Int, v: V) = {
      index += i
      values += v
    }

    def result() = {
      val index = this.index.result()
      val values = this.values.result()

      val outIndex = new Array[Int](index.length)
      val outValues = new Array[V](values.length)

      val ord = sortedIndices(index)
      if(ord.length > 0) {
        outIndex(0) = index(ord(0))
        outValues(0) = values(ord(0))
      }
      var i   = 1
      var out = 0
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

      require(i == 0 || dim > outIndex.last, "Index out of bounds in constructing sparse vector.")
      new SparseVector(outIndex, outValues, out, dim)
    }

    private def sortedIndices(indices: Array[Int]) = {
      Array.range(0, indices.length).sortWith { (i, j) =>
        (indices(i) < indices(j))
      }
    }



  }



  // implicits
  class CanCopySparseVector[@spec(Int, Float, Double) V:ClassManifest:DefaultArrayValue] extends CanCopy[SparseVector[V]] {
    def apply(v1: SparseVector[V]) = {
      v1.copy
    }
  }

  implicit def canCopySparse[@spec(Int, Float, Double) V: ClassManifest: DefaultArrayValue] = new CanCopySparseVector[V]

  implicit def canMapValues[V, V2: ClassManifest: DefaultArrayValue]:CanMapValues[SparseVector[V], V, V2, SparseVector[V2]] = {
    new CanMapValues[SparseVector[V], V, V2, SparseVector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def map(from: SparseVector[V], fn: (V) => V2) = {
        SparseVector.tabulate(from.length)(i => fn(from(i)))
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: SparseVector[V], fn: (V) => V2) = {
        val out = new Array[V2](from.activeSize)
        var i = 0
        while(i < from.activeSize) {
          out(i) = fn(from.data(i))
          i += 1
        }
        new SparseVector(from.index.take(from.activeSize), out, from.activeSize, from.length)
      }
    }
  }

  implicit def canMapPairs[V, V2: ClassManifest: DefaultArrayValue]:CanMapKeyValuePairs[SparseVector[V], Int, V, V2, SparseVector[V2]] = {
    new CanMapKeyValuePairs[SparseVector[V], Int, V, V2, SparseVector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def map(from: SparseVector[V], fn: (Int, V) => V2) = {
        SparseVector.tabulate(from.length)(i => fn(i, from(i)))
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: SparseVector[V], fn: (Int, V) => V2) = {
        val out = new Array[V2](from.used)
        var i = 0
        while(i < from.used) {
          out(i) = fn(i, from.data(i))
          i += 1
        }
        new SparseVector(from.index.take(from.used), out, from.used, from.length)
      }
    }
  }

  class CanZipMapValuesSparseVector[@spec(Int, Double, Float) V, @spec(Int, Double) RV:ClassManifest:DefaultArrayValue] extends CanZipMapValues[SparseVector[V],V,RV,SparseVector[RV]] {
    def create(length : Int) = zeros(length)

    /**Maps all corresponding values from the two collection. */
    def map(from: SparseVector[V], from2: SparseVector[V], fn: (V, V) => RV) = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result = create(from.length)
      var i = 0
      while (i < from.length) {
        result(i) = fn(from(i), from2(i))
        i += 1
      }
      result
    }
  }
  implicit def zipMap[V, R:ClassManifest:DefaultArrayValue] = new CanZipMapValuesSparseVector[V, R]
  implicit val zipMap_d = new CanZipMapValuesSparseVector[Double, Double]
  implicit val zipMap_f = new CanZipMapValuesSparseVector[Float, Float]
  implicit val zipMap_i = new CanZipMapValuesSparseVector[Int, Int]


  implicit def negFromScale[@spec(Int, Float, Double)  V, Double](implicit scale: BinaryOp[SparseVector[V], V, OpMulScalar, SparseVector[V]], field: Ring[V]) = {
    new UnaryOp[SparseVector[V], OpNeg, SparseVector[V]] {
      override def apply(a : SparseVector[V]) = {
        scale(a, field.negate(field.one))
      }
    }
  }


  implicit val space_d = TensorSpace.make[SparseVector[Double], Int, Double]
  implicit val space_f = TensorSpace.make[SparseVector[Float], Int, Float]
  implicit val space_i = TensorSpace.make[SparseVector[Int], Int, Int]

}
