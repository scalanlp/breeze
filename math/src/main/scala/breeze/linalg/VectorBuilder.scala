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
import breeze.generic.{CanMapValues, URFunc}
import breeze.math.{Field, MutableVectorSpace, Semiring, Ring}
import breeze.storage.DefaultArrayValue


/**
 * A VectorBuilder is basically unsorted Sparse Vector. Two parallel
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
 * vector type things.
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
                                                 man: ClassManifest[E],
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
    (index.iterator zip data.iterator).take(used).mkString("USVector(",", ", ")")
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

  def toHashVector = {
    implicit val man = ClassManifest.fromClass(_data.getClass.getComponentType.asInstanceOf[Class[E]])
    val hv = HashVector.zeros[E](length)
    var i = 0
    while(i < used) {
      hv(index(i)) = ring.+(hv(index(i)),data(i))
      i += 1
    }
    hv
  }

  def toSparseVector = {
    val index = this.index
    val values = this.data

    val outIndex = new Array[Int](index.length)
    val outValues = ArrayUtil.newArrayLike(values, values.length)

    val ord = sortedIndices(index)
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
}

object VectorBuilder extends VectorBuilderOps_Double {

  def zeros[@spec(Double, Float, Int) V: ClassManifest:Semiring:DefaultArrayValue](size: Int, initialNonzero: Int = 16) = new VectorBuilder(size, initialNonzero)
  def apply[@spec(Double, Float, Int) V:Semiring:DefaultArrayValue](values: Array[V]) = new VectorBuilder(Array.range(0,values.length), values, values.length, values.length)

  def apply[V:ClassManifest:Semiring:DefaultArrayValue](values: V*):VectorBuilder[V] = apply(values.toArray)
  def fill[@spec(Double, Int, Float) V:ClassManifest:Semiring:DefaultArrayValue](size: Int)(v: =>V):VectorBuilder[V] = apply(Array.fill(size)(v))
  def tabulate[@spec(Double, Int, Float) V:ClassManifest:Semiring:DefaultArrayValue](size: Int)(f: Int=>V):VectorBuilder[V]= apply(Array.tabulate(size)(f))

  def apply[V:ClassManifest:Semiring:DefaultArrayValue](length: Int)(values: (Int, V)*) = {
    val r = zeros[V](length)
    for( (i, v) <- values) {
      r(i) = v
    }
    r
  }


  // implicits
  class CanCopyBuilder[@spec(Int, Float, Double) V:ClassManifest:Semiring:DefaultArrayValue] extends CanCopy[VectorBuilder[V]] {
    def apply(v1: VectorBuilder[V]) = {
      v1.copy
    }
  }

  class CanZerosBuilder[@spec(Int, Float, Double) V:ClassManifest:Semiring:DefaultArrayValue] extends CanCreateZerosLike[VectorBuilder[V], VectorBuilder[V]] {
    def apply(v1: VectorBuilder[V]) = {
      v1.zerosLike
    }
  }

  implicit def canCopyBuilder[@spec(Int, Float, Double) V: ClassManifest: Semiring:DefaultArrayValue] = new CanCopyBuilder[V]
  implicit def canZerosBuilder[@spec(Int, Float, Double) V: ClassManifest: Semiring:DefaultArrayValue] = new CanZerosBuilder[V]

  implicit def negFromScale[@spec(Int, Float, Double)  V, Double](implicit scale: BinaryOp[VectorBuilder[V], V, OpMulScalar, VectorBuilder[V]], field: Ring[V]) = {
    new UnaryOp[VectorBuilder[V], OpNeg, VectorBuilder[V]] {
      override def apply(a : VectorBuilder[V]) = {
        scale(a, field.negate(field.one))
      }
    }
  }


  // private stuff


  // Sigh, Array.range is slow.
  private def range(length: Int) = {
    val result = new Array[Int](length)
    var i = 0
    while(i < length) {
      result(i) = i
      i += 1
    }
    result
  }

}

trait VectorBuilderOps_Double { this: VectorBuilder.type =>
  implicit val canScaleInto_Double: BinaryUpdateOp[VectorBuilder[Double], Double, OpMulScalar] =  {
    new  BinaryUpdateOp[VectorBuilder[Double], Double, OpMulScalar]  {
      def apply(a: VectorBuilder[Double], b: Double) {
        var i = 0
        while(i < a.activeSize) {
          a.data(i) *= b
          i += 1
        }
      }
    }

  }

  implicit val canDivInto_Double: BinaryUpdateOp[VectorBuilder[Double], Double, OpDiv] =  {
    new  BinaryUpdateOp[VectorBuilder[Double], Double, OpDiv]  {
      def apply(a: VectorBuilder[Double], b: Double) {
        var i = 0
        while(i < a.activeSize) {
          a.data(i) /= b
          i += 1
        }
      }
    }

  }

  implicit val canAddInto_VV_Double: BinaryUpdateOp[VectorBuilder[Double], VectorBuilder[Double], OpAdd] =  {
    new  BinaryUpdateOp[VectorBuilder[Double], VectorBuilder[Double], OpAdd]  {
      def apply(a: VectorBuilder[Double], b: VectorBuilder[Double]) {
        require(a.length == b.length, "Dimension mismatch!")
        a.reserve(a.activeSize + b.activeSize)
        var i = 0
        while(i < b.activeSize) {
          a.add(b.index(i), b.data(i))
          i += 1
        }
      }
    }

  }

  implicit val canSubInto_VV_Double: BinaryUpdateOp[VectorBuilder[Double], VectorBuilder[Double], OpSub] =  {
    new  BinaryUpdateOp[VectorBuilder[Double], VectorBuilder[Double], OpSub]  {
      def apply(a: VectorBuilder[Double], b: VectorBuilder[Double]) {
        require(a.length == b.length, "Dimension mismatch!")
        a.reserve(a.activeSize + b.activeSize)
        var i = 0
        while(i < b.activeSize) {
          a.add(b.index(i), -b.data(i))
          i += 1
        }
      }
    }

  }


  implicit val canSet_Double: BinaryUpdateOp[VectorBuilder[Double], VectorBuilder[Double], OpSet] =  {
    new  BinaryUpdateOp[VectorBuilder[Double], VectorBuilder[Double], OpSet]  {
      def apply(a: VectorBuilder[Double], b: VectorBuilder[Double]) {
        a.clear()
        a.reserve(b.activeSize)
        var i = 0
        while(i < b.activeSize) {
          a.add(b.index(i), b.data(i))
          i += 1
        }
      }
    }

  }


  implicit val mulVS_Double: BinaryOp[VectorBuilder[Double], Double, OpMulScalar, VectorBuilder[Double]] = {
    BinaryOp.fromCopyAndUpdate[VectorBuilder[Double], Double, OpMulScalar]
  }

  implicit val divVS_Double: BinaryOp[VectorBuilder[Double], Double, OpDiv, VectorBuilder[Double]] = {
    BinaryOp.fromCopyAndUpdate[VectorBuilder[Double], Double, OpDiv]
  }

  implicit val addVV_Double: BinaryOp[VectorBuilder[Double], VectorBuilder[Double], OpAdd, VectorBuilder[Double]] = {
    BinaryOp.fromCopyAndUpdate[VectorBuilder[Double], VectorBuilder[Double], OpAdd]
  }

  implicit val subVV_Double: BinaryOp[VectorBuilder[Double], VectorBuilder[Double], OpSub, VectorBuilder[Double]] = {
    BinaryOp.fromCopyAndUpdate[VectorBuilder[Double], VectorBuilder[Double], OpSub]
  }

  implicit val neg_Double: UnaryOp[VectorBuilder[Double], OpNeg, VectorBuilder[Double]] = {
    new UnaryOp[VectorBuilder[Double], OpNeg, VectorBuilder[Double]] {
      def apply(a: VectorBuilder[Double]): VectorBuilder[Double] = {
        val c = a.zerosLike
        c.reserve(a.size)
        var i = 0
        while(i < a.activeSize) {
          c.add(a.index(i), -a.data(i))
          i += 1
        }
        c
      }
    }
  }

  implicit val canAxpy_VB_VB_Double: CanAxpy[Double, VectorBuilder[Double], VectorBuilder[Double]] = {
    new  CanAxpy[Double, VectorBuilder[Double], VectorBuilder[Double]]  {
      def apply(s: Double, b: VectorBuilder[Double], a: VectorBuilder[Double]) {
        require(a.length == b.length, "Dimension mismatch!")
        a.reserve(b.activeSize + a.activeSize)
        var i = 0
        val bd = b.data
        while(i < b.activeSize) {
          a.add(b.index(i), s * bd(i))
          i += 1
        }
      }
    }
  }

  implicit val mvector_space_Double: MutableVectorSpace[VectorBuilder[Double], Double] = {
    new MutableVectorSpace[VectorBuilder[Double], Double] {
      def field: Field[Double] = Field.fieldD

       def isNumericOps(v: VectorBuilder[Double]): NumericOps[VectorBuilder[Double]] = v

       def zeros: CanCreateZerosLike[VectorBuilder[Double], VectorBuilder[Double]] = VectorBuilder.canZerosBuilder[Double]

       def copy: CanCopy[VectorBuilder[Double]] = VectorBuilder.canCopyBuilder[Double]

       def mulIntoVS: BinaryUpdateOp[VectorBuilder[Double], Double, OpMulScalar] = canScaleInto_Double

       def divIntoVS: BinaryUpdateOp[VectorBuilder[Double], Double, OpDiv] = canDivInto_Double

       def addIntoVV: BinaryUpdateOp[VectorBuilder[Double], VectorBuilder[Double], OpAdd] = canAddInto_VV_Double
       def subIntoVV: BinaryUpdateOp[VectorBuilder[Double], VectorBuilder[Double], OpSub] = canSubInto_VV_Double


      def mulVS: BinaryOp[VectorBuilder[Double], Double, OpMulScalar, VectorBuilder[Double]] = {
        BinaryOp.fromCopyAndUpdate[VectorBuilder[Double], Double, OpMulScalar]
      }

      def divVS: BinaryOp[VectorBuilder[Double], Double, OpDiv, VectorBuilder[Double]] = {
        BinaryOp.fromCopyAndUpdate[VectorBuilder[Double], Double, OpDiv]
      }

      def addVV: BinaryOp[VectorBuilder[Double], VectorBuilder[Double], OpAdd, VectorBuilder[Double]] = {
        BinaryOp.fromCopyAndUpdate[VectorBuilder[Double], VectorBuilder[Double], OpAdd]
      }

      def subVV: BinaryOp[VectorBuilder[Double], VectorBuilder[Double], OpSub, VectorBuilder[Double]] = {
        BinaryOp.fromCopyAndUpdate[VectorBuilder[Double], VectorBuilder[Double], OpSub]
      }


      implicit def neg: UnaryOp[VectorBuilder[Double], OpNeg, VectorBuilder[Double]] = neg_Double

      implicit def setIntoVV: BinaryUpdateOp[VectorBuilder[Double], VectorBuilder[Double], OpSet] = {
        canSet_Double
      }

      def close(a: VectorBuilder[Double], b: VectorBuilder[Double], tolerance: Double): Boolean = {
        (a.toHashVector - b.toHashVector).norm(2) < tolerance
      }

      implicit def axpyVV: CanAxpy[Double, VectorBuilder[Double], VectorBuilder[Double]] = canAxpy_VB_VB_Double
    }
  }

  // operations involving vectors:
  implicit def canAddInto_V_VB_Double[V<:Vector[Double]]: BinaryUpdateOp[V, VectorBuilder[Double], OpAdd] =  {
    new  BinaryUpdateOp[V, VectorBuilder[Double], OpAdd]  {
      def apply(a: V, b: VectorBuilder[Double]) {
        require(a.length == b.length, "Dimension mismatch!")
        var i = 0
        val bd = b.data
        while(i < b.activeSize) {
          a(b.index(i)) -= bd(i)
          i += 1
        }
      }
    }

  }

  implicit def canSubInto_V_VB_Double[V<:Vector[Double]]: BinaryUpdateOp[V, VectorBuilder[Double], OpSub] =  {
    new  BinaryUpdateOp[V, VectorBuilder[Double], OpSub]  {
      def apply(a: V, b: VectorBuilder[Double]) {
        require(a.length == b.length, "Dimension mismatch!")
        var i = 0
        val bd = b.data
        while(i < b.activeSize) {
          a(b.index(i)) -= bd(i)
          i += 1
        }
      }
    }

  }

  implicit def canAddInto_VB_V_Double[V <: Vector[Double]]: BinaryUpdateOp[VectorBuilder[Double], V, OpAdd] =  {
    new  BinaryUpdateOp[VectorBuilder[Double], V, OpAdd]  {
      def apply(a: VectorBuilder[Double], b: V) {
        b match {
          case b: StorageVector[Double] =>
            var i = 0
            val bd = b.data
            while(i < b.iterableSize) {
              if(b.isActive(i))
                a.add(b.indexAt(i), bd(i))
              i += 1
            }

          case _ =>
            a.reserve(a.activeSize + b.activeSize)
            require(a.length == b.length, "Dimension mismatch!")
            for( (i,v) <- b.activeIterator) {
              a.add(i, v)
            }
        }

      }
    }

  }

  implicit def canSubInto_VB_V_Double[V <: Vector[Double]]: BinaryUpdateOp[VectorBuilder[Double], V, OpSub] =  {
    new  BinaryUpdateOp[VectorBuilder[Double], V, OpSub]  {
      def apply(a: VectorBuilder[Double], b: V) {
        b match {
          case b: StorageVector[Double] =>
            var i = 0
            val bd = b.data
            while(i < b.iterableSize) {
              if(b.isActive(i))
                a.add(b.indexAt(i), -bd(i))
              i += 1
            }

          case _ =>
            a.reserve(a.activeSize + b.activeSize)
            require(a.length == b.length, "Dimension mismatch!")
            for( (i,v) <- b.activeIterator) {
              a.add(i, -v)
            }
        }

      }
    }

  }

  implicit def canDot_V_VB_Double[V<:Vector[Double]]: BinaryOp[V, VectorBuilder[Double], OpMulInner, Double] =  {
    new  BinaryOp[V, VectorBuilder[Double], OpMulInner, Double]  {
      def apply(a: V, b: VectorBuilder[Double]) =  {
        require(a.length == b.length, "Dimension mismatch!")
        var result : Double = 0
        var i = 0
        val bd = b.data
        while(i < b.activeSize) {
          result += a(b.index(i)) * bd(i)
          i += 1
        }
        result
      }
    }
  }

  implicit def canAxpy_V_VB_Double[V<:Vector[Double]]: CanAxpy[Double, VectorBuilder[Double], V] = {
    new  CanAxpy[Double, VectorBuilder[Double], V]  {
      def apply(s: Double, b: VectorBuilder[Double], a: V) {
        require(a.length == b.length, "Dimension mismatch!")
        var i = 0
        val bd = b.data
        while(i < b.activeSize) {
          a(b.index(i)) += s * bd(i)
          i += 1
        }
      }
    }
  }

  implicit def canDot_VB_V_Double[V<:Vector[Double]]: BinaryOp[VectorBuilder[Double], V, OpMulInner, Double] =  {
    new  BinaryOp[VectorBuilder[Double], V, OpMulInner, Double]  {
      def apply(a: VectorBuilder[Double], b: V) =  {
        canDot_V_VB_Double(b,a)
      }
    }
  }


}
