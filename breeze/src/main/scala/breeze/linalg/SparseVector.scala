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
import breeze.storage.DefaultArrayValue
import breeze.linalg.support._
import breeze.util.ArrayUtil
import breeze.math.{Complex, Ring, TensorSpace}
import breeze.collection.mutable.SparseArray
import collection.mutable
import scala.reflect.ClassTag
import CanTraverseValues.ValuesVisitor
import breeze.generic.UFunc.UImpl


/**
 * A Binary-search backed vector.
 * There is a parallel array of ints (in 0 until length) and values, sorted by index value.
 * To quickly access all stored values use the following loop:
 *
 * {{{
 *  var offset = 0
 *  while( offset < v.activeSize) {
 *    val index: Int = v.indexAt(offset)
 *    val value: E = v.valueAt(offset)
 *
 *    offset += 1
 *  }
 * }}}
 *
 *@author dlwh
 */
@SerialVersionUID(1)
class SparseVector[@spec(Double,Int, Float) E](val array: SparseArray[E])
                                              (implicit value: DefaultArrayValue[E])
                                              extends StorageVector[E]
                                              with VectorLike[E, SparseVector[E]] with Serializable {

  /** This auxiliary constructor assumes that the index array is already sorted. */
  def this(index: Array[Int], data: Array[E], activeSize: Int, length: Int)(implicit value: DefaultArrayValue[E])  = this(new SparseArray(index, data, activeSize, length, value.value))
  /** This auxiliary constructor assumes that the index array is already sorted. */
  def this(index: Array[Int], data: Array[E], length: Int)(implicit value: DefaultArrayValue[E])  = this(index, data, index.length, length)


  // Don't delete
  SparseVector.init()

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

object SparseVector extends SparseVectorOps
            with DenseVector_SparseVector_Ops
            with SparseVector_DenseVector_Ops
             {

  def zeros[@spec(Double, Float, Int) V: ClassTag:DefaultArrayValue](size: Int) = new SparseVector(Array.empty, Array.empty[V], 0, size)
  def apply[@spec(Double, Float, Int) V:DefaultArrayValue](values: Array[V]) = new SparseVector(Array.range(0,values.length), values, values.length, values.length)

  def apply[V:ClassTag:DefaultArrayValue](values: V*):SparseVector[V] = apply(values.toArray)
  def fill[@spec(Double, Int, Float) V:ClassTag:DefaultArrayValue](size: Int)(v: =>V):SparseVector[V] = apply(Array.fill(size)(v))
  def tabulate[@spec(Double, Int, Float) V:ClassTag:DefaultArrayValue](size: Int)(f: Int=>V):SparseVector[V]= apply(Array.tabulate(size)(f))

  def apply[V:ClassTag:DefaultArrayValue](length: Int)(values: (Int, V)*) = {
    val r = zeros[V](length)
    for( (i, v) <- values) {
      r(i) = v
    }
    r
  }

  def vertcat[V:DefaultArrayValue:ClassTag](vectors: SparseVector[V]*): SparseVector[V] = {
    val resultArray = vectors.map(_.array).foldLeft(new SparseArray[V](0))(_ concatenate _)
    new SparseVector(resultArray)
  }

  def horzcat[V:DefaultArrayValue:ClassTag](vectors: SparseVector[V]*):CSCMatrix[V] ={
    if(!vectors.forall(_.size==vectors(0).size))
      throw new IllegalArgumentException("vector lengths must be equal, but got: " + vectors.map(_.length).mkString(", "))
    val rows = vectors(0).length
    val cols = vectors.length
    val data = new Array[V](vectors.map(_.data.length).sum)
    val rowIndices = new Array[Int](data.length)
    val colPtrs = new Array[Int](vectors.length + 1)
    val used = data.length

    var vec = 0
    var off = 0
    while(vec < vectors.length) {
      colPtrs(vec) = off
      System.arraycopy(vectors(vec).data, 0, data, off, vectors(vec).activeSize)
      System.arraycopy(vectors(vec).index, 0, rowIndices, off, vectors(vec).activeSize)
      off += vectors(vec).activeSize
      vec += 1
    }
    colPtrs(vec) = off

    new CSCMatrix(data, rows, cols, colPtrs, used, rowIndices)
  }

  // implicits
  class CanCopySparseVector[@spec(Int, Float, Double) V:ClassTag:DefaultArrayValue] extends CanCopy[SparseVector[V]] {
    def apply(v1: SparseVector[V]) = {
      v1.copy
    }
  }

  implicit def canCopySparse[@spec(Int, Float, Double) V: ClassTag: DefaultArrayValue] = new CanCopySparseVector[V]

  implicit def canMapValues[V, V2: ClassTag: DefaultArrayValue]:CanMapValues[SparseVector[V], V, V2, SparseVector[V2]] = {
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
  implicit def handholdCMV[T]= new CanMapValues.HandHold[SparseVector[T], T]

  implicit def canIterateValues[V]:CanTraverseValues[SparseVector[V], V] = {
    new CanTraverseValues[SparseVector[V],V] {


      def isTraversableAgain(from: SparseVector[V]): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: SparseVector[V], fn: ValuesVisitor[V]): Unit = {
        fn.zeros(from.size - from.activeSize, from.default)
        fn.visitArray(from.data, 0, from.activeSize, 1)
      }
    }
  }

  implicit def canTraverseKeyValuePairs[V]:CanTraverseKeyValuePairs[SparseVector[V], Int, V] = {
    new CanTraverseKeyValuePairs[SparseVector[V], Int, V] {
      def isTraversableAgain(from: SparseVector[V]): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: SparseVector[V], fn: CanTraverseKeyValuePairs.KeyValuePairsVisitor[Int, V]): Unit = {
        import from._

        fn.visitArray(index, data, 0, activeSize, 1)
        if(activeSize != size) {
          fn.zeros(size - activeSize, Iterator.range(0, size).filterNot(index contains _), from.default)
        }
      }

    }
  }

  implicit def canTransformValues[V:DefaultArrayValue:ClassTag]:CanTransformValues[SparseVector[V], V, V] = {
    new CanTransformValues[SparseVector[V], V, V] {
      val z = implicitly[DefaultArrayValue[V]]
      /**Transforms all key-value pairs from the given collection. */
      def transform(from: SparseVector[V], fn: (V) => V) {
        val newData =  mutable.ArrayBuilder.make[V]()
        val newIndex = mutable.ArrayBuilder.make[Int]()
        var used = 0
        var i = 0
        while(i < from.length) {
          val vv = fn(from(i))
          if(vv != z) {
            newData += vv
            newIndex += i
            used += 1
          }
          i += 1
        }
        from.array.use(newIndex.result(), newData.result(), used)
      }

      /**Transforms all active key-value pairs from the given collection. */
      def transformActive(from: SparseVector[V], fn: (V) => V) {
        var i = 0
        while(i < from.activeSize) {
          from.data(i) = fn(from.data(i))
          i += 1
        }
      }
    }
  }


  implicit def canMapPairs[V, V2: ClassTag: DefaultArrayValue]:CanMapKeyValuePairs[SparseVector[V], Int, V, V2, SparseVector[V2]] = {
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
          out(i) = fn(from.index(i), from.data(i))
          i += 1
        }
        new SparseVector(from.index.take(from.used), out, from.used, from.length)
      }
    }
  }

  implicit val space_d: TensorSpace[SparseVector[Double], Int, Double] = TensorSpace.make[SparseVector[Double], Int, Double]
  implicit val space_f: TensorSpace[SparseVector[Float], Int, Float] = {
    val nop = this.negFromScale[Float]
    TensorSpace.make[SparseVector[Float], Int, Float]
  }
  implicit val space_i: TensorSpace[SparseVector[Int], Int, Int] = TensorSpace.make[SparseVector[Int], Int, Int]
  
  implicit def canTranspose[V:ClassTag:DefaultArrayValue]: CanTranspose[SparseVector[V], CSCMatrix[V]] = {
    new CanTranspose[SparseVector[V], CSCMatrix[V]] {
      def apply(from: SparseVector[V]) = {
        val transposedMtx = CSCMatrix.zeros[V](1, from.length)
        var i = 0
        while (i < from.activeSize) {
          val c = from.index(i)
          transposedMtx(0, c) = from.data(i)
          i += 1
        }
        transposedMtx
      }
    }
  }
  
  implicit def canTransposeComplex: CanTranspose[SparseVector[Complex], CSCMatrix[Complex]] = {
    new CanTranspose[SparseVector[Complex], CSCMatrix[Complex]] {
      def apply(from: SparseVector[Complex]) = {
        val transposedMtx = CSCMatrix.zeros[Complex](1, from.length)
        var i = 0
        while (i < from.activeSize) {
          val c = from.index(i)
          transposedMtx(0, c) = from.data(i).conjugate
          i += 1
        }
        transposedMtx
      }
    }
  }


  @noinline
  private def init() = {}
}


