package breeze.linalg

import breeze.collection.mutable.OpenAddressHashArray
import breeze.linalg.operators._
import breeze.storage.Zero
import breeze.generic._
import breeze.linalg.support._
import breeze.math._
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3
import breeze.macros.expand
import CanTraverseValues.ValuesVisitor
import breeze.linalg.support.CanTraverseKeyValuePairs.KeyValuePairsVisitor

/**
 * A HashVector is a sparse vector backed by an OpenAddressHashArray
 * @author dlwh
 */
class HashVector[@specialized(Int, Double, Float) E](val array: OpenAddressHashArray[E]) extends Vector[E] with VectorLike[E, HashVector[E]] {

  // don't delete
  HashVector.init()

  def activeIterator: Iterator[(Int, E)] = array.activeIterator

  def activeValuesIterator: Iterator[E] = array.activeValuesIterator

  def activeKeysIterator: Iterator[Int] = array.activeKeysIterator

  def apply(i: Int): E = array(i)

  def update(i: Int, v: E) {
    array(i) = v
  }

  def default = array.defaultValue

  def activeSize: Int = array.activeSize

  def length: Int = array.length

  def copy: HashVector[E] = new HashVector(array.copy)

  def repr = this

  final def iterableSize: Int = array.iterableSize
  def data = array.data
  final def index = array.index
  final def isActive(i: Int) = array.isActive(i)


  override def toString = {
    activeIterator.mkString("HashVector(",", ", ")")
  }

  def allVisitableIndicesActive:Boolean = false

  override def hashCode() = {
    var hash = 47
    // we make the hash code based on index * value, so that zeros don't affect the hashcode.
    val dv = array.default.value(array.zero)
    var i = 0
    while(i < activeSize) {
      if(isActive(i)) {
        val ind = index(i)
        val v = data(i)
        if(v != dv) {
          hash = MurmurHash3.mix(hash, v.##)
          hash = MurmurHash3.mix(hash, ind)
        }
      }

      i += 1
    }

    MurmurHash3.finalizeHash(hash, activeSize)
  }
}


object HashVector extends HashVectorOps
                          with DenseVector_HashVector_Ops
                          with HashVector_DenseVector_Ops
                          with HashVector_SparseVector_Ops
                          with SparseVector_HashVector_Ops
                           {
  def zeros[@specialized(Double, Float, Int) V: ClassTag:Zero](size: Int) = {
    new HashVector(new OpenAddressHashArray[V](size))
  }
  def apply[@specialized(Double, Float, Int) V:Zero](values: Array[V]) = {
    implicit val man = ClassTag[V](values.getClass.getComponentType.asInstanceOf[Class[V]])
    val oah = new OpenAddressHashArray[V](values.length)
    for( (v,i) <- values.zipWithIndex) oah(i) = v
    new HashVector(oah)
  }

  def apply[V:ClassTag:Zero](values: V*):HashVector[V] = {
    apply(values.toArray)
  }
  def fill[@specialized(Double, Int, Float) V:ClassTag:Zero](size: Int)(v: =>V):HashVector[V] = apply(Array.fill(size)(v))
  def tabulate[@specialized(Double, Int, Float) V:ClassTag:Zero](size: Int)(f: Int=>V):HashVector[V]= apply(Array.tabulate(size)(f))

  def apply[V:ClassTag:Zero](length: Int)(values: (Int, V)*) = {
    val r = zeros[V](length)
    for( (i, v) <- values) {
      r(i) = v
    }
    r
  }

  // implicits


  implicit def canCreateZeros[V:ClassTag:Zero]: CanCreateZeros[HashVector[V],Int] =
    new CanCreateZeros[HashVector[V],Int] {
      def apply(d: Int): HashVector[V] = {
        zeros[V](d)
      }
    }


  // implicits
  class CanCopyHashVector[@specialized(Int, Float, Double) V:ClassTag:Zero] extends CanCopy[HashVector[V]] {
    def apply(v1: HashVector[V]) = {
      v1.copy
    }
  }

  implicit def canCopyHash[@specialized(Int, Float, Double) V: ClassTag: Zero] = new CanCopyHashVector[V]

  implicit def canMapValues[V, V2: ClassTag: Zero]:CanMapValues[HashVector[V], V, V2, HashVector[V2]] = {
    new CanMapValues[HashVector[V], V, V2, HashVector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def map(from: HashVector[V], fn: (V) => V2) = {
        HashVector.tabulate(from.length)(i => fn(from(i)))
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: HashVector[V], fn: (V) => V2) = {
        val out = new OpenAddressHashArray[V2](from.length)
        var i = 0
        while(i < from.iterableSize) {
          if(from.isActive(i))
            out(from.index(i)) = fn(from.data(i))
          i += 1
        }
        new HashVector(out)
      }
    }
  }
  implicit def handholdCMV[T]= new CanMapValues.HandHold[HashVector[T], T]

  implicit def canIterateValues[V]:CanTraverseValues[HashVector[V], V] = {
    new CanTraverseValues[HashVector[V],V] {


      def isTraversableAgain(from: HashVector[V]): Boolean = true

      def traverse(from: HashVector[V], fn: ValuesVisitor[V]): Unit = {
        fn.zeros(from.size - from.activeSize, from.default)
        var i = 0
        while(i < from.iterableSize) {
          if(from.isActive(i))
            fn.visit(from.data(i))
          i += 1
        }
      }
    }
  }

  implicit def canTraverseKeyValuePairs[V]:CanTraverseKeyValuePairs[HashVector[V], Int, V] = {
    new CanTraverseKeyValuePairs[HashVector[V], Int, V] {


      def traverse(from: HashVector[V], fn: KeyValuePairsVisitor[Int, V]): Unit = {
        fn.zeros(from.size - from.activeSize, Iterator.range(0, from.size).filterNot(from.index contains _), from.default)
        var i = 0
        while(i < from.iterableSize) {
          if(from.isActive(i))
            fn.visit(from.index(i), from.data(i))
          i += 1
        }
      }

      def isTraversableAgain(from: HashVector[V]): Boolean = true

      def traverse(from: HashVector[V], fn: ValuesVisitor[V]): Unit = {

      }
    }
  }

  implicit def canMapPairs[V, V2: ClassTag: Zero]:CanMapKeyValuePairs[HashVector[V], Int, V, V2, HashVector[V2]] = {
    new CanMapKeyValuePairs[HashVector[V], Int, V, V2, HashVector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def map(from: HashVector[V], fn: (Int, V) => V2) = {
        HashVector.tabulate(from.length)(i => fn(i, from(i)))
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: HashVector[V], fn: (Int, V) => V2) = {
        val out = new OpenAddressHashArray[V2](from.length)
        var i = 0
        while(i < from.iterableSize) {
          if(from.isActive(i))
          out(from.index(i)) = fn(from.index(i), from.data(i))
          i += 1
        }
        new HashVector(out)
      }
    }
  }

  implicit def canTabulate[E:ClassTag:Zero]: CanTabulate[Int, HashVector[E], E] = new CanTabulate[Int,HashVector[E],E] {
    def apply(d: Int, f: (Int) => E): HashVector[E] = tabulate[E](d)(f)
  }

  implicit def space[E:Field:ClassTag:Zero]: MutableFiniteCoordinateField[HashVector[E],Int,E] = {
    implicit val _dim = dim.implVDim[E, HashVector[E]]
    MutableFiniteCoordinateField.make[HashVector[E], Int, E]
  }

  import breeze.math.PowImplicits._


  @expand
  implicit def dv_hv_UpdateOp[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):Op.InPlaceImpl2[DenseVector[T], HashVector[T]] = new Op.InPlaceImpl2[DenseVector[T], HashVector[T]] {
    def apply(a: DenseVector[T], b: HashVector[T]):Unit = {
      require(a.length == b.length, "Vectors must have the same length")
      val ad = a.data
      var aoff = a.offset
      val astride = a.stride

      var i = 0
      while(i < a.length) {
        ad(aoff) = op(ad(aoff), b(i))
        aoff += astride
        i += 1
      }

    }
  }

  // this shouldn't be necessary but it is:
  @expand
  implicit def dv_hv_op[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType] = {
    DenseVector.pureFromUpdate(implicitly[Op.InPlaceImpl2[DenseVector[T], HashVector[T]]])
  }


  @noinline
  private def init() = {}
}



