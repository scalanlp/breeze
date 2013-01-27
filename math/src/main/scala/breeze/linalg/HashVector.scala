package breeze.linalg

import breeze.collection.mutable.OpenAddressHashArray
import operators.{OpNeg, UnaryOp, OpMulScalar, BinaryOp}
import breeze.storage.{ConfigurableDefault, DefaultArrayValue}
import breeze.generic.{CanMapValues, URFunc}
import support.{CanZipMapValues, CanMapKeyValuePairs, CanCopy}
import breeze.math.{TensorSpace, Ring}
import util.MurmurHash

/**
 * A HashVector is a sparse vector backed by an OpenAddressHashArray
 * @author dlwh
 */
class HashVector[@specialized(Int, Double, Float) E](val array: OpenAddressHashArray[E]) extends Vector[E] with VectorLike[E, HashVector[E]] {
  def activeIterator: Iterator[(Int, E)] = array.activeIterator

  def activeValuesIterator: Iterator[E] = array.activeValuesIterator

  def activeKeysIterator: Iterator[Int] = array.activeKeysIterator

  def apply(i: Int): E = array(i)

  def update(i: Int, v: E) {
    array(i) = v
  }

  def activeSize: Int = array.activeSize

  def length: Int = array.length

  def copy: HashVector[E] = new HashVector(array.copy)

  def repr = this

  override def ureduce[A](f: URFunc[E, A]): A = {
    f.apply(array.data, 0, 1, length, array.isActive _)
  }

  final def iterableSize: Int = array.iterableSize
  def data = array.data
  final def index = array.index
  final def isActive(i: Int) = array.isActive(i)


  override def toString = {
    activeIterator.mkString("HashVector(",", ", ")")
  }

  override def hashCode() = {
    val hash = new MurmurHash[E](47)
    // we make the hash code based on index * value, so that zeros don't affect the hashcode.
    val dv = array.default.value(array.defaultArrayValue)
    var i = 0
    while(i < activeSize) {
      if(isActive(i)) {
        val ind = index(i)
        val v = data(i)
        if(v != dv) {
          hash.apply(v)
          hash.append(ind)
        }
      }

      i += 1
    }

    hash.hash

  }
}


object HashVector extends HashVectorOps_Int with HashVectorOps_Float with HashVectorOps_Double {
  def zeros[@specialized(Double, Float, Int) V: ClassManifest:DefaultArrayValue](size: Int) = {
    new HashVector(new OpenAddressHashArray[V](size))
  }
  def apply[@specialized(Double, Float, Int) V:DefaultArrayValue](values: Array[V]) = {
    implicit val man = ClassManifest.fromClass[V](values.getClass.getComponentType.asInstanceOf[Class[V]])
    val oah = new OpenAddressHashArray[V](values.length)
    for( (v,i) <- values.zipWithIndex) oah(i) = v
    new HashVector(oah)
  }

  def apply[V:ClassManifest:DefaultArrayValue](values: V*):HashVector[V] = {
    apply(values.toArray)
  }
  def fill[@specialized(Double, Int, Float) V:ClassManifest:DefaultArrayValue](size: Int)(v: =>V):HashVector[V] = apply(Array.fill(size)(v))
  def tabulate[@specialized(Double, Int, Float) V:ClassManifest:DefaultArrayValue](size: Int)(f: Int=>V):HashVector[V]= apply(Array.tabulate(size)(f))

  def apply[V:ClassManifest:DefaultArrayValue](length: Int)(values: (Int, V)*) = {
    val r = zeros[V](length)
    for( (i, v) <- values) {
      r(i) = v
    }
    r
  }

  // implicits



  // implicits
  class CanCopyHashVector[@specialized(Int, Float, Double) V:ClassManifest:DefaultArrayValue] extends CanCopy[HashVector[V]] {
    def apply(v1: HashVector[V]) = {
      v1.copy
    }
  }

  implicit def canCopyHash[@specialized(Int, Float, Double) V: ClassManifest: DefaultArrayValue] = new CanCopyHashVector[V]

  implicit def canMapValues[V, V2: ClassManifest: DefaultArrayValue]:CanMapValues[HashVector[V], V, V2, HashVector[V2]] = {
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
            out(i) = fn(from.data(i))
          i += 1
        }
        new HashVector(out)
      }
    }
  }

  implicit def canMapPairs[V, V2: ClassManifest: DefaultArrayValue]:CanMapKeyValuePairs[HashVector[V], Int, V, V2, HashVector[V2]] = {
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

  class CanZipMapValuesHashVector[@specialized(Int, Double, Float) V, @specialized(Int, Double) RV:ClassManifest:DefaultArrayValue] extends CanZipMapValues[HashVector[V],V,RV,HashVector[RV]] {
    def create(length : Int) = zeros(length)

    /**Maps all corresponding values from the two collection. */
    def map(from: HashVector[V], from2: HashVector[V], fn: (V, V) => RV) = {
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
  implicit def zipMap[V, R:ClassManifest:DefaultArrayValue] = new CanZipMapValuesHashVector[V, R]
  implicit val zipMap_d = new CanZipMapValuesHashVector[Double, Double]
  implicit val zipMap_f = new CanZipMapValuesHashVector[Float, Float]
  implicit val zipMap_i = new CanZipMapValuesHashVector[Int, Int]


  implicit def negFromScale[@specialized(Int, Float, Double)  V, Double](implicit scale: BinaryOp[HashVector[V], V, OpMulScalar, HashVector[V]], field: Ring[V]) = {
    new UnaryOp[HashVector[V], OpNeg, HashVector[V]] {
      override def apply(a : HashVector[V]) = {
        scale(a, field.negate(field.one))
      }
    }
  }


  implicit val space_d = TensorSpace.make[HashVector[Double], Int, Double]
  implicit val space_f = TensorSpace.make[HashVector[Float], Int, Float]
  implicit val space_i = TensorSpace.make[HashVector[Int], Int, Int]
}