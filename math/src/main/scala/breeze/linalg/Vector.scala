package breeze.linalg

import operators._
import scala.{specialized=>spec}
import breeze.storage.Storage
import breeze.generic.CanMapValues
import breeze.math.{TensorSpace, Ring, Field}
import collection.immutable.BitSet
import support.{CanZipMapValues, CanCopy}

/**
 *
 * @author dlwh
 */
trait VectorLike[@spec E, +Self <: Vector[E]] extends Tensor[Int, E] with TensorLike[Int, E, Self] {
  def map[E2, That](fn: E=>E2)(implicit canMapValues: CanMapValues[Self, E, E2, That]):That = values map fn

  def foreach[U](fn: E=>U) { values foreach fn }

  def copy: Self

}


// Storage should be a self type, but specialization is broken for now.
trait Vector[@spec(Int, Double, Float) E] extends VectorLike[E, Vector[E]] with Storage[E] { //storage: Storage[E] =>

  def keySet: Set[Int] = BitSet( (0 until length) :_*)

  def length: Int
  override def size = length


  def iterator = Iterator.range(0, size).map{i => i -> apply(i)}

  def valuesIterator = Iterator.range(0, size).map{i => apply(i)}

  def keysIterator = Iterator.range(0, size)


  /** Returns the k-norm of this Vector. */
  def norm(n : Double)(implicit field: Ring[E]) : Double = {
    if (n == 1) {
      var sum = 0.0
      activeValuesIterator foreach (v => sum += field.norm(v))
      sum
    } else if (n == 2) {
      var sum = 0.0
      activeValuesIterator foreach (v => { val nn = field.norm(v); sum += nn * nn })
      math.sqrt(sum)
    } else if (n == Double.PositiveInfinity) {
      var max = Double.NegativeInfinity
      activeValuesIterator foreach (v => { val nn = field.norm(v); if (nn > max) max = nn })
      max
    } else {
      var sum = 0.0
      activeValuesIterator foreach (v => { val nn = field.norm(v); sum += math.pow(nn,n) })
      math.pow(sum, 1.0 / n)
    }
  }

}

object Vector extends VectorOps_Int with VectorOps_Double with VectorOps_Float {

  implicit def canCopy[E]:CanCopy[Vector[E]] = new CanCopy[Vector[E]] {
    // Should not inherit from T=>T because those get  used by the compiler.
    def apply(t: Vector[E]): Vector[E] = t.copy
  }

  // There's a bizarre error specializing float's here.
  class CanZipMapValuesVector[@specialized(Int, Double) V, @specialized(Int, Double) RV:ClassManifest] extends CanZipMapValues[Vector[V],V,RV,Vector[RV]] {
    def create(length : Int) = new DenseVector(new Array[RV](length))

    /**Maps all corresponding values from the two collection. */
    def map(from: Vector[V], from2: Vector[V], fn: (V, V) => RV) = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result = create(from.length)
      var i = 0
      while (i < from.length) {
        result.data(i) = fn(from(i), from2(i))
        i += 1
      }
      result
    }
  }

  implicit def canMapValues[V, V2](implicit man: ClassManifest[V2]):CanMapValues[Vector[V], V, V2, Vector[V2]] = {
    new CanMapValues[Vector[V], V, V2, Vector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def map(from: Vector[V], fn: (V) => V2) = {
        DenseVector.tabulate(from.length)(i => fn(from(i)))
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: Vector[V], fn: (V) => V2) = {
        map(from, fn)
      }
    }
  }

  implicit def negFromScale[@specialized V, Double](implicit scale: BinaryOp[Vector[V], V, OpMulScalar, Vector[V]], field: Ring[V]) = {
    new UnaryOp[Vector[V], OpNeg, Vector[V]] {
      override def apply(a : Vector[V]) = {
        scale(a, field.negate(field.one))
      }
    }
  }


  implicit def zipMap[V, R:ClassManifest] = new CanZipMapValuesVector[V, R]
  implicit val zipMap_d = new CanZipMapValuesVector[Double, Double]
  implicit val zipMap_f = new CanZipMapValuesVector[Float, Float]
  implicit val zipMap_i = new CanZipMapValuesVector[Int, Int]


  implicit val space_d = TensorSpace.make[Vector[Double], Int, Double]
  implicit val space_f = TensorSpace.make[Vector[Float], Int, Float]
  implicit val space_i = TensorSpace.make[Vector[Int], Int, Int]

  /*
  implicit val canScaleD: BinaryUpdateRegistry[Vector[Double], Double, OpMulScalar] = {
    new BinaryUpdateRegistry[Vector[Double], Double, OpMulScalar] {
    }
  }

  implicit val canDotD: BinaryRegistry[Vector[Double], Vector[Double], OpMulInner, Double] = {
    new BinaryRegistry[Vector[Double], Vector[Double], OpMulInner, Double] {
    }

  }

  implicit val canAddIntoD: BinaryUpdateRegistry[Vector[Double], Vector[Double], OpAdd] = {
    new BinaryUpdateRegistry[Vector[Double], Vector[Double], OpAdd] {
    }

  }

  implicit val canSubIntoD: BinaryUpdateRegistry[Vector[Double], Vector[Double], OpSub] = {
    new BinaryUpdateRegistry[Vector[Double], Vector[Double], OpSub] {
    }
  }
  */
}

trait VectorConstructors[Vec[T]<:Vector[T]] {
  def zeros[V:ClassManifest](size: Int):Vec[V]
  def apply[@spec(Double, Int, Float) V](values: Array[V]):Vec[V]

  def apply[V:ClassManifest](values: V*):Vec[V] = apply(values.toArray)
  def fill[@spec(Double, Int, Float) V:ClassManifest](size: Int)(v: =>V):Vec[V] = apply(Array.fill(size)(v))
  def tabulate[@spec(Double, Int, Float) V:ClassManifest](size: Int)(f: Int=>V):Vec[V]= apply(Array.tabulate(size)(f))



}