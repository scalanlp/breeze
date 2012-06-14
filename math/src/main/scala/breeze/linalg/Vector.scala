package breeze.linalg

import operators._
import scala.{specialized=>spec}
import breeze.storage.Storage
import breeze.generic.CanMapValues
import breeze.math.Field

/**
 *
 * @author dlwh
 */
trait VectorLike[@spec E, +Self <: Vector[E]] extends Tensor[Int, E] with TensorLike[Int, E, Self] {
  def map[E2, That](fn: E=>E2)(implicit canMapValues: CanMapValues[Self, E, E2, That]):That = values map fn

  def foreach[U](fn: E=>U) { values foreach fn }

}


// Storage should be a self type, but specialization is broken for now.
trait Vector[@spec(Int, Double, Float) E] extends VectorLike[E, Vector[E]] with Storage[E] { //storage: Storage[E] =>

  def offset: Int
  def stride: Int
  def length: Int
  override def size = length

  final def apply(i: Int) = {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    rawApply(offset + i * stride)
  }

  final def update(i: Int, v: E) {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    rawUpdate(offset + i * stride, v)
  }

  def iterator = Iterator.range(0, size).map{i => i -> apply(i)}

  def valuesIterator = Iterator.range(0, size).map{i => apply(i)}

  def keysIterator = Iterator.range(0, size)


  /** Returns the k-norm of this Vector. */
  def norm(n : Double)(implicit field: Field[E]) : Double = {
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

object Vector {
  val canScaleD: BinaryUpdateRegistry[Vector[Double], Double, OpMulScalar] = {
    new BinaryUpdateRegistry[Vector[Double], Double, OpMulScalar] {
      def doOp(a: Vector[Double], b: Double) = {
        throw new Exception("Not yet...")
      }
    }
  }

  implicit val canDotD: BinaryRegistry[Vector[Double], Vector[Double], OpMulInner, Double] = {
    new BinaryRegistry[Vector[Double], Vector[Double], OpMulInner, Double] {
      def doOp(a: Vector[Double], b: Vector[Double]) = {
        throw new Exception("Not yet...")
      }
    }

  }

  implicit val canAddIntoD: BinaryUpdateRegistry[Vector[Double], Vector[Double], OpAdd] = {
    new BinaryUpdateRegistry[Vector[Double], Vector[Double], OpAdd] {
      def doOp(a: Vector[Double], b: Vector[Double]) {
        throw new Exception("Not yet...")
      }
    }

  }

  implicit val canSubIntoD: BinaryUpdateRegistry[Vector[Double], Vector[Double], OpSub] = {
    new BinaryUpdateRegistry[Vector[Double], Vector[Double], OpSub] {
      def doOp(a: Vector[Double], b: Vector[Double]) {
        throw new Exception("Not yet...")
      }
    }

  }
}

trait VectorConstructors[Vec[T]<:Vector[T]] {
  def zeros[V:ClassManifest](size: Int):Vec[V]
  def apply[@spec(Double, Int, Float) V](values: Array[V]):Vec[V]

  def apply[V:ClassManifest](values: V*):Vec[V] = apply(values.toArray)
  def fill[@spec(Double, Int, Float) V:ClassManifest](size: Int)(v: =>V):Vec[V] = apply(Array.fill(size)(v))
  def tabulate[@spec(Double, Int, Float) V:ClassManifest](size: Int)(f: Int=>V):Vec[V]= apply(Array.tabulate(size)(f))

}