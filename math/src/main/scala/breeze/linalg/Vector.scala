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
import breeze.generic.CanMapValues
import breeze.math.{TensorSpace, Ring, Field}
import collection.immutable.BitSet
import support.{CanZipMapValues, CanCopy}
import util.Random
import breeze.storage.Storage

/**
 *
 * @author dlwh
 */
trait VectorLike[@spec E, +Self <: Vector[E]] extends Tensor[Int, E] with TensorLike[Int, E, Self] {
  def map[E2, That](fn: E=>E2)(implicit canMapValues: CanMapValues[Self, E, E2, That]):That = values map fn

  def foreach[U](fn: E=>U) { values foreach fn }

  def copy: Self

}


trait Vector[@spec(Int, Double, Float) E] extends VectorLike[E, Vector[E]]{

  def keySet: Set[Int] = BitSet( (0 until length) :_*)

  def length: Int
  override def size = length

  def iterator = Iterator.range(0, size).map{i => i -> apply(i)}

  def valuesIterator = Iterator.range(0, size).map{i => apply(i)}

  def keysIterator = Iterator.range(0, size)


  override def equals(p1: Any) = p1 match {
    case x: Vector[_] =>
        this.length == x.length &&
          (valuesIterator sameElements x.valuesIterator)
    case _ => false
  }



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

  def toDenseVector(implicit cm: ClassManifest[E]) = {
    new DenseVector(toArray)
  }

  def toArray(implicit cm: ClassManifest[E]) = {
    val result = new Array[E](length)
    var i = 0
    while(i < length) {
      result(i) = apply(i)
      i += 1
    }
    result
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

  implicit def negFromScale[@specialized(Int, Float, Double) V, Double](implicit scale: BinaryOp[Vector[V], V, OpMulScalar, Vector[V]], field: Ring[V]) = {
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

}


trait VectorConstructors[Vec[T]<:Vector[T]] {
  def zeros[V:ClassManifest](size: Int):Vec[V]
  def apply[@spec(Double, Int, Float) V](values: Array[V]):Vec[V]

  def apply[V:ClassManifest](values: V*):Vec[V] = {
    // manual specialization so that we create the right DenseVector specialization... @specialized doesn't work here
    val man = implicitly[ClassManifest[V]]
    if(man == manifest[Double]) apply(values.toArray.asInstanceOf[Array[Double]]).asInstanceOf[Vec[V]]
    else if (man == manifest[Float]) apply(values.toArray.asInstanceOf[Array[Float]]).asInstanceOf[Vec[V]]
    else if (man == manifest[Int]) apply(values.toArray.asInstanceOf[Array[Int]]).asInstanceOf[Vec[V]]
    else apply(values.toArray)
//     apply(values.toArray)
  }
  def fill[@spec(Double, Int, Float) V:ClassManifest](size: Int)(v: =>V):Vec[V] = apply(Array.fill(size)(v))
  def tabulate[@spec(Double, Int, Float) V:ClassManifest](size: Int)(f: Int=>V):Vec[V]= apply(Array.tabulate(size)(f))

  def rand(size: Int, rand: Random = new Random()) = {
    // Array#fill is slow.
    val arr = new Array[Double](size)
    var i = 0
    while(i < arr.length) {
      arr(i) = rand.nextDouble()
      i += 1
    }

    apply(arr)

  }



}

trait StorageVector[E] extends Vector[E] with Storage[E]