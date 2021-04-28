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
import support._
import support.CanTraverseValues.ValuesVisitor
import breeze.generic.UFunc
import breeze.generic.UFunc.{InPlaceImpl2, UImpl, UImpl2}
import breeze.macros.expand
import breeze.math._
import breeze.stats.distributions.Rand
import breeze.storage.{Storage, Zero}
import breeze.util.ReflectionUtil

import scala.{specialized => spec}
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.ArrayBuilder
import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

/**
 * Trait for operators and such used in vectors.
 * @author dlwh
 */
trait VectorLike[@spec V, +Self <: Vector[V]] extends Tensor[Int, V] with TensorLike[Int, V, Self] {
  def map[V2, That](fn: V => V2)(implicit canMapValues: CanMapValues[Self @uncheckedVariance, V, V2, That]): That =
    values.map(fn)

  def foreach[U](fn: V => U): Unit = { values.foreach(fn) }
}

/**
 * A Vector represents the mathematical concept of a vector in math.
 * @tparam V
 */
trait Vector[@spec(Int, Double, Float) V] extends VectorLike[V, Vector[V]] {

  /**
   * @return the set of keys in this vector (0 until length)
   */
  def keySet: Set[Int] = BitSet((0 until length): _*)

  def length: Int
  override def size = length

  def iterator = Iterator.range(0, size).map { i =>
    i -> apply(i)
  }

  def valuesIterator = Iterator.range(0, size).map { i =>
    apply(i)
  }

  def keysIterator = Iterator.range(0, size)

  def copy: Vector[V]

  override def equals(p1: Any) = p1 match {
    case x: Vector[_] =>
      this.length == x.length &&
        (valuesIterator.sameElements(x.valuesIterator))
    case _ => false
  }

  override def hashCode(): Int = throw new UnsupportedOperationException("hashCode has to be overridden for Vectors")

  def toDenseVector(implicit cm: ClassTag[V]) = {
    DenseVector(toArray)
  }


  /**Returns copy of this [[breeze.linalg.Vector]] as a [[scala.Vector]]*/
  def toScalaVector: scala.Vector[V] = scala.Vector.empty ++ valuesIterator

  /**Returns copy of this [[breeze.linalg.Vector]] as a [[scala.Array]]*/
  def toArray(implicit cm: ClassTag[V]) = {
    val result = new Array[V](length)
    var i = 0
    while (i < length) {
      result(i) = apply(i)
      i += 1
    }
    result
  }

  //ToDo 2: Should this be deprecated and changed to `toScalaVector`?
  /**Returns copy of this [[breeze.linalg.Vector]] as a [[scala.Vector]]*/
  def toVector(implicit cm: ClassTag[V]) = Vector[V](toArray)

  //ToDo 2: implement fold/scan/reduce to operate along one axis of a matrix/tensor
  // <editor-fold defaultstate="collapsed" desc=" scala.collection -like padTo, fold/scan/reduce ">

  /** See [[scala.collection.mutable.ArrayOps.padTo]].
   */
  def padTo(len: Int, elem: V)(implicit cm: ClassTag[V]): Vector[V] = Vector[V](toArray.padTo(len, elem))

  def exists(f: V => Boolean) = valuesIterator.exists(f)
  override def forall(f: V => Boolean) = valuesIterator.forall(f)

  /** See [[scala.collection.mutable.ArrayOps.fold]].
   */
  def fold[E1 >: V](z: E1)(op: (E1, E1) => E1): E1 = valuesIterator.fold(z)(op)

  /** See [[scala.collection.mutable.ArrayOps.foldLeft]].
   */
  def foldLeft[B](z: B)(op: (B, V) => B): B = valuesIterator.foldLeft(z)(op)

  /** See [[scala.collection.mutable.ArrayOps.foldRight]].
   */
  def foldRight[B](z: B)(op: (V, B) => B): B = valuesIterator.foldRight(z)(op)

  /** See [[scala.collection.mutable.ArrayOps.reduce]].
   */
  def reduce[E1 >: V](op: (E1, E1) => E1): E1 = valuesIterator.reduce(op)

  /** See [[scala.collection.mutable.ArrayOps.reduceLeft]].
   */
  def reduceLeft[B >: V](op: (B, V) => B): B = {
    valuesIterator.reduceLeft(op)
  }

  /** See [[scala.collection.mutable.ArrayOps.reduceRight]].
   */
  def reduceRight[B >: V](op: (V, B) => B): B = {
    valuesIterator.reduceRight(op)
  }

  /** See [[scala.collection.mutable.ArrayOps.scan]].
   */
  def scan[E1 >: V](z: E1)(op: (E1, E1) => E1)(implicit cm: ClassTag[V], cm1: ClassTag[E1]): Vector[E1] = {
    Vector[E1](toArray.scan(z)(op))
  }

  /** See [[scala.collection.mutable.ArrayOps.scanLeft]].
   */
  def scanLeft[B](z: B)(op: (B, V) => B)(implicit cm1: ClassTag[B]): Vector[B] = {
    Vector[B](valuesIterator.scanLeft(z)(op).toArray)
  }

  /** See [[scala.collection.mutable.ArrayOps.scanRight]].
   */
  def scanRight[B](z: B)(op: (V, B) => B)(implicit cm1: ClassTag[B]): Vector[B] =
    Vector[B](toScalaVector.scanRight(z)(op).toArray)

  // </editor-fold>

}

object Vector extends VectorConstructors[Vector] {

  /**
   * Creates a Vector of size size.
   * @param size
   * @tparam V
   * @return
   */
  def zeros[V: ClassTag: Zero](size: Int): Vector[V] = DenseVector.zeros(size)

  /**
   * Creates a vector with the specified elements
   * @param values
   * @tparam V
   * @return
   */
  def apply[@spec(Double, Int, Float, Long) V](values: Array[V]): Vector[V] = DenseVector(values)

  implicit def scalarOf[T]: ScalarOf[Vector[T], T] = ScalarOf.dummy

  implicit def canCopy[E]: CanCopy[Vector[E]] = new CanCopy[Vector[E]] {
    def apply(t: Vector[E]): Vector[E] = t.copy
  }

  implicit def space[V: Field: Zero: ClassTag]: MutableFiniteCoordinateField[Vector[V], Int, V] = {
    val f = implicitly[Field[V]]
    import f.normImpl
    implicit val _dim: dim.Impl[Vector[V], Int] = dim.implVDim[V, Vector[V]]
    MutableFiniteCoordinateField.make[Vector[V], Int, V]
  }
}



/**
 * Trait that can mixed to companion objects to enable utility methods for creating vectors.
 * @tparam Vec
 */
trait VectorConstructors[Vec[T] <: Vector[T]] {

  /**
   * Creates a Vector of size size.
   * @param size
   * @tparam V
   * @return
   */
  def zeros[V: ClassTag: Zero](size: Int): Vec[V]

  /**
   * Creates a vector with the specified elements
   * @param values
   * @tparam V
   * @return
   */
  def apply[@spec(Double, Int, Float, Long) V](values: Array[V]): Vec[V]

  /**
   * Creates a vector with the specified elements
   * @param values
   * @tparam V
   * @return
   */
  def apply[V: ClassTag](values: V*): Vec[V] = {
    // manual specialization so that we create the right DenseVector specialization... @specialized doesn't work here
    val man = implicitly[ClassTag[V]]
    if (man == ClassTag[Double]) apply(values.toArray.asInstanceOf[Array[Double]]).asInstanceOf[Vec[V]]
    else if (man == ClassTag[Float]) apply(values.toArray.asInstanceOf[Array[Float]]).asInstanceOf[Vec[V]]
    else if (man == ClassTag[Int]) apply(values.toArray.asInstanceOf[Array[Int]]).asInstanceOf[Vec[V]]
    else apply(values.toArray)
//     apply(values.toArray)
  }

  //ToDo 2: I'm not sure fill/tabulate are really useful outside of the context of a DenseVector?
  implicit def canCreateZeros[V: ClassTag: Zero]: CanCreateZeros[Vec[V], Int] =
    new CanCreateZeros[Vec[V], Int] {
      def apply(d: Int): Vec[V] = {
        zeros[V](d)
      }
    }

  /**
   * Creates a Vector of uniform random numbers in (0,1)
   * @param size
   * @param rand
   * @return
   */
  def rand[T: ClassTag](size: Int, rand: Rand[T] = Rand.uniform): Vec[T] = {
    // Array#fill is slow.
    val arr = new Array[T](size)
    var i = 0
    while (i < arr.length) {
      arr(i) = rand.draw()
      i += 1
    }

    apply(arr)
  }

  def range(start: Int, end: Int): Vec[Int] = range(start, end, 1)
  def range(start: Int, end: Int, step: Int): Vec[Int] = apply[Int](Array.range(start, end, step))

  def rangeF(start: Float, end: Float, step: Float = 1.0f): Vec[Float] = {
    import breeze.macros.cforRange
    require(end > start)
    require(end - start > step)
    var size: Int = math.ceil((end - start) / step).toInt
    // #751: floating point shenanigans
    if (size > 0 && start + step * (size - 1) >= end) {
      size -= 1
    }
    val data = new Array[Float](size)
    cforRange(0 until size) { i =>
      data(i) = start + i * step
    }
    apply(data)
  }

  def rangeD(start: Double, end: Double, step: Double = 1.0): Vec[Double] = {
    import breeze.macros.cforRange
    require(end > start)
    require(end - start > step)
    var size: Int = math.ceil((end - start) / step).toInt
    // #751: floating point shenanigans
    if (size > 0 && start + step * (size - 1) >= end) {
      size -= 1
    }
    val data = new Array[Double](size)
    cforRange(0 until size) { i =>
      data(i) = start + i * step
    }
    apply(data)
  }
}

trait StorageVector[V] extends Vector[V] with Storage[V]
