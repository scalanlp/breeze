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
import breeze.generic.{UFunc}
import breeze.generic.UFunc.{UImpl2, UImpl, InPlaceImpl2}
import breeze.macros.expand
import breeze.math._
import breeze.stats.distributions.Rand
import breeze.storage.{Zero, Storage}

import scala.util.hashing.MurmurHash3
import scala.{specialized=>spec}
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.ArrayBuilder
import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

/**
 * Trait for operators and such used in vectors.
 * @author dlwh
 */
trait VectorLike[@spec V, +Self <: Vector[V]] extends Tensor[Int, V] with TensorLike[Int, V, Self] {
  def map[V2, That](fn: V=>V2)(implicit canMapValues: CanMapValues[Self  @uncheckedVariance, V, V2, That]):That = values map fn

  def foreach[U](fn: V=>U): Unit = { values foreach fn }
}


/**
 * A Vector represents the mathematical concept of a vector in math.
 * @tparam V
 */
trait Vector[@spec(Int, Double, Float) V] extends VectorLike[V, Vector[V]]{

  /**
   * @return the set of keys in this vector (0 until length)
   */
  def keySet: Set[Int] = BitSet( (0 until length) :_*)

  def length: Int
  override def size = length

  def iterator = Iterator.range(0, size).map{i => i -> apply(i)}

  def valuesIterator = Iterator.range(0, size).map{i => apply(i)}

  def keysIterator = Iterator.range(0, size)

  def copy: Vector[V]

  override def equals(p1: Any) = p1 match {
    case x: Vector[_] =>
        this.length == x.length &&
          (valuesIterator sameElements x.valuesIterator)
    case _ => false
  }



  def toDenseVector(implicit cm: ClassTag[V]) = {
    DenseVector(toArray)
  }

  /**Returns copy of this [[breeze.linalg.Vector]] as a [[scala.Array]]*/
  def toArray(implicit cm: ClassTag[V]) = {
    val result = new Array[V](length)
    var i = 0
    while(i < length) {
      result(i) = apply(i)
      i += 1
    }
    result
  }

  //ToDo 2: Should this be deprecated and changed to `toScalaVector`?
  /**Returns copy of this [[breeze.linalg.Vector]] as a [[scala.Vector]]*/
  def toVector(implicit cm: ClassTag[V]) = Vector[V]( toArray )

  //ToDo 2: implement fold/scan/reduce to operate along one axis of a matrix/tensor
  // <editor-fold defaultstate="collapsed" desc=" scala.collection -like padTo, fold/scan/reduce ">

  /** See [[scala.collection.mutable.ArrayOps.padTo]].
    */
  def padTo(len: Int, elem: V)(implicit cm: ClassTag[V]): Vector[V] = Vector[V]( toArray.padTo(len, elem) )

  def exists(f: V=>Boolean) = valuesIterator.exists(f)
  override def forall(f: V=>Boolean) = valuesIterator.forall(f)

  /** See [[scala.collection.mutable.ArrayOps.fold]].
    */
  def fold[E1 >: V](z: E1)(op: (E1, E1) => E1 ): E1 = valuesIterator.fold(z)( op )
  /** See [[scala.collection.mutable.ArrayOps.foldLeft]].
    */
  def foldLeft[B >: V](z: B)(op: (B, V) => B ): B = valuesIterator.foldLeft(z)( op )

  /** See [[scala.collection.mutable.ArrayOps.foldRight]].
    */
  def foldRight[B >: V](z: B)(op: (V, B) => B ): B = valuesIterator.foldRight(z)( op )

  /** See [[scala.collection.mutable.ArrayOps.reduce]].
    */
  def reduce[E1 >: V](op: (E1, E1) => E1 ): E1 = valuesIterator.reduce( op )
  /** See [[scala.collection.mutable.ArrayOps.reduceLeft]].
    */
  def reduceLeft[B >: V](op: (B, V) => B ): B = {
    valuesIterator.reduceLeft( op )
  }
  /** See [[scala.collection.mutable.ArrayOps.reduceRight]].
    */
  def reduceRight[B >: V](op: (V, B) => B ): B = {
    valuesIterator.reduceRight( op )
  }

  /** See [[scala.collection.mutable.ArrayOps.scan]].
    */
  def scan[E1 >: V](z: E1)(op: (E1, E1) => E1 )(implicit cm: ClassTag[V], cm1: ClassTag[E1]): Vector[E1] = {
    Vector[E1]( toArray.scan(z)( op ))
  }

  /** See [[scala.collection.mutable.ArrayOps.scanLeft]].
    */
  def scanLeft[B >: V](z: B)(op: (B, V) => B )(implicit cm1: ClassTag[B]): Vector[B] = {
    Vector[B]( valuesIterator.scanLeft(z)( op ).toArray )
  }

  /** See [[scala.collection.mutable.ArrayOps.scanRight]].
    */
  def scanRight[B >: V](z: B)(op: (V, B) => B )(implicit cm1: ClassTag[B]): Vector[B] = Vector[B]( valuesIterator.scanRight(z)( op ).toArray )

  // </editor-fold>

}

object Vector extends VectorConstructors[Vector] with VectorOps {


  /**
   * Creates a Vector of size size.
   * @param size
   * @tparam V
   * @return
   */
  def zeros[V: ClassTag : Zero](size: Int): Vector[V] = DenseVector.zeros(size)


  /**
   * Creates a vector with the specified elements
   * @param values
   * @tparam V
   * @return
   */
  def apply[@spec(Double, Int, Float, Long) V](values: Array[V]): Vector[V] = DenseVector(values)

  implicit def canCopy[E]:CanCopy[Vector[E]] = new CanCopy[Vector[E]] {
    // Should not inherit from T=>T because those get  used by the compiler.
    def apply(t: Vector[E]): Vector[E] = t.copy
  }

  // There's a bizarre error specializing float's here.
  class CanZipMapValuesVector[@spec(Int, Double) V, @spec(Int, Double) RV:ClassTag] extends CanZipMapValues[Vector[V],V,RV,Vector[RV]] {
    def create(length : Int) = DenseVector(new Array[RV](length))

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

  implicit def canMapValues[V, V2](implicit man: ClassTag[V2]):CanMapValues[Vector[V], V, V2, Vector[V2]] = {
    new CanMapValues[Vector[V], V, V2, Vector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def apply(from: Vector[V], fn: (V) => V2) = {
        DenseVector.tabulate(from.length)(i => fn(from(i)))
      }
    }
  }

  implicit def canMapActiveValues[V, V2](implicit man: ClassTag[V2]):CanMapActiveValues[Vector[V], V, V2, Vector[V2]] = {
    new CanMapActiveValues[Vector[V], V, V2, Vector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def apply(from: Vector[V], fn: (V) => V2) = {
        DenseVector.tabulate(from.length)(i => fn(from(i)))
      }
    }
  }

  implicit def scalarOf[T]: ScalarOf[Vector[T], T] = ScalarOf.dummy

  implicit def negFromScale[@spec(Double, Int, Float, Long) V, Double](implicit scale: OpMulScalar.Impl2[Vector[V], V, Vector[V]], ring: Ring[V]) = {
    new OpNeg.Impl[Vector[V], Vector[V]] {
      override def apply(a : Vector[V]) = {
        scale(a, ring.negate(ring.one))
      }
    }
  }


  implicit def zipMap[V, R:ClassTag] = new CanZipMapValuesVector[V, R]
  implicit val zipMap_d = new CanZipMapValuesVector[Double, Double]
  implicit val zipMap_f = new CanZipMapValuesVector[Float, Float]
  implicit val zipMap_i = new CanZipMapValuesVector[Int, Int]

  class CanZipMapKeyValuesVector[@spec(Double, Int, Float, Long) V, @spec(Int, Double) RV:ClassTag] extends CanZipMapKeyValues[Vector[V],Int, V,RV,Vector[RV]] {
    def create(length : Int) = DenseVector(new Array[RV](length))

    /**Maps all corresponding values from the two collection. */
    def map(from: Vector[V], from2: Vector[V], fn: (Int, V, V) => RV): Vector[RV] = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result = create(from.length)
      var i = 0
      while (i < from.length) {
        result.data(i) = fn(i, from(i), from2(i))
        i += 1
      }
      result
    }

    override def mapActive(from: Vector[V], from2: Vector[V], fn: (Int, V, V) => RV): Vector[RV] = {
      map(from, from2, fn)
    }
  }


  implicit def zipMapKV[V, R:ClassTag]: CanZipMapKeyValuesVector[V, R] = new CanZipMapKeyValuesVector[V, R]


  /**Returns the k-norm of this Vector. */
  implicit def canNorm[T](implicit canNormS: norm.Impl[T, Double]): norm.Impl2[Vector[T], Double, Double] = {

    new norm.Impl2[Vector[T], Double, Double] {
      def apply(v: Vector[T], n: Double): Double = {
        import v._
        if (n == 1) {
          var sum = 0.0
          activeValuesIterator foreach (v => sum += canNormS(v) )
          sum
        } else if (n == 2) {
          var sum = 0.0
          activeValuesIterator foreach (v => { val nn = canNormS(v); sum += nn * nn })
          math.sqrt(sum)
        } else if (n == Double.PositiveInfinity) {
          var max = 0.0
          activeValuesIterator foreach (v => { val nn = canNormS(v); if (nn > max) max = nn })
          max
        } else {
          var sum = 0.0
          activeValuesIterator foreach (v => { val nn = canNormS(v); sum += math.pow(nn,n) })
          math.pow(sum, 1.0 / n)
        }
      }
    }
  }

  implicit def canIterateValues[V]: CanTraverseValues[Vector[V], V] = new CanTraverseValues[Vector[V], V] {

    def isTraversableAgain(from: Vector[V]): Boolean = true

    def traverse(from: Vector[V], fn: ValuesVisitor[V]): Unit = {
      for( v <- from.valuesIterator) {
        fn.visit(v)
      }
    }

  }

  implicit def canTraverseKeyValuePairs[V]: CanTraverseKeyValuePairs[Vector[V], Int, V] =

    new CanTraverseKeyValuePairs[Vector[V], Int, V] {
      def isTraversableAgain(from: Vector[V]): Boolean = true

      def traverse(from: Vector[V], fn: CanTraverseKeyValuePairs.KeyValuePairsVisitor[Int, V]): Unit = {
        for(i <- 0 until from.length)
          fn.visit(i, from(i))
      }

    }



  implicit def space[V:Field:Zero:ClassTag]: MutableFiniteCoordinateField[Vector[V], Int, V] = {
    val f = implicitly[Field[V]]
    import f.normImpl
    implicit val _dim = dim.implVDim[V, Vector[V]]
    MutableFiniteCoordinateField.make[Vector[V], Int, V]
  }
}



trait VectorOps { this: Vector.type =>
  import breeze.math.PowImplicits._


  @expand.valify
  @expand
  implicit def v_v_Idempotent_Op[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _})
  op: Op.Impl2[T, T, T]):BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]] = new BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]] {
    override def bindingMissing(a: Vector[T], b: Vector[T]): Vector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val result = a.copy
      for((k,v) <- b.activeIterator) {
        result(k) = op(a(k), v)
      }
      result
    }
  }

  implicit def v_v_Idempotent_OpSub[T:Ring]:OpSub.Impl2[Vector[T], Vector[T], Vector[T]] =
    new OpSub.Impl2[Vector[T], Vector[T], Vector[T]] {
    val r = implicitly[Ring[T]]
    def apply(a: Vector[T], b: Vector[T]): Vector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val result = a.copy
      for((k,v) <- b.activeIterator) {
        result(k) = r.-(a(k), v)
      }
      result
    }
  }

  implicit def v_v_Idempotent_OpAdd[T:Semiring]:OpAdd.Impl2[Vector[T], Vector[T], Vector[T]] =
    new OpAdd.Impl2[Vector[T], Vector[T], Vector[T]] {
    val r = implicitly[Semiring[T]]
    def apply(a: Vector[T], b: Vector[T]): Vector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val result = a.copy
      for((k,v) <- b.activeIterator) {
        result(k) = r.+(a(k), v)
      }
      result
    }
  }

  @expand
  @expand.valify
  implicit def v_v_nilpotent_Op[@expand.args(Int, Double, Float, Long) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l) zero: T):BinaryRegistry[Vector[T], Vector[T], OpMulScalar.type, Vector[T]] = new BinaryRegistry[Vector[T], Vector[T], OpMulScalar.type, Vector[T]] {
    override def bindingMissing(a: Vector[T], b: Vector[T]): Vector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val builder = new VectorBuilder[T](a.length)
      for((k,v) <- b.activeIterator) {
        val r = a(k) * v
        if(r != zero)
          builder.add(k, r)
      }
      builder.toVector
    }
  }





  @expand
  @expand.valify
  implicit def v_v_Op[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]] = new BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]] {
    override def bindingMissing(a: Vector[T], b: Vector[T]): Vector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val result = Vector.zeros[T](a.length)
      var i = 0
      while(i < a.length) {
        result(i) = op(a(i), b(i))
        i += 1
      }
      result
    }
  }



  /*
  @expand

  implicit def cast_v_v_Op[V1, V2,
  @expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub, OpMulScalar,OpDiv, OpSet, OpMod, OpPow) Op <: OpType](implicit v1: V1<:<Vector[T], v2: V2<:<Vector[T]) = {
    implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].asInstanceOf[Op.Impl2[V1, V2, Vector[T]]]
  }
  */

  @expand
  @expand.valify
  implicit def v_s_Op[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T],
  @expand.sequence[T](0, 0.0, 0.0f, 0l)
  zero: T):BinaryRegistry[Vector[T], T, Op.type, Vector[T]] = new BinaryRegistry[Vector[T], T, Op.type, Vector[T]] {
    override def bindingMissing(a: Vector[T], b: T): Vector[T] = {
      val result = Vector.zeros[T](a.length)

      var i = 0
      while(i < a.length) {
        result(i) = op(a(i), b)
        i += 1
      }
      result
    }
  }

  @expand
  @expand.valify
  implicit def s_v_Op[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T],
   @expand.sequence[T](0, 0.0, 0.0f, 0l)
   zero: T):BinaryRegistry[T, Vector[T], Op.type, Vector[T]] = new BinaryRegistry[T, Vector[T], Op.type, Vector[T]] {
    override def bindingMissing(b: T, a: Vector[T]): Vector[T] = {
      val result = Vector.zeros[T](a.length)

      var i = 0
      while(i < a.length) {
        result(i) = op(b, a(i))
        i += 1
      }
      result
    }
  }

  @expand
  implicit def v_sField_Op[@expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpMod, OpPow) Op <: OpType, T:Field:ClassTag]
  (implicit @expand.sequence[Op]({f.+(_,_)},  {f.-(_,_)}, {f.*(_,_)}, {f.*(_,_)}, {f./(_,_)}, {f.%(_,_)}, {f.pow(_,_)}) op: Op.Impl2[T, T, T]):
  BinaryRegistry[Vector[T], T, Op.type, Vector[T]] = new BinaryRegistry[Vector[T], T, Op.type, Vector[T]] {
    val f = implicitly[Field[T]]
    override def bindingMissing(a: Vector[T], b: T): Vector[T] = {
      val result = Vector.zeros[T](a.length)

      var i = 0
      while(i < a.length) {
        result(i) = op(a(i), b)
        i += 1
      }
      result
    }
  }



  @expand
  @expand.valify
  implicit def v_v_UpdateOp[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):BinaryUpdateRegistry[Vector[T], Vector[T], Op.type] = new BinaryUpdateRegistry[Vector[T], Vector[T], Op.type] {
    override def bindingMissing(a: Vector[T], b: Vector[T]):Unit = {
      require(b.length == a.length, "Vectors must be the same length!")
      var i = 0
      while(i < a.length) {
        a(i) = op(a(i), b(i))
        i += 1
      }
    }
  }

  @expand
  @expand.valify
  implicit def v_v_Idempotent_UpdateOp[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _})
  op: Op.Impl2[T, T, T]):BinaryUpdateRegistry[Vector[T], Vector[T], Op.type] = new BinaryUpdateRegistry[Vector[T], Vector[T], Op.type] {
    override def bindingMissing(a: Vector[T], b: Vector[T]):Unit = {
      require(b.length == a.length, "Vectors must be the same length!")
      for( (k,v) <- b.activeIterator) {
        a(k) = op(a(k), v)
      }
    }
  }

  implicit def castUpdateOps[V1, V2, T, Op](implicit v1ev: V1<:<Vector[T],
                                            V2ev: V2<:<Vector[T],
                                            op: UFunc.InPlaceImpl2[Op, Vector[T], Vector[T]]): InPlaceImpl2[Op, V1, V2] = {
    op.asInstanceOf[UFunc.InPlaceImpl2[Op, V1, V2]]
  }

  implicit def castOps[V1, V2, T, Op, VR](implicit v1ev: V1<:<Vector[T],
                                          V2ev: V2<:<Vector[T],
                                          op: UImpl2[Op, Vector[T], Vector[T], VR]): UImpl2[Op, V1, V2, VR] = {
    op.asInstanceOf[UFunc.UImpl2[Op, V1, V2, VR]]
  }

//  implicit def castScalarOps[V1, T, Op, VR](implicit v1ev: V1<:<Vector[T],
//                                            op: UImpl2[Op, Vector[T], T, VR]): UImpl2[Op, V1, T, VR] = {
//    op.asInstanceOf[UFunc.UImpl2[Op, V1, T, VR]]
//  }
//
//  implicit def castScalarLhsOps[V1, T, Op, VR](implicit v1ev: V1<:<Vector[T],
//                                               op: UImpl2[Op, T, Vector[T], VR]): UImpl2[Op, T, V1, VR] = {
//    op.asInstanceOf[UFunc.UImpl2[Op, T, V1, VR]]
//  }

  import shapeless._

  implicit def castFunc[V1, T, Op, VR](implicit v1ev: V1<:<Vector[T], v1ne: V1 =:!= Vector[T],
                                       op: UImpl[Op, Vector[T], VR]): UImpl[Op, V1, VR] = {
    op.asInstanceOf[UFunc.UImpl[Op, V1, VR]]
  }

  @expand
  @expand.valify
  implicit def v_s_UpdateOp[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T]):BinaryUpdateRegistry[Vector[T], T, Op.type] = new BinaryUpdateRegistry[Vector[T], T, Op.type] {
    override def bindingMissing(a: Vector[T], b: T):Unit = {
      var i = 0
      while(i < a.length) {
        a(i) = op(a(i), b)
        i += 1
      }
    }
  }

  @expand
  implicit def v_s_UpdateOp[@expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType, T:Field:ClassTag]
  (implicit @expand.sequence[Op]({f.+(_,_)},  {f.-(_, _)}, {f.*(_, _)}, {f.*(_, _)}, {f./(_, _)}, {(a,b) => b}, {f.%(_,_)}, {f.pow(_,_)})
  op: Op.Impl2[T, T, T]):BinaryUpdateRegistry[Vector[T], T, Op.type] = new BinaryUpdateRegistry[Vector[T], T, Op.type] {
    val f = implicitly[Field[T]]
    override def bindingMissing(a: Vector[T], b: T):Unit = {
      var i = 0
      while(i < a.length) {
        a(i) = op(a(i), b)
        i += 1
      }
    }
  }


  @expand
  @expand.valify
  implicit def canDot_V_V[@expand.args(Int, Long, Float, Double) T](implicit @expand.sequence[T](0, 0l, 0.0f, 0.0) zero: T): BinaryRegistry[Vector[T], Vector[T], breeze.linalg.operators.OpMulInner.type, T] = {
    new BinaryRegistry[Vector[T], Vector[T], breeze.linalg.operators.OpMulInner.type, T] {
      override def bindingMissing(a: Vector[T], b: Vector[T]):T = {
        require(b.length == a.length, "Vectors must be the same length!")
        if (a.activeSize > b.activeSize) {
          bindingMissing(b, a)
        } else {
          var result : T = zero
          for( (k,v) <- a.activeIterator) {
            result += v * b(k)
          }
          result
        }
      }
    }
  }

  implicit def canDot_V_V[T:ClassTag:Semiring]: BinaryRegistry[Vector[T], Vector[T], breeze.linalg.operators.OpMulInner.type, T] = {
    new BinaryRegistry[Vector[T], Vector[T], breeze.linalg.operators.OpMulInner.type, T] {
      val s = implicitly[Semiring[T]]
      override def bindingMissing(a: Vector[T], b: Vector[T]):T = {
        require(b.length == a.length, "Vectors must be the same length!")
        if (a.activeSize > b.activeSize) {
          bindingMissing(b, a)
        } else {
          var result : T = s.zero
          for( (k,v) <- a.activeIterator) {
            result = s.+(result,s.*(v, b(k)))
          }
          result
        }
      }
    }
  }


  @expand
  @expand.valify
  implicit def axpy[@expand.args(Int, Double, Float, Long) V]: TernaryUpdateRegistry[Vector[V], V, Vector[V], scaleAdd.type]  = {
    new TernaryUpdateRegistry[Vector[V], V, Vector[V], scaleAdd.type] {
      override def bindingMissing(a: Vector[V], s: V, b: Vector[V]) {
        require(b.length == a.length, "Vectors must be the same length!")
        if(s == 0) return

        var i = 0
        for( (k, v) <- b.activeIterator) {
          a(k) += s * v
          i += 1
        }
      }
    }
  }


  implicit def axpy[V:Semiring:ClassTag]: TernaryUpdateRegistry[Vector[V], V, Vector[V], scaleAdd.type]  = {
    new TernaryUpdateRegistry[Vector[V], V, Vector[V], scaleAdd.type] {
      val sr = implicitly[Semiring[V]]
      override def bindingMissing(a: Vector[V], s: V, b: Vector[V]) {
        require(b.length == a.length, "Vectors must be the same length!")
        if(s == 0) return

        var i = 0
        for( (k, v) <- b.activeIterator) {
          a(k) = sr.+(a(k), sr.*(s, v))
          i += 1
        }
      }
    }
  }

  @expand
  @expand.valify
  implicit def zipValuesImpl_V_V[@expand.args(Int, Double, Float, Long) T]: BinaryRegistry[Vector[T], Vector[T], zipValues.type, ZippedValues[T, T]] = {
    new BinaryRegistry[Vector[T], Vector[T], zipValues.type, ZippedValues[T, T]]  {
      protected override def bindingMissing(a: Vector[T], b: Vector[T]):ZippedValues[T, T] = {
        require(a.length == b.length, "vector dimension mismatch")
        ZippedVectorValues(a,b)
      }
    }
  }

  implicit def zipValuesSubclass[Vec1, Vec2, T, U](implicit view1: Vec1<:<Vector[T],
                                                   view2: Vec2 <:< Vector[U],
                                                   op: zipValues.Impl2[Vector[T], Vector[U], ZippedValues[T, U]]) = {
    op.asInstanceOf[zipValues.Impl2[Vec1, Vec2, ZippedValues[T, U]]]
  }

  case class ZippedVectorValues[@spec(Double, Int, Float, Long) T,
                                @spec(Double, Int, Float, Long) U](a: Vector[T], b: Vector[U]) extends ZippedValues[T, U] {
    def foreach(f: (T, U) => Unit): Unit = {
      var i = 0
      while(i < a.length) {
        f(a(i), b(i))
        i += 1
      }
    }
  }

  implicit def vAddIntoField[T](implicit field: Field[T], zero: Zero[T], ct: ClassTag[T]):OpAdd.InPlaceImpl2[Vector[T], Vector[T]] = {
    new OpAdd.InPlaceImpl2[Vector[T], Vector[T]] {
      override def apply(v: Vector[T], v2: Vector[T]) = {
        for(i <- 0 until v.length) v(i) = field.+(v(i), v2(i))
      }
    }

  }

  implicit def vSubIntoField[T](implicit field: Field[T], zero: Zero[T], ct: ClassTag[T]):OpSub.InPlaceImpl2[Vector[T], Vector[T]] = {
    new OpSub.InPlaceImpl2[Vector[T], Vector[T]] {
      override def apply(v: Vector[T], v2: Vector[T]) = {
        for(i <- 0 until v.length) v(i) = field.-(v(i), v2(i))
      }
    }

  }

  implicit def vMulIntoField[T](implicit field: Field[T], zero: Zero[T], ct: ClassTag[T]):OpMulScalar.InPlaceImpl2[Vector[T], Vector[T]] = {
    new OpMulScalar.InPlaceImpl2[Vector[T], Vector[T]] {
      override def apply(v: Vector[T], v2: Vector[T]) = {
        for(i <- 0 until v.length) v(i) = field.*(v(i), v2(i))
      }
    }

  }

  implicit def vDivIntoField[T](implicit field: Field[T], zero: Zero[T], ct: ClassTag[T]):OpDiv.InPlaceImpl2[Vector[T], Vector[T]] = {
    new OpDiv.InPlaceImpl2[Vector[T], Vector[T]] {
      override def apply(v: Vector[T], v2: Vector[T]) = {
        for(i <- 0 until v.length) v(i) = field./(v(i), v2(i))
      }
    }

  }


  implicit def vPowInto[T](implicit pow: OpPow.Impl2[T, T, T], zero: Zero[T], ct: ClassTag[T]):OpPow.InPlaceImpl2[Vector[T], Vector[T]] = {
    new OpPow.InPlaceImpl2[Vector[T], Vector[T]] {
      override def apply(v: Vector[T], v2: Vector[T]) = {
        for(i <- 0 until v.length) v(i) = pow(v(i), v2(i))
      }
    }

  }
  


  implicit def vAddIntoSField[T](implicit field: Semiring[T], zero: Zero[T], ct: ClassTag[T]):OpAdd.InPlaceImpl2[Vector[T], T] = {
    new OpAdd.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for(i <- 0 until v.length) v(i) = field.+(v(i), v2)
      }
    }

  }

  implicit def vAddSField[T](implicit field: Semiring[T], zero: Zero[T], ct: ClassTag[T]):OpAdd.Impl2[Vector[T], T, Vector[T]] = {
    binaryOpFromUpdateOp(implicitly[CanCopy[Vector[T]]], vAddIntoSField, ct)
  }
  implicit def vSubSField[T](implicit field: Ring[T], zero: Zero[T], ct: ClassTag[T]):OpSub.Impl2[Vector[T], T, Vector[T]]  = binaryOpFromUpdateOp(implicitly[CanCopy[Vector[T]]], vSubIntoSField, ct)
  implicit def vMulScalarSField[T](implicit field: Semiring[T], zero: Zero[T], ct: ClassTag[T]):OpMulScalar.Impl2[Vector[T], T, Vector[T]]  = binaryOpFromUpdateOp(implicitly[CanCopy[Vector[T]]], vMulScalarIntoSField, ct)
  implicit def vDivSField[T](implicit field: Field[T], zero: Zero[T], ct: ClassTag[T]):OpDiv.Impl2[Vector[T], T, Vector[T]]  = binaryOpFromUpdateOp(implicitly[CanCopy[Vector[T]]], vDivIntoSField, ct)
  implicit def vPowS[T](implicit pow: OpPow.Impl2[T, T, T], zero: Zero[T], ct: ClassTag[T]):OpPow.Impl2[Vector[T], T, Vector[T]]  = binaryOpFromUpdateOp(implicitly[CanCopy[Vector[T]]], vPowIntoS, ct)


  implicit def vSubIntoSField[T](implicit field: Ring[T], zero: Zero[T], ct: ClassTag[T]):OpSub.InPlaceImpl2[Vector[T], T] = {
    new OpSub.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for(i <- 0 until v.length) v(i) = field.-(v(i), v2)
      }
    }

  }


  implicit def vMulScalarIntoSField[T](implicit field: Semiring[T], zero: Zero[T], ct: ClassTag[T]):OpMulScalar.InPlaceImpl2[Vector[T], T] = {
    new OpMulScalar.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for(i <- 0 until v.length) v(i) = field.*(v(i), v2)
      }
    }
  }

  implicit def vDivIntoSField[T](implicit field: Field[T], zero: Zero[T], ct: ClassTag[T]):OpDiv.InPlaceImpl2[Vector[T], T] = {
    new OpDiv.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for(i <- 0 until v.length) v(i) = field./(v(i), v2)
      }
    }
  }

  implicit def vPowIntoS[T](implicit pow: OpPow.Impl2[T, T, T], zero: Zero[T], ct: ClassTag[T]):OpPow.InPlaceImpl2[Vector[T], T] = {
    new OpPow.InPlaceImpl2[Vector[T], T] {
      override def apply(v: Vector[T], v2: T) = {
        for(i <- 0 until v.length) v(i) = pow(v(i), v2)
      }
    }
  }
 
  implicit def dotField[T](implicit field: Semiring[T]):OpMulInner.Impl2[Vector[T], Vector[T], T] = {
    new OpMulInner.Impl2[Vector[T], Vector[T], T] {
      override def apply(v: Vector[T], v2: Vector[T]): T = {
        var acc = field.zero
        for(i <- 0 until v.length) {
          acc = field.+(acc, field.*(v(i), v2(i)))
        }
        acc
      }
    }
  }


  def binaryOpFromUpdateOp[Op<:OpType, V, Other]
  (implicit copy: CanCopy[Vector[V]], op: UFunc.InPlaceImpl2[Op, Vector[V], Other], man: ClassTag[V]):
  UFunc.UImpl2[Op, Vector[V], Other, Vector[V]] = {
    new UFunc.UImpl2[Op, Vector[V], Other, Vector[V]] {
      override def apply(a : Vector[V], b : Other): Vector[V] = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def implOpSet_V_V_InPlace[V]: OpSet.InPlaceImpl2[Vector[V], Vector[V]] = {

    new OpSet.InPlaceImpl2[Vector[V], Vector[V]] {
      def apply(a: Vector[V], b: Vector[V]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")

        for (i <- 0 until a.length) {
          a(i) = b(i)
        }


      }
    }
  }

  implicit def implOpSet_V_S_InPlace[V]: OpSet.InPlaceImpl2[Vector[V], V] = {

    new OpSet.InPlaceImpl2[Vector[V], V] {
      def apply(a: Vector[V], b: V): Unit = {
        for (i <- 0 until a.length) {
          a(i) = b
        }


      }
    }
  }

  implicit def canGaxpy[V:Semiring]: scaleAdd.InPlaceImpl3[Vector[V], V, Vector[V]] =

    new scaleAdd.InPlaceImpl3[Vector[V], V, Vector[V]] {
      val ring = implicitly[Semiring[V]]
      def apply(a: Vector[V], s: V, b: Vector[V]): Unit = {
        require(b.length == a.length, "Vectors must be the same length!")
        for (i <- 0 until a.length) {
          a(i) = ring.+(a(i), ring.*(s, b(i)))
        }

      }
    }

}

/**
 * Trait that can mixed to companion objects to enable utility methods for creating vectors.
 * @tparam Vec
 */
trait VectorConstructors[Vec[T]<:Vector[T]] {
  /**
   * Creates a Vector of size size.
   * @param size
   * @tparam V
   * @return
   */
  def zeros[V:ClassTag:Zero](size: Int): Vec[V]

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
  def apply[V:ClassTag](values: V*): Vec[V] = {
    // manual specialization so that we create the right DenseVector specialization... @specialized doesn't work here
    val man = implicitly[ClassTag[V]]
    if(man == manifest[Double]) apply(values.toArray.asInstanceOf[Array[Double]]).asInstanceOf[Vec[V]]
    else if (man == manifest[Float]) apply(values.toArray.asInstanceOf[Array[Float]]).asInstanceOf[Vec[V]]
    else if (man == manifest[Int]) apply(values.toArray.asInstanceOf[Array[Int]]).asInstanceOf[Vec[V]]
    else apply(values.toArray)
//     apply(values.toArray)
  }

  //ToDo 2: I'm not sure fill/tabulate are really useful outside of the context of a DenseVector?
  /**
   * Analogous to Array.fill
   * @param size
   * @param v
   * @tparam V
   * @return
   */
  def fill[@spec(Double, Int, Float, Long) V:ClassTag](size: Int)(v: =>V): Vec[V] = {
    apply(Array.fill(size)(v))
  }


  /**
   * Analogous to Array.tabulate
   * @param size
   * @param f
   * @tparam V
   * @return
   */
  def tabulate[@spec(Double, Int, Float, Long) V:ClassTag](size: Int)(f: Int=>V): Vec[V] = {
    apply(Array.tabulate(size)(f))
  }

  /**
   * Analogous to Array.tabulate, but taking a scala.Range to iterate over, instead of an index.
   * @param f
   * @tparam V
   * @return
   */
  def tabulate[@spec(Double, Int, Float, Long) V:ClassTag](range: Range)(f: Int=>V):Vec[V]= {
    val b = ArrayBuilder.make[V]()
    b.sizeHint(range.length)
    var i = 0
    while (i < range.length) {
      b += f( range(i) )
      i += 1
    }
    apply(b.result )
  }

  implicit def canCreateZeros[V:ClassTag:Zero]: CanCreateZeros[Vec[V], Int] =
    new CanCreateZeros[Vec[V], Int] {
      def apply(d: Int): Vec[V] = {
        zeros[V](d)
      }
    }

  implicit def canTabulate[V:ClassTag:Zero]: CanTabulate[Int, Vec[V], V] = new CanTabulate[Int,Vec[V],V] {
    def apply(d: Int, f: (Int) => V): Vec[V] = tabulate(d)(f)
  }

  /**
   * Creates a Vector of uniform random numbers in (0,1)
   * @param size
   * @param rand
   * @return
   */
  def rand[T:ClassTag](size: Int, rand: Rand[T] = Rand.uniform): Vec[T] = {
    // Array#fill is slow.
    val arr = new Array[T](size)
    var i = 0
    while(i < arr.length) {
      arr(i) = rand.draw()
      i += 1
    }

    apply(arr)
  }

  def range(start:Int, end: Int): Vec[Int] = range(start,end,1)
  def range(start:Int, end: Int, step: Int): Vec[Int] = apply[Int](Array.range(start,end,step))

  def rangeF(start: Float, end: Float, step: Float = 1.0f): Vec[Float] = {
    import spire.implicits.cfor
    require(end > start)
    require(end-start > step)
    val size: Int = math.floor((end - start)/step).toInt
    val data = new Array[Float](size)
    cfor(0)(i => i < size, i => i+1)(i => {
      data(i) = (start+i*step)
    })
    apply(data)
  }

  def rangeD(start: Double, end: Double, step: Double = 1.0): Vec[Double] = {
    import spire.implicits.cfor
    require(end > start)
    require(end-start > step)
    val size: Int = math.floor((end - start)/step).toInt
    val data = new Array[Double](size)
    cfor(0)(i => i < size, i => i+1)(i => {
      data(i) = (start+i*step)
    })
    apply(data)
  }
}

trait StorageVector[V] extends Vector[V] with Storage[V]
