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
import breeze.generic.{UFunc}
import breeze.math._
import scala.math.BigInt
import collection.immutable.BitSet
import breeze.linalg.support._
import breeze.storage.{Zero, Storage}
import scala.reflect.ClassTag
import breeze.stats.distributions.Rand
import breeze.macros.expand
import scala.annotation.unchecked.uncheckedVariance
import CanTraverseValues.ValuesVisitor
import scala.collection.mutable.ArrayBuilder
import breeze.generic.UFunc.{UImpl2, UImpl, InPlaceImpl2}

/**
 * Trait for operators and such used in vectors.
 * @author dlwh
 */
trait VectorLike[@spec E, +Self <: Vector[E]] extends Tensor[Int, E] with TensorLike[Int, E, Self] {
  def map[E2, That](fn: E=>E2)(implicit canMapValues: CanMapValues[Self  @uncheckedVariance, E, E2, That]):That = values map fn

  def foreach[U](fn: E=>U) { values foreach fn }

  def copy: Self
}


/**
 * A Vector represents the mathematical concept of a vector in math.
 * @tparam E
 */
trait Vector[@spec(Int, Double, Float) E] extends VectorLike[E, Vector[E]]{

  /**
   * @return the set of keys in this vector (0 until length)
   */
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

  def toDenseVector(implicit cm: ClassTag[E]) = {
    new DenseVector(toArray)
  }

  /**Returns copy of this [[breeze.linalg.Vector]] as a [[scala.Array]]*/
  def toArray(implicit cm: ClassTag[E]) = {
    val result = new Array[E](length)
    var i = 0
    while(i < length) {
      result(i) = apply(i)
      i += 1
    }
    result
  }

  /**Returns copy of this [[breeze.linalg.Vector]] as a [[scala.Vector]]*/
  def toVector(implicit cm: ClassTag[E]) = Vector[E]( toArray )

  //ToDo 2: implement fold/scan/reduce to operate along one axis of a matrix/tensor
  // <editor-fold defaultstate="collapsed" desc=" scala.collection -like padTo, fold/scan/reduce ">

  /** See [[scala.collection.mutable.ArrayOps.padTo]].
    */
  def padTo(len: Int, elem: E)(implicit cm: ClassTag[E]): Vector[E] = Vector[E]( toArray.padTo(len, elem) )

  /** See [[scala.collection.mutable.ArrayOps.fold]].
    */
  def fold[E1 >: E](z: E1)(op: (E1, E1) => E1 )(implicit cm: ClassTag[E]): E1 = toArray.fold(z)( op )
  /** See [[scala.collection.mutable.ArrayOps.foldLeft]].
    */
  def foldLeft[B >: E](z: B)(op: (B, E) => B )(implicit cm: ClassTag[E]): B = {
    val it = valuesIterator
    it.foldLeft(z)( op )
  }
  /** See [[scala.collection.mutable.ArrayOps.foldRight]].
    */
  def foldRight[B >: E](z: B)(op: (E, B) => B )(implicit cm: ClassTag[E]): B = toArray.foldRight(z)( op )

  /** See [[scala.collection.mutable.ArrayOps.reduce]].
    */
  def reduce[E1 >: E](op: (E1, E1) => E1 )(implicit cm: ClassTag[E], cm1: ClassTag[E1]): Vector[E1] = Vector[E1]( toArray.reduce( op ))
  /** See [[scala.collection.mutable.ArrayOps.reduceLeft]].
    */
  def reduceLeft[B >: E](op: (B, E) => B )(implicit cm: ClassTag[E]): B = {
    val it = valuesIterator
    it.reduceLeft( op )
  }
  /** See [[scala.collection.mutable.ArrayOps.reduceRight]].
    */
  def reduceRight[B >: E](op: (E, B) => B )(implicit cm: ClassTag[E]): B = toArray.reduceRight( op )

  /** See [[scala.collection.mutable.ArrayOps.scan]].
    */
  def scan[E1 >: E](z: E1)(op: (E1, E1) => E1 )(implicit cm: ClassTag[E], cm1: ClassTag[E1]): Vector[E1] = Vector[E1]( toArray.scan(z)( op ))
  /** See [[scala.collection.mutable.ArrayOps.scanLeft]].
    */
  def scanLeft[B >: E](z: B)(op: (B, E) => B )(implicit cm: ClassTag[E], cm1: ClassTag[B]): Vector[B] = {
    val it = valuesIterator
    Vector[B]( it.scanLeft(z)( op ).toArray )
  }
  /** See [[scala.collection.mutable.ArrayOps.scanRight]].
    */
  def scanRight[B >: E](z: B)(op: (E, B) => B )(implicit cm: ClassTag[E], cm1: ClassTag[B]): Vector[B] = Vector[B]( toArray.scanRight(z)( op ) )

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
  def apply[@specialized(Int, Float, Double) V](values: Array[V]): Vector[V] = DenseVector(values)

  implicit def canCopy[E]:CanCopy[Vector[E]] = new CanCopy[Vector[E]] {
    // Should not inherit from T=>T because those get  used by the compiler.
    def apply(t: Vector[E]): Vector[E] = t.copy
  }

  // There's a bizarre error specializing float's here.
  class CanZipMapValuesVector[@specialized(Int, Double) V, @specialized(Int, Double) RV:ClassTag] extends CanZipMapValues[Vector[V],V,RV,Vector[RV]] {
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

  // the canmapvalues implicit in UFunc should take care of this, but limits of scala type inference, blah blah blah
  implicit def mapUFuncImpl[Tag, V,  U](implicit impl: UFunc.UImpl[Tag, V, U], canMapValues: CanMapValues[Vector[V], V, U, Vector[U]]): UFunc.UImpl[Tag, Vector[V], Vector[U]] = {
    new UFunc.UImpl[Tag, Vector[V], Vector[U]] {
      def apply(v: Vector[V]): Vector[U] = canMapValues.map(v, impl.apply)
    }
  }

  implicit def canMapValues[V, V2](implicit man: ClassTag[V2]):CanMapValues[Vector[V], V, V2, Vector[V2]] = {
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

  implicit def handholdCMV[T]= new CanMapValues.HandHold[Vector[T], T]

  implicit def negFromScale[@specialized(Int, Float, Double) V, Double](implicit scale: OpMulScalar.Impl2[Vector[V], V, Vector[V]], ring: Ring[V]) = {
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

    /** Iterates all values from the given collection. */
    def traverse(from: Vector[V], fn: ValuesVisitor[V]): Unit = {
      for( v <- from.valuesIterator) {
        fn.visit(v)
      }
    }

  }


  implicit val space_d = TensorSpace.make[Vector[Double], Int, Double]
  implicit val space_f = TensorSpace.make[Vector[Float], Int, Float]
  implicit val space_i = TensorSpace.make[Vector[Int], Int, Int]

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

  implicit def castUpdateOps[V1, V2, T, Op <: OpType](implicit v1ev: V1<:<Vector[T],
                                                V2ev: V2<:<Vector[T],
                                                op: UFunc.InPlaceImpl2[Op, Vector[T], Vector[T]]): InPlaceImpl2[Op, V1, V2] = {
    op.asInstanceOf[UFunc.InPlaceImpl2[Op, V1, V2]]
  }

  implicit def castOps[V1, V2, T, Op <: OpType, VR](implicit v1ev: V1<:<Vector[T],
                                                    V2ev: V2<:<Vector[T],
                                                    op: UImpl2[Op, Vector[T], Vector[T], VR]): UImpl2[Op, V1, V2, VR] = {
    op.asInstanceOf[UFunc.UImpl2[Op, V1, V2, VR]]
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

  case class ZippedVectorValues[@specialized(Int, Double, Long, Float) T,
                                @specialized(Int, Double, Long, Float) U](a: Vector[T], b: Vector[U]) extends ZippedValues[T, U] {
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
  def zeros[V:ClassTag:Zero](size: Int):Vec[V]

  /**
   * Creates a vector with the specified elements
   * @param values
   * @tparam V
   * @return
   */
  def apply[@spec(Double, Int, Float) V](values: Array[V]):Vec[V]

  /**
   * Creates a vector with the specified elements
   * @param values
   * @tparam V
   * @return
   */
  def apply[V:ClassTag](values: V*):Vec[V] = {
    // manual specialization so that we create the right DenseVector specialization... @specialized doesn't work here
    val man = implicitly[ClassTag[V]]
    if(man == manifest[Double]) apply(values.toArray.asInstanceOf[Array[Double]]).asInstanceOf[Vec[V]]
    else if (man == manifest[Float]) apply(values.toArray.asInstanceOf[Array[Float]]).asInstanceOf[Vec[V]]
    else if (man == manifest[Int]) apply(values.toArray.asInstanceOf[Array[Int]]).asInstanceOf[Vec[V]]
    else apply(values.toArray)
//     apply(values.toArray)
  }

  /**
   * Analogous to Array.fill
   * @param size
   * @param v
   * @tparam V
   * @return
   */
  def fill[@spec(Double, Int, Float) V:ClassTag](size: Int)(v: =>V):Vec[V] = apply(Array.fill(size)(v))

  /**
   * Analogous to Array.tabulate
   * @param size
   * @param f
   * @tparam V
   * @return
   */
  def tabulate[@spec(Double, Int, Float) V:ClassTag](size: Int)(f: Int=>V):Vec[V]= apply(Array.tabulate(size)(f))

  /**
   * Analogous to Array.tabulate, but taking a scala.Range to iterate over, instead of an index.
   * @param f
   * @tparam V
   * @return
   */
  def tabulate[@spec(Double, Int, Float) V:ClassTag](range: Range)(f: Int=>V):Vec[V]= {
    val b = ArrayBuilder.make[V]()
    b.sizeHint(range.length)
    var i = 0
    while (i < range.length) {
      b += f( range(i) )
      i += 1
    }
    apply(b.result )
  }




  /**
   * Creates a Vector of uniform random numbers in (0,1)
   * @param size
   * @param rand
   * @return
   */
  def rand[T:ClassTag](size: Int, rand: Rand[T] = Rand.uniform) = {
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

trait StorageVector[E] extends Vector[E] with Storage[E]
