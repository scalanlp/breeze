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
import breeze.math.{Complex, TensorSpace, Ring}
import scala.math.BigInt
import collection.immutable.BitSet
import breeze.linalg.support._
import breeze.storage.{DefaultArrayValue, Storage}
import scala.reflect.ClassTag
import breeze.stats.distributions.Rand
import breeze.macros.expand
import scala.annotation.unchecked.uncheckedVariance
import CanTraverseValues.ValuesVisitor
import scala.collection.mutable.ArrayBuilder

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

}

object Vector extends VectorConstructors[Vector] with VectorOps {


  /**
   * Creates a Vector of size size.
   * @param size
   * @tparam V
   * @return
   */
  def zeros[V: ClassTag : DefaultArrayValue](size: Int): Vector[V] = DenseVector.zeros(size)


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
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def v_v_Idempotent_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
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
  implicit def v_v_nilpotent_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T):BinaryRegistry[Vector[T], Vector[T], OpMulScalar.type, Vector[T]] = new BinaryRegistry[Vector[T], Vector[T], OpMulScalar.type, Vector[T]] {
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
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def v_v_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
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
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def cast_v_v_Op[V1, V2,
  @expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar,OpDiv, OpSet, OpMod, OpPow) Op <: OpType](implicit v1: V1<:<Vector[T], v2: V2<:<Vector[T]) = {
    implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].asInstanceOf[Op.Impl2[V1, V2, Vector[T]]]
  }
  */

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def v_s_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T],
  @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero)
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
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def v_v_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
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
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def v_v_Idempotent_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
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

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def v_s_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
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
  implicit def canDot_V_V[@expand.args(Int, Long, BigInt, Complex, Float, Double) T](implicit @expand.sequence[T](0, 0l, BigInt(0), Complex.zero, 0.0f, 0.0) zero: T): BinaryRegistry[Vector[T], Vector[T], breeze.linalg.operators.OpMulInner.type, T] = {
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
  implicit def axpy[@expand.args(Int, Double, Float, Long, BigInt, Complex) V]: CanAxpy[V, Vector[V], Vector[V]] = {
    new CanAxpy[V, Vector[V], Vector[V]] {
      def apply(s: V, b: Vector[V], a: Vector[V]) {
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
  def zeros[V:ClassTag:DefaultArrayValue](size: Int):Vec[V]

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
}

trait StorageVector[E] extends Vector[E] with Storage[E]
