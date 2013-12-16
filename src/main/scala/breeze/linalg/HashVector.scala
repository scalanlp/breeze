package breeze.linalg

import breeze.collection.mutable.OpenAddressHashArray
import breeze.linalg.operators._
import breeze.storage.{ConfigurableDefault, DefaultArrayValue}
import breeze.generic.{UFunc, CanMapValues, URFunc}
import support.{CanZipMapValues, CanMapKeyValuePairs, CanCopy}
import breeze.math.{Semiring, TensorSpace, Ring, Complex}
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3
import breeze.macros.expand
import scala.math.BigInt

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
    f.apply(array.data, 0, 1, array.data.length, array.isActive _)
  }

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
    val dv = array.default.value(array.defaultArrayValue)
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
                          with SparseVector_HashVector_Ops {
  def zeros[@specialized(Double, Float, Int) V: ClassTag:DefaultArrayValue](size: Int) = {
    new HashVector(new OpenAddressHashArray[V](size))
  }
  def apply[@specialized(Double, Float, Int) V:DefaultArrayValue](values: Array[V]) = {
    implicit val man = ClassTag[V](values.getClass.getComponentType.asInstanceOf[Class[V]])
    val oah = new OpenAddressHashArray[V](values.length)
    for( (v,i) <- values.zipWithIndex) oah(i) = v
    new HashVector(oah)
  }

  def apply[V:ClassTag:DefaultArrayValue](values: V*):HashVector[V] = {
    apply(values.toArray)
  }
  def fill[@specialized(Double, Int, Float) V:ClassTag:DefaultArrayValue](size: Int)(v: =>V):HashVector[V] = apply(Array.fill(size)(v))
  def tabulate[@specialized(Double, Int, Float) V:ClassTag:DefaultArrayValue](size: Int)(f: Int=>V):HashVector[V]= apply(Array.tabulate(size)(f))

  def apply[V:ClassTag:DefaultArrayValue](length: Int)(values: (Int, V)*) = {
    val r = zeros[V](length)
    for( (i, v) <- values) {
      r(i) = v
    }
    r
  }

  // implicits



  // implicits
  class CanCopyHashVector[@specialized(Int, Float, Double) V:ClassTag:DefaultArrayValue] extends CanCopy[HashVector[V]] {
    def apply(v1: HashVector[V]) = {
      v1.copy
    }
  }

  implicit def canCopyHash[@specialized(Int, Float, Double) V: ClassTag: DefaultArrayValue] = new CanCopyHashVector[V]

  // the canmapvalues implicit in UFunc should take care of this, but limits of scala type inference, blah blah blah
  implicit def mapUFuncImpl[Tag, V,  U](implicit impl: UFunc.UImpl[Tag, V, U], canMapValues: CanMapValues[HashVector[V], V, U, HashVector[U]]): UFunc.UImpl[Tag, HashVector[V], HashVector[U]] = {
    new UFunc.UImpl[Tag, HashVector[V], HashVector[U]] {
      def apply(v: HashVector[V]): HashVector[U] = canMapValues.map(v, impl.apply)
    }
  }



  implicit def canMapValues[V, V2: ClassTag: DefaultArrayValue]:CanMapValues[HashVector[V], V, V2, HashVector[V2]] = {
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

  implicit def canMapPairs[V, V2: ClassTag: DefaultArrayValue]:CanMapKeyValuePairs[HashVector[V], Int, V, V2, HashVector[V2]] = {
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

  class CanZipMapValuesHashVector[@specialized(Int, Double, Float) V, @specialized(Int, Double) RV:ClassTag:DefaultArrayValue] extends CanZipMapValues[HashVector[V],V,RV,HashVector[RV]] {
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
  implicit def zipMap[V, R:ClassTag:DefaultArrayValue] = new CanZipMapValuesHashVector[V, R]
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



trait DenseVector_HashVector_Ops { this: HashVector.type =>
  import breeze.math.PowImplicits._


  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dv_hv_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[DenseVector[T], HashVector[T], Op] = new BinaryUpdateOp[DenseVector[T], HashVector[T], Op] {
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
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dv_hv_op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType] = DenseVector.pureFromUpdate(implicitly[BinaryUpdateOp[DenseVector[T], HashVector[T], Op]])


  @expand
  implicit def dv_hv_Update_Zero_Idempotent[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _})
  op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[DenseVector[T], HashVector[T], Op] = new BinaryUpdateOp[DenseVector[T], HashVector[T], Op] {
    def apply(a: DenseVector[T], b: HashVector[T]):Unit = {
      require(a.length == b.length, "Vectors must have the same length")
      val ad = a.data
      val bd = b.data
      val bi = b.index
      val bsize = b.iterableSize

      var i = 0
      while(i < bsize) {
        val aoff = a.offset + bi(i) * a.stride
        if(b.isActive(i))
          ad(aoff) = op(ad(aoff), bd(i))
        i += 1
      }
    }
  }


  @expand
  implicit def canDot_DV_HV[@expand.args(Int, Double, Float, Long, BigInt, Complex) T](implicit @expand.sequence[T](0, 0.0, 0f, 0l, BigInt(0), Complex.zero) zero: T): BinaryOp[DenseVector[T], HashVector[T], breeze.linalg.operators.OpMulInner, T] = {
    new BinaryOp[DenseVector[T], HashVector[T], breeze.linalg.operators.OpMulInner, T] {
      def apply(a: DenseVector[T], b: HashVector[T]) = {
        var result: T = zero

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        val adata = a.data
        val aoff = a.offset
        val stride = a.stride

        var i = 0
        while(i < bsize) {
          if(b.isActive(i))
            result += adata(aoff + bi(i) * stride) * bd(i)
          i += 1
        }
        result
      }
    }
  }


}

trait HashVector_DenseVector_Ops extends DenseVector_HashVector_Ops { this: HashVector.type =>
  import breeze.math.PowImplicits._


  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def hv_dv_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[HashVector[T], DenseVector[T], Op] = new BinaryUpdateOp[HashVector[T], DenseVector[T], Op] {
    def apply(a: HashVector[T], b: DenseVector[T]):Unit = {
      require(a.length == b.length, "Vectors must have the same length")

      var i = 0
      while(i < b.length) {
        a(i) = op(a(i), b(i))
        i += 1
      }
    }
  }


  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def hv_dv_op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
                        @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryOp[HashVector[T], DenseVector[T], Op, DenseVector[T]] = {
    new BinaryOp[HashVector[T], DenseVector[T], Op, DenseVector[T]] {
      def apply(a: HashVector[T], b: DenseVector[T]) = {
        require(a.length == b.length, "Vectors must have the same length")
        val result = DenseVector.zeros[T](a.length)

        var i = 0
        while(i < b.length) {
          result(i) = op(a(i), b(i))
          i += 1
        }

        result
      }
    }
  }

  @expand
  implicit def canDot_HV_DV[@expand.args(Int, Float, Double, Long, BigInt, Complex) T]: BinaryOp[HashVector[T], DenseVector[T], breeze.linalg.operators.OpMulInner, T] = {
    new BinaryOp[HashVector[T], DenseVector[T], breeze.linalg.operators.OpMulInner, T] {
      def apply(a: HashVector[T], b: DenseVector[T]) = {
        require(b.length == a.length, "Vectors must be the same length!")
        b dot a
      }
    }
      //      Vector.canDotProductV_T.register(this)
  }


}

trait HashVectorOps extends HashVector_GenericOps { this: HashVector.type =>
  import breeze.math.PowImplicits._


  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def hv_hv_Idempotent_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _})
  op: BinaryOp[T, T, Op, T]):BinaryOp[HashVector[T], HashVector[T], Op, HashVector[T]] = new BinaryOp[HashVector[T], HashVector[T], Op, HashVector[T]] {
    def apply(a: HashVector[T], b: HashVector[T]): HashVector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val result = a.copy
      for((k,v) <- b.activeIterator) {
        result(k) = op(a(k), v)
      }
      result
    }
  }


  @expand
  implicit def hv_hv_nilpotent_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T):BinaryOp[HashVector[T], HashVector[T], OpMulScalar, HashVector[T]] = new BinaryOp[HashVector[T], HashVector[T], OpMulScalar, HashVector[T]] {
    def apply(a: HashVector[T], b: HashVector[T]): HashVector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val builder = new VectorBuilder[T](a.length)
      for((k,v) <- b.activeIterator) {
        val r = a(k) * v
        if(r != zero)
          builder.add(k, r)
      }
      builder.toHashVector
    }
  }



  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def hv_hv_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryOp[HashVector[T], HashVector[T], Op, HashVector[T]] = new BinaryOp[HashVector[T], HashVector[T], Op, HashVector[T]] {
    def apply(a: HashVector[T], b: HashVector[T]): HashVector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val result = HashVector.zeros[T](a.length)
      var i = 0
      while(i < a.length) {
        result(i) = op(a(i), b(i))
        i += 1
      }
      result
    }
  }

  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def hv_v_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryOp[HashVector[T], Vector[T], Op, HashVector[T]] = new BinaryOp[HashVector[T], Vector[T], Op, HashVector[T]] {
    def apply(a: HashVector[T], b: Vector[T]): HashVector[T] = {

      require(b.length == a.length, "Vectors must be the same length!")
      val result = HashVector.zeros[T](a.length)

      var i = 0
      while(i < a.length) {
        result(i) = op(a(i), b(i))
        i += 1
      }
      result
    }
  }

  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def hv_s_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T],
  @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero)
  zero: T):BinaryOp[HashVector[T], T, Op, HashVector[T]] = new BinaryOp[HashVector[T], T, Op, HashVector[T]] {
    def apply(a: HashVector[T], b: T): HashVector[T] = {
      val result = HashVector.zeros[T](a.length)

      var i = 0
      while(i < a.length) {
        result(i) = op(a(i), b)
        i += 1
      }
      result
    }
  }



  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def hv_hv_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[HashVector[T], HashVector[T], Op] = new BinaryUpdateOp[HashVector[T], HashVector[T], Op] {
    def apply(a: HashVector[T], b: HashVector[T]):Unit = {
      require(b.length == a.length, "Vectors must be the same length!")
      var i = 0
      while(i < a.length) {
        a(i) = op(a(i), b(i))
        i += 1
      }
    }
  }

  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def hv_hv_Idempotent_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _})
  op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[HashVector[T], HashVector[T], Op] = new BinaryUpdateOp[HashVector[T], HashVector[T], Op] {
    def apply(a: HashVector[T], b: HashVector[T]):Unit = {
      require(b.length == a.length, "Vectors must be the same length!")
      for( (k,v) <- b.activeIterator) {
        a(k) = op(a(k), v)
      }
    }
  }

  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def hv_s_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _})
  op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[HashVector[T], T, Op] = new BinaryUpdateOp[HashVector[T], T, Op] {
    def apply(a: HashVector[T], b: T):Unit = {
      var i = 0
      while(i < a.length) {
        a(i) = op(a(i), b)
        i += 1
      }
    }
  }



  @expand
  implicit def canDot_HV_HV[@expand.args(Int, Long, BigInt, Complex, Double, Float) T](implicit @expand.sequence[T](0, 0l, BigInt(0), Complex.zero, 0.0, 0f) zero: T): BinaryOp[HashVector[T], HashVector[T], breeze.linalg.operators.OpMulInner, T] = {
    new BinaryOp[HashVector[T], HashVector[T], breeze.linalg.operators.OpMulInner, T] {
      def apply(a: HashVector[T], b: HashVector[T]):T = {
        require(b.length == a.length, "Vectors must be the same length!")

        if (a.iterableSize > b.iterableSize) {
          apply(b, a)
        } else {
          var result : T = zero
          for( (k,v) <- a.activeIterator) {
            result += v * b(k)
          }
          result
        }
      }
//      Vector.canDotProductV_T.register(this)
    }
  }


}



trait HashVector_SparseVector_Ops extends HashVectorOps { this: HashVector.type =>
  import breeze.math.PowImplicits._


  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def hv_sv_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T],
   @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T):BinaryOp[HashVector[T], SparseVector[T], Op, HashVector[T]] = new BinaryOp[HashVector[T], SparseVector[T], Op, HashVector[T]] {
    def apply(a: HashVector[T], b: SparseVector[T]): HashVector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val builder = new VectorBuilder[T](a.length)
      for((k,v) <- b.iterator) {
        val r = op(a(k), v)
        if(r != zero)
          builder.add(k, r)
      }
      builder.toHashVector
    }
  }


  @expand
  implicit def hv_sv_nilpotent_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T):BinaryOp[HashVector[T], SparseVector[T], OpMulScalar, HashVector[T]] = new BinaryOp[HashVector[T], SparseVector[T], OpMulScalar, HashVector[T]] {
    def apply(a: HashVector[T], b: SparseVector[T]): HashVector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val builder = new VectorBuilder[T](a.length)
      for((k,v) <- b.activeIterator) {
        val r = a(k) * v
        if(r != zero)
          builder.add(k, r)
      }
      builder.toHashVector
    }
  }


  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def hv_sv_Idempotent_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _})
  op: BinaryOp[T, T, Op, T]):BinaryOp[HashVector[T], SparseVector[T], Op, HashVector[T]] = new BinaryOp[HashVector[T], SparseVector[T], Op, HashVector[T]] {
    def apply(a: HashVector[T], b: SparseVector[T]): HashVector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val result = a.copy
      var boff = 0

      while(boff < b.activeSize) {
        val k = b.indexAt(boff)
        val v = b.valueAt(boff)
        result(k) = op(a(k), v)
        boff += 1
      }
      result
    }
  }


  @expand
  implicit def canDot_HV_SV[@expand.args(Int, Long, BigInt, Complex, Float, Double) T](implicit @expand.sequence[T](0, 0l, BigInt(0), Complex.zero, 0f, 0.0) zero: T): BinaryOp[HashVector[T], SparseVector[T], breeze.linalg.operators.OpMulInner, T] = {
    new BinaryOp[HashVector[T], SparseVector[T], breeze.linalg.operators.OpMulInner, T] {
      def apply(a: HashVector[T], b: SparseVector[T]) = {
        require(b.length == a.length, "Vectors must be the same length!")
        var result: T = zero
        var boff = 0

        while(boff < b.activeSize) {
          result += a(b.indexAt(boff)) * b.valueAt(boff)
          boff += 1
        }

        result
      }
//      Vector.canDotProductV_T.register(this)
    }
  }



  protected def updateFromPure[T, Op<:OpType, Other](implicit op: BinaryOp[HashVector[T], Other, Op, HashVector[T]],
                                                              set: BinaryUpdateOp[HashVector[T], HashVector[T], OpSet]): BinaryUpdateOp[HashVector[T], Other, Op] = {
    new BinaryUpdateOp[HashVector[T], Other, Op] {
      def apply(a: HashVector[T], b: Other) {
        val result = op(a, b)
        a := result
      }
    }
  }

  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def hv_sv_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  :BinaryUpdateOp[HashVector[T], SparseVector[T], Op] = updateFromPure



}


trait SparseVector_HashVector_Ops extends HashVectorOps with HashVector_SparseVector_Ops { this: HashVector.type =>
  import breeze.math.PowImplicits._


  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def sv_hv_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T],
   @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T):BinaryOp[SparseVector[T], HashVector[T], Op, SparseVector[T]] = new BinaryOp[SparseVector[T], HashVector[T], Op, SparseVector[T]] {
    def apply(a: SparseVector[T], b: HashVector[T]): SparseVector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val builder = new VectorBuilder[T](a.length)
      for((k,v) <- b.iterator) {
        val r = op(a(k), v)
        if(r != zero)
          builder.add(k, r)
      }
      builder.toSparseVector
    }
  }

  @expand
  implicit def sv_hv_nilpotent_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T):BinaryOp[SparseVector[T], HashVector[T], OpMulScalar, SparseVector[T]] = new BinaryOp[SparseVector[T], HashVector[T], OpMulScalar, SparseVector[T]] {
    def apply(a: SparseVector[T], b: HashVector[T]): SparseVector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val builder = new VectorBuilder[T](a.length)
      for((k,v) <- a.activeIterator) {
        val r = v * b(k)
        if(r != zero)
          builder.add(k, r)
      }
      builder.toSparseVector(true, true)
    }
  }


  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def sv_hv_Idempotent_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _})
  op: BinaryOp[T, T, Op, T]):BinaryOp[SparseVector[T], HashVector[T], Op, SparseVector[T]] = new BinaryOp[SparseVector[T], HashVector[T], Op, SparseVector[T]] {
    def apply(a: SparseVector[T], b: HashVector[T]): SparseVector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val builder = new VectorBuilder[T](a.length)
      var aoff = 0
      while(aoff < a.activeSize) {
        val k = a.indexAt(aoff)
        val v = a.valueAt(aoff)
        builder.add(k,v)
        aoff += 1
      }

      for((k,v) <- b.activeIterator) {
        builder.add(k, v)
      }

      builder.toSparseVector
    }
  }




  implicit def canDot_SV_HV[T](implicit op: BinaryOp[HashVector[T], SparseVector[T], OpMulInner, T]): BinaryOp[SparseVector[T], HashVector[T], breeze.linalg.operators.OpMulInner, T] = {
    new BinaryOp[SparseVector[T], HashVector[T], breeze.linalg.operators.OpMulInner, T] {
      def apply(a: SparseVector[T], b: HashVector[T]) = {
        b dot a
      }
    }
  }

  protected def updateFromPureS[T, Op<:OpType, Other](implicit op: BinaryOp[SparseVector[T], Other, Op, SparseVector[T]],
                                                     set: BinaryUpdateOp[SparseVector[T], SparseVector[T], OpSet]): BinaryUpdateOp[SparseVector[T], Other, Op] = {
    new BinaryUpdateOp[SparseVector[T], Other, Op] {
      def apply(a: SparseVector[T], b: Other) {
        val result = op(a, b)
        a := result
      }
    }
  }

  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def sv_hv_update[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  :BinaryUpdateOp[SparseVector[T], HashVector[T], Op] = updateFromPureS
}

trait HashVector_GenericOps { this: HashVector.type =>
  import breeze.math.PowImplicits._
  @expand
  implicit def pureFromUpdate[@expand.args(Int, Double, Float, Long, BigInt, Complex) T, Other,Op<:OpType](op: BinaryUpdateOp[HashVector[T], Other, Op])(implicit copy: CanCopy[HashVector[T]]):BinaryOp[HashVector[T], Other, Op, HashVector[T]] = {
    new BinaryOp[HashVector[T], Other, Op, HashVector[T]] {
      override def apply(a : HashVector[T], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def pureFromUpdate[T, Other, Op<:OpType](op: BinaryUpdateOp[HashVector[T], Other, Op])(implicit copy: CanCopy[HashVector[T]]):BinaryOp[HashVector[T], Other, Op, HashVector[T]] = {
    new BinaryOp[HashVector[T], Other, Op, HashVector[T]] {
      override def apply(a : HashVector[T], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def canSet_HV_Generic[V]: BinaryUpdateOp[HashVector[V], V, breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[HashVector[V], V, breeze.linalg.operators.OpSet] {
      def apply(a: HashVector[V], b: V) {
        var i = 0
        while(i < a.length) {
          a(i) = b
          i += 1
        }

      }
    }
  }

  implicit def canSet_HV_HV_Generic[V]: BinaryUpdateOp[HashVector[V], Vector[V], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[HashVector[V], Vector[V], breeze.linalg.operators.OpSet] {
      def apply(a: HashVector[V], b: Vector[V]) {
        require(b.length == a.length, "Vectors must be the same length!")
        var i = 0
         while(i < a.length) {
          a(i) = b(i)
          i += 1
        }
      }
    }
  }

  implicit def canGaxpy[V:Semiring]: CanAxpy[V, HashVector[V], HashVector[V]] = {
    new CanAxpy[V, HashVector[V], HashVector[V]] {
      val ring = implicitly[Semiring[V]]
      def apply(s: V, b: HashVector[V], a: HashVector[V]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (k,v) <- b.activeIterator)
          a(k) = ring.+(a(k),ring.*(s, v))
      }
    }
  }


}