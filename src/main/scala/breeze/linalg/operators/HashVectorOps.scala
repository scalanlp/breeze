package breeze.linalg.operators

import breeze.macros.expand
import breeze.math.{Ring, Semiring, Complex}
import scala.math.BigInt
import breeze.linalg._
import breeze.generic.{UFunc2ZippingImplicits, UFunc}
import breeze.linalg.support.{CanZipMapValues, CanAxpy, CanCopy}
import breeze.generic.UFunc.{UImpl, UImpl2}
import scala.reflect.ClassTag
import breeze.storage.DefaultArrayValue

trait DenseVector_HashVector_Ops { this: HashVector.type =>
  import breeze.math.PowImplicits._



  @expand
  implicit def dv_hv_Update_Zero_Idempotent[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _})
  op: Op.Impl2[T, T, T]):Op.InPlaceImpl2[DenseVector[T], HashVector[T]] = new Op.InPlaceImpl2[DenseVector[T], HashVector[T]] {
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
  implicit def canDot_DV_HV[@expand.args(Int, Double, Float, Long, BigInt, Complex) T](implicit @expand.sequence[T](0, 0.0, 0f, 0l, BigInt(0), Complex.zero) zero: T): breeze.linalg.operators.OpMulInner.Impl2[DenseVector[T], HashVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[DenseVector[T], HashVector[T], T] {
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
  op: Op.Impl2[T, T, T]):Op.InPlaceImpl2[HashVector[T], DenseVector[T]] = new Op.InPlaceImpl2[HashVector[T], DenseVector[T]] {
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
  op: Op.Impl2[T, T, T]):Op.Impl2[HashVector[T], DenseVector[T], DenseVector[T]] = {
    new Op.Impl2[HashVector[T], DenseVector[T], DenseVector[T]] {
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
  implicit def canDot_HV_DV[@expand.args(Int, Float, Double, Long, BigInt, Complex) T]: breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], DenseVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], DenseVector[T], T] {
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
  op: Op.Impl2[T, T, T]):Op.Impl2[HashVector[T], HashVector[T], HashVector[T]] = new Op.Impl2[HashVector[T], HashVector[T], HashVector[T]] {
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
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T):OpMulScalar.Impl2[HashVector[T], HashVector[T], HashVector[T]] = new OpMulScalar.Impl2[HashVector[T], HashVector[T], HashVector[T]] {
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
  op: Op.Impl2[T, T, T]):Op.Impl2[HashVector[T], HashVector[T], HashVector[T]] = new Op.Impl2[HashVector[T], HashVector[T], HashVector[T]] {
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
  op: Op.Impl2[T, T, T]):Op.Impl2[HashVector[T], Vector[T], HashVector[T]] = new Op.Impl2[HashVector[T], Vector[T], HashVector[T]] {
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
  op: Op.Impl2[T, T, T],
   @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero)
   zero: T):Op.Impl2[HashVector[T], T, HashVector[T]] = new Op.Impl2[HashVector[T], T, HashVector[T]] {
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
  op: Op.Impl2[T, T, T]):Op.InPlaceImpl2[HashVector[T], HashVector[T]] = new Op.InPlaceImpl2[HashVector[T], HashVector[T]] {
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
  op: Op.Impl2[T, T, T]):Op.InPlaceImpl2[HashVector[T], HashVector[T]] = new Op.InPlaceImpl2[HashVector[T], HashVector[T]] {
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
  op: Op.Impl2[T, T, T]):Op.InPlaceImpl2[HashVector[T], T] = new Op.InPlaceImpl2[HashVector[T], T] {
    def apply(a: HashVector[T], b: T):Unit = {
      var i = 0
      while(i < a.length) {
        a(i) = op(a(i), b)
        i += 1
      }
    }
  }



  @expand
  implicit def canDot_HV_HV[@expand.args(Int, Long, BigInt, Complex, Double, Float) T](implicit @expand.sequence[T](0, 0l, BigInt(0), Complex.zero, 0.0, 0f) zero: T): breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], HashVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], HashVector[T], T] {
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

  @expand
  @expand.valify
  implicit def canNorm[@expand.args(Int, Double, Float, Long, BigInt, Complex) T]: norm.Impl2[HashVector[T], Double, Double] = {

    new norm.Impl2[HashVector[T], Double, Double] {
      def apply(v: HashVector[T], n: Double): Double = {
        import v._
        if (n == 1) {
          var sum = 0.0
          activeValuesIterator foreach (v => sum += v.abs.toDouble )
          sum
        } else if (n == 2) {
          var sum = 0.0
          activeValuesIterator  foreach (v => { val nn = v.abs.toDouble; sum += nn * nn })
          math.sqrt(sum)
        } else if (n == Double.PositiveInfinity) {
          var max = 0.0
          activeValuesIterator foreach (v => { val nn = v.abs.toDouble; if (nn > max) max = nn })
          max
        } else {
          var sum = 0.0
          activeValuesIterator foreach (v => { val nn = v.abs.toDouble; sum += math.pow(nn,n) })
          math.pow(sum, 1.0 / n)
        }
      }
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
  op: Op.Impl2[T, T, T],
   @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T):Op.Impl2[HashVector[T], SparseVector[T], HashVector[T]] = new Op.Impl2[HashVector[T], SparseVector[T], HashVector[T]] {
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
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T):OpMulScalar.Impl2[HashVector[T], SparseVector[T], HashVector[T]] = new OpMulScalar.Impl2[HashVector[T], SparseVector[T], HashVector[T]] {
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
  op: Op.Impl2[T, T, T]):Op.Impl2[HashVector[T], SparseVector[T], HashVector[T]] = new Op.Impl2[HashVector[T], SparseVector[T], HashVector[T]] {
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
  implicit def canDot_HV_SV[@expand.args(Int, Long, BigInt, Complex, Float, Double) T](implicit @expand.sequence[T](0, 0l, BigInt(0), Complex.zero, 0f, 0.0) zero: T): breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], SparseVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], SparseVector[T], T] {
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



  protected def updateFromPure[T, Op, Other](implicit op: UFunc.UImpl2[Op, HashVector[T], Other, HashVector[T]],
                                             set: OpSet.InPlaceImpl2[HashVector[T], HashVector[T]]): UFunc.InPlaceImpl2[Op, HashVector[T], Other] = {
    new UFunc.InPlaceImpl2[Op, HashVector[T], Other] {
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
  :Op.InPlaceImpl2[HashVector[T], SparseVector[T]] = updateFromPure



}


trait SparseVector_HashVector_Ops extends HashVectorOps with HashVector_SparseVector_Ops { this: HashVector.type =>
  import breeze.math.PowImplicits._


  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def sv_hv_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T],
   @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T):Op.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] = new Op.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] {
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
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T):OpMulScalar.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] = new OpMulScalar.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] {
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
  op: Op.Impl2[T, T, T]):Op.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] = new Op.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] {
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




  implicit def canDot_SV_HV[T](implicit op: OpMulInner.Impl2[HashVector[T], SparseVector[T], T]): breeze.linalg.operators.OpMulInner.Impl2[SparseVector[T], HashVector[T], T] = {
    new breeze.linalg.operators.OpMulInner.Impl2[SparseVector[T], HashVector[T], T] {
      def apply(a: SparseVector[T], b: HashVector[T]) = {
        b dot a
      }
    }
  }

  protected def updateFromPureS[T, Op, Other](implicit op: UFunc.UImpl2[Op, SparseVector[T], Other, SparseVector[T]],
                                              set: OpSet.InPlaceImpl2[SparseVector[T], SparseVector[T]]): UFunc.InPlaceImpl2[Op, SparseVector[T], Other] = {
    new UFunc.InPlaceImpl2[Op, SparseVector[T], Other] {
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
  :Op.InPlaceImpl2[SparseVector[T], HashVector[T]] = updateFromPureS
}

trait HashVector_GenericOps extends UFunc2ZippingImplicits[HashVector]{ this: HashVector.type =>
  import breeze.math.PowImplicits._
  @expand
  implicit def pureFromUpdate[@expand.args(Int, Double, Float, Long, BigInt, Complex) T, Other,Op<:OpType](op: UFunc.InPlaceImpl2[Op, HashVector[T], Other])(implicit copy: CanCopy[HashVector[T]]):UFunc.UImpl2[Op, HashVector[T], Other, HashVector[T]] = {
    new UImpl2[Op, HashVector[T], Other, HashVector[T]] {
      override def apply(a : HashVector[T], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def pureFromUpdate[T, Other,Op<:OpType](op: UFunc.InPlaceImpl2[Op, HashVector[T], Other])(implicit copy: CanCopy[HashVector[T]]):UFunc.UImpl2[Op, HashVector[T], Other, HashVector[T]] = {
    new UFunc.UImpl2[Op, HashVector[T], Other, HashVector[T]] {
      override def apply(a : HashVector[T], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def canSet_HV_Generic[V]: OpSet.InPlaceImpl2[HashVector[V], V] = {
    new OpSet.InPlaceImpl2[HashVector[V], V] {
      def apply(a: HashVector[V], b: V) {
        var i = 0
        while(i < a.length) {
          a(i) = b
          i += 1
        }

      }
    }
  }

  implicit def canSet_HV_HV_Generic[V]: OpSet.InPlaceImpl2[HashVector[V], Vector[V]] = {
    new OpSet.InPlaceImpl2[HashVector[V], Vector[V]] {
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
  implicit val zipMap_d: CanZipMapValuesHashVector[Double, Double] = new CanZipMapValuesHashVector[Double, Double]
  implicit val zipMap_f: CanZipMapValuesHashVector[Float, Float] = new CanZipMapValuesHashVector[Float, Float]
  implicit val zipMap_i: CanZipMapValuesHashVector[Int, Int] = new CanZipMapValuesHashVector[Int, Int]


  implicit def negFromScale[V](implicit scale: OpMulScalar.Impl2[HashVector[V], V, HashVector[V]], field: Ring[V]): OpNeg.Impl[HashVector[V], HashVector[V]] = {
    new OpNeg.Impl[HashVector[V], HashVector[V]] {
      override def apply(a : HashVector[V]) = {
        scale(a, field.negate(field.one))
      }
    }
  }

}
