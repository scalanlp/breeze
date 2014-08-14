package breeze.linalg.operators

import breeze.linalg._
import breeze.linalg.support.{CanZipMapValues, CanCopy}
import breeze.generic.{UFunc}
import breeze.generic.UFunc.{UImpl, UImpl2}
import breeze.macros.expand
import breeze.math.{Field, Ring, Semiring}
import breeze.storage.Zero

import scala.{specialized=>spec}
import scala.reflect.ClassTag

trait DenseVector_HashVector_Ops { this: HashVector.type =>
  import breeze.math.PowImplicits._



  @expand
  implicit def dv_hv_Update_Zero_Idempotent[@expand.args(Int, Double, Float, Long) T,
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
  implicit def canDot_DV_HV[@expand.args(Int, Double, Float, Long) T](implicit @expand.sequence[T](0, 0.0, 0f, 0l) zero: T): breeze.linalg.operators.OpMulInner.Impl2[DenseVector[T], HashVector[T], T] = {
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
  implicit def hv_dv_UpdateOp[@expand.args(Int, Double, Float, Long) T,
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
  implicit def hv_dv_op[@expand.args(Int, Double, Float, Long) T,
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
  implicit def canDot_HV_DV[@expand.args(Int, Float, Double, Long) T]: breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], DenseVector[T], T] = {
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
  implicit def hv_hv_Idempotent_Op[@expand.args(Int, Double, Float, Long) T,
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
  implicit def hv_hv_nilpotent_Op[@expand.args(Int, Double, Float, Long) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l) zero: T):OpMulScalar.Impl2[HashVector[T], HashVector[T], HashVector[T]] = new OpMulScalar.Impl2[HashVector[T], HashVector[T], HashVector[T]] {
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
  implicit def hv_hv_Op[@expand.args(Int, Double, Float, Long) T,
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
  implicit def hv_v_Op[@expand.args(Int, Double, Float, Long) T,
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
  implicit def hv_s_Op[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpMulMatrix, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ * _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T],
   @expand.sequence[T](0, 0.0, 0.0f, 0l)
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
  implicit def hv_hv_UpdateOp[@expand.args(Int, Double, Float, Long) T,
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
  implicit def hv_hv_Idempotent_UpdateOp[@expand.args(Int, Double, Float, Long) T,
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
  implicit def hv_s_UpdateOp[@expand.args(Int, Double, Float, Long) T,
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
  implicit def canDot_HV_HV[@expand.args(Int, Long, Double, Float) T](implicit @expand.sequence[T](0, 0l, 0.0, 0f) zero: T): breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], HashVector[T], T] = {
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
  implicit def canNorm[@expand.args(Int, Double, Float, Long) T]: norm.Impl2[HashVector[T], Double, Double] = {

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

  implicit def canNorm[T:Field:ClassTag]: norm.Impl2[HashVector[T], Double, Double] = {

    new norm.Impl2[HashVector[T], Double, Double] {
      val f = implicitly[Field[T]]
      def apply(v: HashVector[T], n: Double): Double = {
        import v._
        if (n == 1) {
          var sum = 0.0
          activeValuesIterator foreach (v => sum += f.sNorm(v) )
          sum
        } else if (n == 2) {
          var sum = 0.0
          activeValuesIterator  foreach (v => { val nn = f.sNorm(v); sum += nn * nn })
          math.sqrt(sum)
        } else if (n == Double.PositiveInfinity) {
          var max = 0.0
          activeValuesIterator foreach (v => { val nn = f.sNorm(v); if (nn > max) max = nn })
          max
        } else {
          var sum = 0.0
          activeValuesIterator foreach (v => { val nn = f.sNorm(v); sum += math.pow(nn,n) })
          math.pow(sum, 1.0 / n)
        }
      }
    }
  }

}



trait HashVector_SparseVector_Ops extends HashVectorOps { this: HashVector.type =>
  import breeze.math.PowImplicits._


  @expand
  implicit def hv_sv_Op[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T],
   @expand.sequence[T](0, 0.0, 0.0f, 0l) zero: T):Op.Impl2[HashVector[T], SparseVector[T], HashVector[T]] = new Op.Impl2[HashVector[T], SparseVector[T], HashVector[T]] {
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
  implicit def hv_sv_nilpotent_Op[@expand.args(Int, Double, Float, Long) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l) zero: T):OpMulScalar.Impl2[HashVector[T], SparseVector[T], HashVector[T]] = new OpMulScalar.Impl2[HashVector[T], SparseVector[T], HashVector[T]] {
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
  implicit def hv_sv_Idempotent_Op[@expand.args(Int, Double, Float, Long) T,
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
  implicit def canDot_HV_SV[@expand.args(Int, Long, Float, Double) T](implicit @expand.sequence[T](0, 0l, 0f, 0.0) zero: T): breeze.linalg.operators.OpMulInner.Impl2[HashVector[T], SparseVector[T], T] = {
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
  implicit def hv_sv_UpdateOp[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  :Op.InPlaceImpl2[HashVector[T], SparseVector[T]] = updateFromPure



}


trait SparseVector_HashVector_Ops extends HashVectorOps with HashVector_SparseVector_Ops { this: HashVector.type =>
  import breeze.math.PowImplicits._


  @expand
  implicit def sv_hv_Op[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: Op.Impl2[T, T, T],
   @expand.sequence[T](0, 0.0, 0.0f, 0l) zero: T):Op.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] = new Op.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] {
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
  implicit def sv_hv_nilpotent_Op[@expand.args(Int, Double, Float, Long) T]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l) zero: T):OpMulScalar.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] = new OpMulScalar.Impl2[SparseVector[T], HashVector[T], SparseVector[T]] {
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
  implicit def sv_hv_Idempotent_Op[@expand.args(Int, Double, Float, Long) T,
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
  implicit def sv_hv_update[@expand.args(Int, Double, Float, Long) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  :Op.InPlaceImpl2[SparseVector[T], HashVector[T]] = updateFromPureS
}

trait HashVector_GenericOps { this: HashVector.type =>
  import breeze.math.PowImplicits._
  @expand
  implicit def pureFromUpdate[@expand.args(Int, Double, Float, Long) T, Other,Op<:OpType](op: UFunc.InPlaceImpl2[Op, HashVector[T], Other])(implicit copy: CanCopy[HashVector[T]]):UFunc.UImpl2[Op, HashVector[T], Other, HashVector[T]] = {
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

  implicit def canGaxpy[V:Semiring]: scaleAdd.InPlaceImpl3[HashVector[V], V, HashVector[V]] = {
    new scaleAdd.InPlaceImpl3[HashVector[V], V, HashVector[V]] {
      val ring = implicitly[Semiring[V]]
      def apply(a: HashVector[V], s: V, b: HashVector[V]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (k,v) <- b.activeIterator)
          a(k) = ring.+(a(k),ring.*(s, v))
      }
    }
  }

  class CanZipMapValuesHashVector[@spec(Double, Int, Float, Long) V, @spec(Int, Double) RV:ClassTag:Zero]
    extends CanZipMapValues[HashVector[V],V,RV,HashVector[RV]] {

    def create(length : Int) = zeros(length)

    /**Maps all corresponding values from the two collections. */
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
  implicit def zipMap[V, R:ClassTag:Zero] = new CanZipMapValuesHashVector[V, R]
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

  implicit def vAddIntoField[T](implicit field: Field[T], ct: ClassTag[T]):OpAdd.InPlaceImpl2[HashVector[T], HashVector[T]] = {
    new OpAdd.InPlaceImpl2[HashVector[T], HashVector[T]] {
      override def apply(v: HashVector[T], v2: HashVector[T]) = {
        for(i <- 0 until v.length) v(i) = field.+(v(i), v2(i))
      }
    }

  }

  implicit def vSubIntoField[T](implicit field: Field[T], ct: ClassTag[T]):OpSub.InPlaceImpl2[HashVector[T], HashVector[T]] = {
    new OpSub.InPlaceImpl2[HashVector[T], HashVector[T]] {
      override def apply(v: HashVector[T], v2: HashVector[T]) = {
        for(i <- 0 until v.length) v(i) = field.-(v(i), v2(i))
      }
    }

  }

  implicit def vMulIntoField[T](implicit field: Field[T], ct: ClassTag[T]):OpMulScalar.InPlaceImpl2[HashVector[T], HashVector[T]] = {
    new OpMulScalar.InPlaceImpl2[HashVector[T], HashVector[T]] {
      override def apply(v: HashVector[T], v2: HashVector[T]) = {
        for(i <- 0 until v.length) v(i) = field.*(v(i), v2(i))
      }
    }

  }

  implicit def vDivIntoField[T](implicit field: Field[T], ct: ClassTag[T]):OpDiv.InPlaceImpl2[HashVector[T], HashVector[T]] = {
    new OpDiv.InPlaceImpl2[HashVector[T], HashVector[T]] {
      override def apply(v: HashVector[T], v2: HashVector[T]) = {
        for(i <- 0 until v.length) v(i) = field./(v(i), v2(i))
      }
    }

  }


  implicit def vPowInto[T](implicit pow: OpPow.Impl2[T, T, T], ct: ClassTag[T]):OpPow.InPlaceImpl2[HashVector[T], HashVector[T]] = {
    new OpPow.InPlaceImpl2[HashVector[T], HashVector[T]] {
      override def apply(v: HashVector[T], v2: HashVector[T]) = {
        for(i <- 0 until v.length) v(i) = pow(v(i), v2(i))
      }
    }

  }

  implicit def vAddIntoSField[T](implicit field: Semiring[T], ct: ClassTag[T]):OpAdd.InPlaceImpl2[HashVector[T], T] = {
    new OpAdd.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T) = {
        for(i <- 0 until v.length) v(i) = field.+(v(i), v2)
      }
    }

  }

  implicit def vAddSField[T](implicit field: Semiring[T], ct: ClassTag[T]):OpAdd.Impl2[HashVector[T], T, HashVector[T]] = {
    binaryOpFromUpdateOp(implicitly[CanCopy[HashVector[T]]], vAddIntoSField, ct)
  }
  implicit def vSubSField[T](implicit field: Ring[T], ct: ClassTag[T]):OpSub.Impl2[HashVector[T], T, HashVector[T]]  = binaryOpFromUpdateOp(implicitly[CanCopy[HashVector[T]]], vSubIntoSField, ct)
  implicit def vMulScalarSField[T](implicit field: Semiring[T], ct: ClassTag[T]):OpMulScalar.Impl2[HashVector[T], T, HashVector[T]]  = binaryOpFromUpdateOp(implicitly[CanCopy[HashVector[T]]], vMulScalarIntoSField, ct)
  implicit def vDivSField[T](implicit field: Field[T], ct: ClassTag[T]):OpDiv.Impl2[HashVector[T], T, HashVector[T]]  = binaryOpFromUpdateOp(implicitly[CanCopy[HashVector[T]]], vDivIntoSField, ct)
  implicit def vPowS[T](implicit pow: OpPow.Impl2[T, T, T], ct: ClassTag[T], zero: Zero[T]):OpPow.Impl2[HashVector[T], T, HashVector[T]]  = binaryOpFromUpdateOp(implicitly[CanCopy[HashVector[T]]], vPowIntoS, ct)


  implicit def vSubIntoSField[T](implicit field: Ring[T], ct: ClassTag[T]):OpSub.InPlaceImpl2[HashVector[T], T] = {
    new OpSub.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T) = {
        for(i <- 0 until v.length) v(i) = field.-(v(i), v2)
      }
    }

  }


  implicit def vMulScalarIntoSField[T](implicit field: Semiring[T], ct: ClassTag[T]):OpMulScalar.InPlaceImpl2[HashVector[T], T] = {
    new OpMulScalar.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T) = {
        for(i <- 0 until v.length) v(i) = field.*(v(i), v2)
      }
    }
  }

  implicit def vDivIntoSField[T](implicit field: Field[T], ct: ClassTag[T]):OpDiv.InPlaceImpl2[HashVector[T], T] = {
    new OpDiv.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T) = {
        for(i <- 0 until v.length) v(i) = field./(v(i), v2)
      }
    }
  }

  implicit def vPowIntoS[T](implicit pow: OpPow.Impl2[T, T, T], ct: ClassTag[T]):OpPow.InPlaceImpl2[HashVector[T], T] = {
    new OpPow.InPlaceImpl2[HashVector[T], T] {
      override def apply(v: HashVector[T], v2: T) = {
        for(i <- 0 until v.length) v(i) = pow(v(i), v2)
      }
    }
  }

  implicit def dotField[T](implicit field: Semiring[T]):OpMulInner.Impl2[HashVector[T], HashVector[T], T] = {
    new OpMulInner.Impl2[HashVector[T], HashVector[T], T] {
      override def apply(v: HashVector[T], v2: HashVector[T]): T = {
        var acc = field.zero
        for(i <- 0 until v.length) {
          acc = field.+(acc, field.*(v(i), v2(i)))
        }
        acc
      }
    }
  }


  def binaryOpFromUpdateOp[Op<:OpType, V, Other]
  (implicit copy: CanCopy[HashVector[V]], op: UFunc.InPlaceImpl2[Op, HashVector[V], Other], man: ClassTag[V]):
  UFunc.UImpl2[Op, HashVector[V], Other, HashVector[V]] = {
    new UFunc.UImpl2[Op, HashVector[V], Other, HashVector[V]] {
      override def apply(a : HashVector[V], b : Other): HashVector[V] = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def implOpSet_V_V_InPlace[V]: OpSet.InPlaceImpl2[HashVector[V], HashVector[V]] = {

    new OpSet.InPlaceImpl2[HashVector[V], HashVector[V]] {
      def apply(a: HashVector[V], b: HashVector[V]): Unit = {
        require(b.length == a.length, "HashVectors must be the same length!")

        for (i <- 0 until a.length) {
          a(i) = b(i)
        }


      }
    }
  }

  /**Returns the k-norm of this HashVector. */
  implicit def canNorm[T](implicit canNormS: norm.Impl[T, Double]): norm.Impl2[HashVector[T], Double, Double] = {

    new norm.Impl2[HashVector[T], Double, Double] {
      def apply(v: HashVector[T], n: Double): Double = {
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

}
