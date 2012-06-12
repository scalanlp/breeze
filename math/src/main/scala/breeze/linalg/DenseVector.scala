package breeze.linalg

import operators._
import scala.{specialized=>spec}
import breeze.storage.DenseStorage
import breeze.util.ArrayUtil
import support.{CanSlice, CanMapValues, CanCopy}
import breeze.math.Field
import breeze.numerics.IntMath
import java.util.Arrays

/**
 *
 * @author dlwh
 */
final class DenseVector[@spec(Double, Int, Float, Long) E](val data: Array[E],
                                                         val offset: Int,
                                                         val stride: Int,
                                                         val length: Int) extends Vector[E] with DenseStorage[E] with VectorLike[E, DenseVector[E]] {
  def this(data: Array[E]) = this(data, 0, 1, data.length)
  def repr = this

  def activeIterator = iterator

  def activeValuesIterator = valuesIterator

  def activeKeysIterator = keysIterator

  override def equals(p1: Any) = p1 match {
    case x: DenseVector[_] =>
//      length == x.length && (( stride == x.stride
//        && offset == x.offset
//        && data.length == x.data.length
//        && ArrayUtil.equals(data, x.data)
//      )  ||  (
          valuesIterator sameElements x.valuesIterator
//        ))

    case _ => false
  }

  override def toString = {
    valuesIterator.mkString("DenseVector(",", ", ")")
  }
}



object DenseVector extends VectorConstructors[DenseVector]
                      with DenseVectorDefaultOperations_Int
                      with DenseVectorDefaultOperations_Float
                      with DenseVectorDefaultOperations_Double {
  def zeros[@spec(Double, Float, Int) V: ClassManifest](size: Int) = apply(new Array[V](size))
  def apply[@spec(Double, Float, Int) V](values: Array[V]) = new DenseVector(values)

  implicit val canAddIntoD: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpAdd] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpAdd] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        org.netlib.blas.Daxpy.daxpy(
          a.length, 1.0, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      setParent(Vector.canAddIntoD)
    }

  }

  implicit val canSubIntoD: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSub] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSub] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        org.netlib.blas.Daxpy.daxpy(
          a.length, -1.0, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      setParent(Vector.canSubIntoD)
    }

  }

  implicit val canDotD: BinaryOp[DenseVector[Double], DenseVector[Double], OpMulInner, Double] = {
    new BinaryOp[DenseVector[Double], DenseVector[Double], OpMulInner, Double] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        org.netlib.blas.Ddot.ddot(
          a.length, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      Vector.canDotD.registerBinary(this)
    }

  }

  implicit val canScaleD: BinaryUpdateOp[DenseVector[Double], Double, OpMulScalar] = {
    new BinaryUpdateOp[DenseVector[Double], Double, OpMulScalar] {
      def apply(a: DenseVector[Double], b: Double) = {
        org.netlib.blas.Dscal.dscal(
          a.length, b, a.data, a.offset, a.stride)
      }
      setParent(Vector.canScaleD)
    }

  }

  implicit val canSetD:BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSet] = new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSet] {
    def apply(a: DenseVector[Double], b: DenseVector[Double]) {
      org.netlib.blas.Dcopy.dcopy(
        a.length, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
    }
  }

  implicit def canCopyDenseVector[@specialized V:ClassManifest] = new CanCopy[DenseVector[V]] {
    def apply(v1: DenseVector[V]) = {
      new DenseVector(Array.tabulate(v1.length)(i => v1(i)))
    }
  }

  implicit def binaryOpFromBinaryUpdateOp[V, Other, Op<:OpType](implicit copy: CanCopy[DenseVector[V]], op: BinaryUpdateOp[DenseVector[V], Other, Op], man: ClassManifest[V]) = {
    new BinaryOp[DenseVector[V], Other, Op, DenseVector[V]] {
      override def apply(a : DenseVector[V], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def negFromScale[@specialized V, Double](implicit scale: BinaryOp[DenseVector[V], V, OpMulScalar, DenseVector[V]], field: Field[V]) = {
    new UnaryOp[DenseVector[V], OpNeg, DenseVector[V]] {
      override def apply(a : DenseVector[V]) = {
        scale(a, field.-(field.zero, field.one))
      }
    }
  }

  implicit def canMapValues[V, V2](implicit man: ClassManifest[V2]):CanMapValues[DenseVector[V], V, V2, DenseVector[V2]] = {
    new CanMapValues[DenseVector[V], V, V2, DenseVector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def map(from: DenseVector[V], fn: (V) => V2) = {
        DenseVector.tabulate(from.length)(i => fn(from(i)))
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: DenseVector[V], fn: (V) => V2) = {
        map(from, fn)
      }
    }
  }

  // slicing
  implicit def canSlice[V]: CanSlice[DenseVector[V], Range, DenseVector[V]] = {
    new CanSlice[DenseVector[V], Range, DenseVector[V]] {
      def apply(v: DenseVector[V], r: Range) = {
        require(r.isEmpty || r.last < v.length)
        require(r.isEmpty || r.head >= 0)
        new DenseVector(v.data, offset = v.offset + r.start, stride = v.stride * r.step, length = r.length)
      }
    }
  }


  // concatenation
  /**
   * Horizontal concatenation of two or more row vectors into one matrix.
   * @throws IllegalArgumentException if vectors have different sizes
  def horzcat[V: ClassManifest](vectors: DenseVector[V]*): DenseMatrix[V] = {
    val size = vectors.head.size
    if (!(vectors forall (_.size == size)))
      throw new IllegalArgumentException("All vectors must have the same size!")
    val result = DenseMatrix.zeros[V](vectors.size, size)
    for ((v, col) <- vectors zip (0 until vectors.size))
      result(::, col) := v
    result
  }
   */

  /**
   * Vertical concatenation of two or more column vectors into one large vector.
   */
  def vertcat[V](vectors: DenseVector[V]*)(implicit canSet: BinaryUpdateOp[DenseVector[V], DenseVector[V], OpSet], vman: ClassManifest[V]): DenseVector[V] = {
    val size = vectors.foldLeft(0)(_ + _.size)
    val result = zeros[V](size)
    var offset = 0
    for (v <- vectors) {
      result(offset until (offset + v.size)) := v
      offset += v.size
    }
    result
  }

  implicit def canTranspose[V]: CanTranspose[DenseVector[V], DenseMatrix[V]] = {
    new CanTranspose[DenseVector[V], DenseMatrix[V]] {
      def apply(from: DenseVector[V]) = {
        new DenseMatrix(from.data, offset = from.offset, cols = from.length, rows = 1, majorStride = from.stride)
      }
    }
  }
}

// This could be regarded as code gen, but it's just copy paste for now.
trait DenseVectorDefaultOperations_Int { this : DenseVector.type =>
  type V = Int

  implicit val canAddIntoVV_I: BinaryUpdateOp[DenseVector[V], DenseVector[V], OpAdd] = {
    new BinaryUpdateOp[DenseVector[V], DenseVector[V], OpAdd] {
      def apply(a: DenseVector[V], b: DenseVector[V]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) += bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canAddIntoVS_I: BinaryUpdateOp[DenseVector[V], V, OpAdd] = {
    new BinaryUpdateOp[DenseVector[V], V, OpAdd] {
      def apply(a: DenseVector[V], b: V) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) += b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canSubIntoVV_I: BinaryUpdateOp[DenseVector[V], DenseVector[V], OpSub] = {
    new BinaryUpdateOp[DenseVector[V], DenseVector[V], OpSub] {
      def apply(a: DenseVector[V], b: DenseVector[V]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) -= bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canSubIntoVS_I: BinaryUpdateOp[DenseVector[V], V, OpSub] = {
    new BinaryUpdateOp[DenseVector[V], V, OpSub] {
      def apply(a: DenseVector[V], b: V) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) -= b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canDotV_I: BinaryOp[DenseVector[V], DenseVector[V], OpMulInner, V] = {
    new BinaryOp[DenseVector[V], DenseVector[V], OpMulInner, V] {
      def apply(a: DenseVector[V], b: DenseVector[V]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        var accum: V = 0
        while(i < a.length) {
          accum += ad(aoff) * bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        accum
      }
    }

  }

  implicit val canMulIntoVV_I: BinaryUpdateOp[DenseVector[V], DenseVector[V], OpMulScalar] = {
    new BinaryUpdateOp[DenseVector[V], DenseVector[V], OpMulScalar] {
      def apply(a: DenseVector[V], b: DenseVector[V]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) *= bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canMulIntoVS_I: BinaryUpdateOp[DenseVector[V], V, OpMulScalar] = {
    new BinaryUpdateOp[DenseVector[V], V, OpMulScalar] {
      def apply(a: DenseVector[V], b: V) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) *= b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canDivIntoVV_I: BinaryUpdateOp[DenseVector[V], DenseVector[V], OpDiv] = {
    new BinaryUpdateOp[DenseVector[V], DenseVector[V], OpDiv] {
      def apply(a: DenseVector[V], b: DenseVector[V]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) /= bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canDivIntoVS_I: BinaryUpdateOp[DenseVector[V], V, OpDiv] = {
    new BinaryUpdateOp[DenseVector[V], V, OpDiv] {
      def apply(a: DenseVector[V], b: V) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) /= b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canSetIntoVV_I: BinaryUpdateOp[DenseVector[V], DenseVector[V], OpSet] = {
    new BinaryUpdateOp[DenseVector[V], DenseVector[V], OpSet] {
      def apply(a: DenseVector[V], b: DenseVector[V]) {
        require(a.length == b.length, "Vectors must have same length")
        if(a.stride == 1 && b.stride == 1) {
          System.arraycopy(b.data, b.offset, a.data, b.offset, b.length)
          return
        }
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) = bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canSetIntoVS_I: BinaryUpdateOp[DenseVector[V], V, OpSet] = {
    new BinaryUpdateOp[DenseVector[V], V, OpSet] {
      def apply(a: DenseVector[V], b: V) {
        if (a.stride == 1) {
          Arrays.fill(a.data, a.offset, a.length + a.offset, b)
          return
        }
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canPowIntoVV_I: BinaryUpdateOp[DenseVector[V], DenseVector[V], OpPow] = {
    new BinaryUpdateOp[DenseVector[V], DenseVector[V], OpPow] {
      def apply(a: DenseVector[V], b: DenseVector[V]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) = IntMath.ipow(ad(aoff), bd(boff))
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canPowIntoVS_I: BinaryUpdateOp[DenseVector[V], V, OpPow] = {
    new BinaryUpdateOp[DenseVector[V], V, OpPow] {
      def apply(a: DenseVector[V], b: V) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = IntMath.ipow(ad(aoff), b)
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canModIntoVV_I: BinaryUpdateOp[DenseVector[V], DenseVector[V], OpMod] = {
    new BinaryUpdateOp[DenseVector[V], DenseVector[V], OpMod] {
      def apply(a: DenseVector[V], b: DenseVector[V]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) %= bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canModIntoVS_I: BinaryUpdateOp[DenseVector[V], V, OpMod] = {
    new BinaryUpdateOp[DenseVector[V], V, OpMod] {
      def apply(a: DenseVector[V], b: V) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) %= b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

}


// This could be regarded as code gen, but it's just copy paste for now.
trait DenseVectorDefaultOperations_Float { this : DenseVector.type =>

  implicit val canAddIntoVV_F: BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpAdd] = {
    new BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpAdd] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) += bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canAddIntoVS_F: BinaryUpdateOp[DenseVector[Float], Float, OpAdd] = {
    new BinaryUpdateOp[DenseVector[Float], Float, OpAdd] {
      def apply(a: DenseVector[Float], b: Float) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) += b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canSubIntoVV_F: BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpSub] = {
    new BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpSub] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) -= bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canSubIntoVS_F: BinaryUpdateOp[DenseVector[Float], Float, OpSub] = {
    new BinaryUpdateOp[DenseVector[Float], Float, OpSub] {
      def apply(a: DenseVector[Float], b: Float) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) -= b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canDotV_F: BinaryOp[DenseVector[Float], DenseVector[Float], OpMulInner, Float] = {
    new BinaryOp[DenseVector[Float], DenseVector[Float], OpMulInner, Float] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        var accum: Float = 0
        while(i < a.length) {
          accum += ad(aoff) * bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        accum
      }
    }

  }

  implicit val canMulIntoVV_F: BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpMulScalar] = {
    new BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpMulScalar] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) *= bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canMulIntoVS_F: BinaryUpdateOp[DenseVector[Float], Float, OpMulScalar] = {
    new BinaryUpdateOp[DenseVector[Float], Float, OpMulScalar] {
      def apply(a: DenseVector[Float], b: Float) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) *= b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canDivIntoVV_F: BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpDiv] = {
    new BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpDiv] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) /= bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canDivIntoVS_F: BinaryUpdateOp[DenseVector[Float], Float, OpDiv] = {
    new BinaryUpdateOp[DenseVector[Float], Float, OpDiv] {
      def apply(a: DenseVector[Float], b: Float) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) /= b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canSetIntoVV_F: BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpSet] = {
    new BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpSet] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) {
        require(a.length == b.length, "Vectors must have same length")
        if(a.stride == 1 && b.stride == 1) {
          System.arraycopy(b.data, b.offset, a.data, b.offset, b.length)
          return
        }
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) = bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canSetIntoVS_F: BinaryUpdateOp[DenseVector[Float], Float, OpSet] = {
    new BinaryUpdateOp[DenseVector[Float], Float, OpSet] {
      def apply(a: DenseVector[Float], b: Float) {
        if (a.stride == 1) {
          Arrays.fill(a.data, a.offset, a.length + a.offset, b)
          return
        }
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canPowIntoVV_F: BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpPow] = {
    new BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpPow] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) = math.pow(ad(aoff), bd(boff)).toFloat
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canPowIntoVS_F: BinaryUpdateOp[DenseVector[Float], Float, OpPow] = {
    new BinaryUpdateOp[DenseVector[Float], Float, OpPow] {
      def apply(a: DenseVector[Float], b: Float) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = math.pow(ad(aoff), b).toFloat
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canModIntoVV_F: BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpMod] = {
    new BinaryUpdateOp[DenseVector[Float], DenseVector[Float], OpMod] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) %= bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canModIntoVS_F: BinaryUpdateOp[DenseVector[Float], Float, OpMod] = {
    new BinaryUpdateOp[DenseVector[Float], Float, OpMod] {
      def apply(a: DenseVector[Float], b: Float) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) %= b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

}

// This could be regarded as code gen, but it's just copy paste for now.
trait DenseVectorDefaultOperations_Double { this : DenseVector.type =>

  implicit val canAddIntoVV_D: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpAdd] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpAdd] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) += bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canAddIntoVS_D: BinaryUpdateOp[DenseVector[Double], Double, OpAdd] = {
    new BinaryUpdateOp[DenseVector[Double], Double, OpAdd] {
      def apply(a: DenseVector[Double], b: Double) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) += b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canSubIntoVV_D: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSub] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSub] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) -= bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canSubIntoVS_D: BinaryUpdateOp[DenseVector[Double], Double, OpSub] = {
    new BinaryUpdateOp[DenseVector[Double], Double, OpSub] {
      def apply(a: DenseVector[Double], b: Double) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) -= b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canDotV_D: BinaryOp[DenseVector[Double], DenseVector[Double], OpMulInner, Double] = {
    new BinaryOp[DenseVector[Double], DenseVector[Double], OpMulInner, Double] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        var accum: Double = 0
        while(i < a.length) {
          accum += ad(aoff) * bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        accum
      }
    }

  }

  implicit val canMulIntoVV_D: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpMulScalar] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpMulScalar] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) *= bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canMulIntoVS_D: BinaryUpdateOp[DenseVector[Double], Double, OpMulScalar] = {
    new BinaryUpdateOp[DenseVector[Double], Double, OpMulScalar] {
      def apply(a: DenseVector[Double], b: Double) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) *= b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canDivIntoVV_D: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpDiv] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpDiv] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) /= bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canDivIntoVS_D: BinaryUpdateOp[DenseVector[Double], Double, OpDiv] = {
    new BinaryUpdateOp[DenseVector[Double], Double, OpDiv] {
      def apply(a: DenseVector[Double], b: Double) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) /= b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canSetIntoVV_D: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSet] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSet] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) {
        require(a.length == b.length, "Vectors must have same length")
        if(a.stride == 1 && b.stride == 1) {
          System.arraycopy(b.data, b.offset, a.data, b.offset, b.length)
          return
        }
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) = bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canSetIntoVS_D: BinaryUpdateOp[DenseVector[Double], Double, OpSet] = {
    new BinaryUpdateOp[DenseVector[Double], Double, OpSet] {
      def apply(a: DenseVector[Double], b: Double) {
        if (a.stride == 1) {
          Arrays.fill(a.data, a.offset, a.length + a.offset, b)
          return
        }
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canPowIntoVV_D: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpPow] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpPow] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) = math.pow(ad(aoff), bd(boff)).toDouble
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canPowIntoVS_D: BinaryUpdateOp[DenseVector[Double], Double, OpPow] = {
    new BinaryUpdateOp[DenseVector[Double], Double, OpPow] {
      def apply(a: DenseVector[Double], b: Double) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = math.pow(ad(aoff), b).toDouble
          aoff += a.stride
          i += 1
        }
      }
    }
  }

  implicit val canModIntoVV_D: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpMod] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpMod] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        val ad = a.data
        val bd = b.data
        var i = 0
        var aoff = a.offset
        var boff = b.offset
        while(i < a.length) {
          ad(aoff) %= bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
      }
    }
  }

  implicit val canModIntoVS_D: BinaryUpdateOp[DenseVector[Double], Double, OpMod] = {
    new BinaryUpdateOp[DenseVector[Double], Double, OpMod] {
      def apply(a: DenseVector[Double], b: Double) = {
        val ad = a.data
        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) %= b
          aoff += a.stride
          i += 1
        }
      }
    }
  }

}
