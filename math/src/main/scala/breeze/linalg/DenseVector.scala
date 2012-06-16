package breeze.linalg

import operators._
import scala.{specialized=>spec}
import breeze.storage.DenseStorage
import breeze.generic.CanMapValues
import support.{CanSlice, CanCopy}
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

  def apply(i: Int) = {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    rawApply(offset + i * stride)
  }

  def update(i: Int, v: E) {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    rawUpdate(offset + i * stride, v)
  }

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
                      with DenseVectorOps_Int
                      with DenseVectorOps_Float
                      with DenseVectorOps_Double
                      with DenseVectorOps_SparseVector_Double
                      with DenseVectorOps_SparseVector_Float
                      with DenseVectorOps_SparseVector_Int {
  def zeros[@spec(Double, Float, Int) V: ClassManifest](size: Int) = apply(new Array[V](size))
  def apply[@spec(Double, Float, Int) V](values: Array[V]) = new DenseVector(values)

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

  // hyperspecialized operators

  implicit val canAddIntoD: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpAdd] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpAdd] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        org.netlib.blas.Daxpy.daxpy(
          a.length, 1.0, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      Vector.canAddIntoD.register(this)
    }

  }

  implicit val canSubIntoD: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSub] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSub] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        org.netlib.blas.Daxpy.daxpy(
          a.length, -1.0, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      Vector.canSubIntoD.register(this)
    }

  }

  implicit val canDotD: BinaryOp[DenseVector[Double], DenseVector[Double], OpMulInner, Double] = {
    new BinaryOp[DenseVector[Double], DenseVector[Double], OpMulInner, Double] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        org.netlib.blas.Ddot.ddot(
          a.length, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      Vector.canDotD.register(this)
    }

  }

  implicit val canScaleIntoD: BinaryUpdateOp[DenseVector[Double], Double, OpMulScalar] = {
    new BinaryUpdateOp[DenseVector[Double], Double, OpMulScalar] {
      def apply(a: DenseVector[Double], b: Double) = {
        org.netlib.blas.Dscal.dscal(
          a.length, b, a.data, a.offset, a.stride)
      }
      Vector.canScaleD.register(this)
    }

  }
  implicit val canScaleD = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseVector[Double]]], canScaleIntoD, implicitly[ClassManifest[Double]])

  implicit val canSetD:BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSet] = new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSet] {
    def apply(a: DenseVector[Double], b: DenseVector[Double]) {
      org.netlib.blas.Dcopy.dcopy(
        a.length, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
    }
  }

  implicit def canCopyDenseVector[V:ClassManifest] = new CanCopy[DenseVector[V]] {
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




  implicit def canTranspose[V]: CanTranspose[DenseVector[V], DenseMatrix[V]] = {
    new CanTranspose[DenseVector[V], DenseMatrix[V]] {
      def apply(from: DenseVector[V]) = {
        new DenseMatrix(from.data, offset = from.offset, cols = from.length, rows = 1, majorStride = from.stride)
      }
    }
  }
}

