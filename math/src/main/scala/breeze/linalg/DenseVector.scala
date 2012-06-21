package breeze.linalg

import operators._
import scala.{specialized=>spec}
import breeze.storage.DenseStorage
import breeze.generic.{URFunc, UReduceable, CanMapValues}
import support.{CanCreateZerosLike, CanZipMapValues, CanSlice, CanCopy}
import breeze.numerics.IntMath
import java.util.Arrays
import breeze.math.{TensorSpace, Semiring, Ring, Field}
import breeze.util.ArrayUtil

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

  override def ureduce[Final](f: URFunc[E, Final]) = {
    if(offset == 0 && stride == 1) f(data, length)
    else f(data, offset, stride, length, {(_:Int) => true})
  }

  def copy: DenseVector[E] = {
    implicit val man = ClassManifest.fromClass[E](data.getClass.getComponentType.asInstanceOf[Class[E]])
    val r = new DenseVector(new Array[E](length), 0, 1, length)
    r := this
    r
  }
}



object DenseVector extends VectorConstructors[DenseVector] with DenseVector_GenericOps
                      with DenseVectorOps_Int
                      with DenseVectorOps_Float
                      with DenseVectorOps_Double
                      with DenseVectorOps_SparseVector_Double
                      with DenseVectorOps_SparseVector_Float
                      with DenseVectorOps_SparseVector_Int
                      with DenseVector_SpecialOps {
  def zeros[@spec(Double, Float, Int) V: ClassManifest](size: Int) = apply(new Array[V](size))
  def apply[@spec(Double, Float, Int) V](values: Array[V]) = new DenseVector(values)
  def ones[@spec(Double, Float, Int) V: ClassManifest:Semiring](size: Int) = {
    val r = apply(new Array[V](size))
    assert(r.stride == 1)
    ArrayUtil.fill(r.data, r.offset, r.length, implicitly[Semiring[V]].one)
    r
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

  implicit def canCreateZerosLike[V:ClassManifest] = new CanCreateZerosLike[DenseVector[V], DenseVector[V]] {
    def apply(v1: DenseVector[V]) = {
      zeros[V](v1.length)
    }
  }



  implicit def canCopyDenseVector[V:ClassManifest] = new CanCopy[DenseVector[V]] {
    def apply(v1: DenseVector[V]) = {
      new DenseVector(Array.tabulate(v1.length)(i => v1(i)))
    }
  }

  def binaryOpFromBinaryUpdateOp[V, Other, Op<:OpType](implicit copy: CanCopy[DenseVector[V]], op: BinaryUpdateOp[DenseVector[V], Other, Op], man: ClassManifest[V]) = {
    new BinaryOp[DenseVector[V], Other, Op, DenseVector[V]] {
      override def apply(a : DenseVector[V], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def negFromScale[@specialized V, Double](implicit scale: BinaryOp[DenseVector[V], V, OpMulScalar, DenseVector[V]], field: Ring[V]) = {
    new UnaryOp[DenseVector[V], OpNeg, DenseVector[V]] {
      override def apply(a : DenseVector[V]) = {
        scale(a, field.negate(field.one))
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

  class CanZipMapValuesDenseVector[@specialized V, @specialized RV:ClassManifest] extends CanZipMapValues[DenseVector[V],V,RV,DenseVector[RV]] {
    def create(length : Int) = new DenseVector(new Array[RV](length))

    /**Maps all corresponding values from the two collection. */
    def map(from: DenseVector[V], from2: DenseVector[V], fn: (V, V) => RV) = {
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


  implicit def zipMap[V, R:ClassManifest] = new CanZipMapValuesDenseVector[V, R]
  implicit val zipMap_d = new CanZipMapValuesDenseVector[Double, Double]
  implicit val zipMap_f = new CanZipMapValuesDenseVector[Float, Float]
  implicit val zipMap_i = new CanZipMapValuesDenseVector[Int, Int]

  implicit val space_d = TensorSpace.make[DenseVector[Double], Int, Double]
  implicit val space_f = TensorSpace.make[DenseVector[Float], Int, Float]
  implicit val space_i = TensorSpace.make[DenseVector[Int], Int, Int]



}

trait DenseVector_GenericOps { this: DenseVector.type =>
  implicit def canSet_DV_Generic[V]: BinaryUpdateOp[DenseVector[V], V, breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseVector[V], V, breeze.linalg.operators.OpSet] {
      def apply(a: DenseVector[V], b: V) {
        val ad = a.data
        if(a.stride == 1) {
          ArrayUtil.fill(ad, a.offset, a.length, b)
          return
        }

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

  implicit def canSet_DV_DV_Generic[V]: BinaryUpdateOp[DenseVector[V], DenseVector[V], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseVector[V], DenseVector[V], breeze.linalg.operators.OpSet] {
      def apply(a: DenseVector[V], b: DenseVector[V]) {
        require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }

      }
    }
  }
}

trait DenseVector_SpecialOps extends DenseVectorOps_Double { this: DenseVector.type =>
  // hyperspecialized operators

    implicit val canAddIntoD: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpAdd] = {
      new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpAdd] {
        def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
          require(a.length == b.length, "Vectors must have same length")
          org.netlib.blas.Daxpy.daxpy(
            a.length, 1.0, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
        }
        Vector.canAddInto_V_V_Double.register(this)
      }

    }

    implicit val canSubIntoD: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSub] = {
      new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSub] {
        def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
          require(a.length == b.length, "Vectors must have same length")
          org.netlib.blas.Daxpy.daxpy(
            a.length, -1.0, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
        }
        Vector.canSubInto_V_V_Double.register(this)
      }

    }

    implicit val canDotD: BinaryOp[DenseVector[Double], DenseVector[Double], OpMulInner, Double] = {
      new BinaryOp[DenseVector[Double], DenseVector[Double], OpMulInner, Double] {
        def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
          require(a.length == b.length, "Vectors must have same length")
          org.netlib.blas.Ddot.ddot(
            a.length, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
        }
        Vector.canDotProductV_Double.register(this)
      }

    }

    implicit val canScaleIntoD: BinaryUpdateOp[DenseVector[Double], Double, OpMulScalar] = {
      new BinaryUpdateOp[DenseVector[Double], Double, OpMulScalar] {
        def apply(a: DenseVector[Double], b: Double) = {
          org.netlib.blas.Dscal.dscal(
            a.length, b, a.data, a.offset, a.stride)
        }
        Vector.canMulScalarInto_V_S_Double.register(this)
      }

    }
    implicit val canScaleD = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseVector[Double]]], canScaleIntoD, implicitly[ClassManifest[Double]])

    implicit val canSetD:BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSet] = new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSet] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) {
        org.netlib.blas.Dcopy.dcopy(
          a.length, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      Vector.canSetInto_V_V_Double.register(this)
    }


    implicit val canDot_DV_DV_Int: BinaryOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpMulInner, Int] = {
      new BinaryOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpMulInner, Int] {
        def apply(a: DenseVector[Int], b: DenseVector[Int]) = {
          require(b.length == a.length, "Vectors must be the same length!")

          val ad = a.data
          val bd = b.data
          var aoff = a.offset
          var boff = b.offset
          var result = 0

          var i = 0
          while(i < a.length) {
            result += ad(aoff) * bd(boff)
            aoff += a.stride
            boff += b.stride
            i += 1
          }
          result

        }
        Vector.canDotProductV_Int.register(this)
      }
    }

    implicit val canDot_DV_DV_Float: BinaryOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpMulInner, Float] = {
       new BinaryOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpMulInner, Float] {
         def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
           require(b.length == a.length, "Vectors must be the same length!")

           val ad = a.data
           val bd = b.data
           var aoff = a.offset
           var boff = b.offset
           var result = 0f

           var i = 0
           while(i < a.length) {
             result += ad(aoff) * bd(boff)
             aoff += a.stride
             boff += b.stride
             i += 1
           }
           result

         }
         Vector.canDotProductV_Float.register(this)
       }
     }
}