package breeze.linalg.operators

import breeze.linalg._
import breeze.macros.cforRange
import scalaxy.debug.require
import com.github.fommil.netlib.BLAS.{getInstance => blas}

trait DenseVectorExtraSpecialOps extends DenseVectorExpandOps {

  implicit val canAddIntoD: OpAdd.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] = {
    new OpAdd.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        canDaxpy(a, 1.0, b)
      }
      implicitly[BinaryUpdateRegistry[Vector[Double], Vector[Double], OpAdd.type]].register(this)
    }
  }

  implicit object canDaxpy
      extends scaleAdd.InPlaceImpl3[DenseVector[Double], Double, DenseVector[Double]]
      with Serializable {
    def apply(y: DenseVector[Double], a: Double, x: DenseVector[Double]): Unit = {
      require(x.length == y.length, s"Vectors must have same length")
      // using blas here is always a bad idea.
      if (x.noOffsetOrStride && y.noOffsetOrStride) {
        val ad = x.data
        val bd = y.data
        cforRange(0 until x.length) { i =>
          bd(i) += ad(i) * a
        }
      } else {
        cforRange(0 until x.length) { i =>
          y(i) += x(i) * a
        }
      }
    }

  }
  implicitly[TernaryUpdateRegistry[Vector[Double], Double, Vector[Double], scaleAdd.type]].register(canDaxpy)

  implicit val canAddD: OpAdd.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] = {
    pureFromUpdate(implicitly, implicitly)
  }
  implicitly[BinaryRegistry[Vector[Double], Vector[Double], OpAdd.type, Vector[Double]]].register(canAddD)

  implicit val canSubIntoD: OpSub.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] = {
    new OpSub.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        canDaxpy(a, -1.0, b)
      }
      implicitly[BinaryUpdateRegistry[Vector[Double], Vector[Double], OpSub.type]].register(this)
    }

  }
  implicit val dv_canSubD: OpSub.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] = {
    pureFromUpdate(implicitly, implicitly)
  }
  implicitly[BinaryRegistry[Vector[Double], Vector[Double], OpSub.type, Vector[Double]]].register(dv_canSubD)

  implicit object canDotD extends OpMulInner.Impl2[DenseVector[Double], DenseVector[Double], Double] {
    def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
      require(a.length == b.length, s"Vectors must have same length")
      if (a.noOffsetOrStride && b.noOffsetOrStride && a.length < DenseVectorSupportMethods.MAX_SMALL_DOT_PRODUCT_LENGTH) {
        DenseVectorSupportMethods.smallDotProduct_Double(a.data, b.data, a.length)
      } else {
        blasPath(a, b)
      }
    }

    private def blasPath(a: DenseVector[Double], b: DenseVector[Double]): Double = {
      if ((a.length <= 300 || !usingNatives) && a.stride == 1 && b.stride == 1) {
        DenseVectorSupportMethods.dotProduct_Double(a.data, a.offset, b.data, b.offset, a.length)
      } else {
        val boff = if (b.stride >= 0) b.offset else (b.offset + b.stride * (b.length - 1))
        val aoff = if (a.stride >= 0) a.offset else (a.offset + a.stride * (a.length - 1))
        blas.ddot(a.length, b.data, boff, b.stride, a.data, aoff, a.stride)
      }
    }

  }
  implicitly[BinaryRegistry[Vector[Double], Vector[Double], OpMulInner.type, Double]].register(canDotD)

  /**
   *  Returns the p-norm of this Vector (specialized for Double).
   */
  implicit def canNorm_Double: norm.Impl2[DenseVector[Double], Double, Double] = {
    new norm.Impl2[DenseVector[Double], Double, Double] {
      def apply(v: DenseVector[Double], p: Double): Double = {
        if (p == 2) {
          math.sqrt(v.dot(v))
        } else if (p == 1) {
          var sum = 0.0
          cforRange(0 until v.length)(i => sum += math.abs(v(i)))
          sum
        } else if (p == Double.PositiveInfinity) {
          var max = 0.0
          cforRange(0 until v.length)(i => max = math.max(max, math.abs(v(i))))
          max
        } else if (p == 0) {
          var nnz = 0.0
          cforRange(0 until v.length)(i => if (v(i) != 0) nnz += 1)
          nnz
        } else {
          var sum = 0.0
          cforRange(0 until v.length)(i => sum += math.pow(math.abs(v(i)), p))
          math.pow(sum, 1.0 / p)
        }
      }
    }
  }

  implicit def canDim[E]: dim.Impl[DenseVector[E], Int] = new dim.Impl[DenseVector[E], Int] {
    def apply(v: DenseVector[E]): Int = v.length
  }
}
