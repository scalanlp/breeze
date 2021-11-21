package breeze.linalg.operators

import breeze.linalg._
import breeze.macros.cforRange
import breeze.macros.require
import dev.ludovic.netlib.BLAS.{getInstance => blas}

trait DenseVector_DoubleOps extends DenseVectorExpandOps {

  // This takes higher precedence than [[DenseVector.canCopyDenseVector]] to avoid deadlock in
  // static initialization. See comment on [[DenseVectorDeps]].
  import DenseVectorDeps.canCopyDenseVector

  // TODO: try deleting (axpy)
  implicit val impl_OpAdd_InPlace_DV_DV_Double: OpAdd.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] = {
    new OpAdd.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        impl_scaleAdd_InPlace_DV_T_DV_Double(a, 1.0, b)
      }
      implicitly[BinaryUpdateRegistry[Vector[Double], Vector[Double], OpAdd.type]].register(this)
    }
  }

  implicit object impl_scaleAdd_InPlace_DV_T_DV_Double
      extends scaleAdd.InPlaceImpl3[DenseVector[Double], Double, DenseVector[Double]]
      with Serializable {
    def apply(y: DenseVector[Double], a: Double, x: DenseVector[Double]): Unit = {
      require(x.length == y.length, s"Vectors must have same length")
      // using blas here is always a bad idea.
      if (y.overlaps(x)) {
        apply(y, a, x.copy)
      } else if (x.noOffsetOrStride && y.noOffsetOrStride) {
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
  implicitly[TernaryUpdateRegistry[Vector[Double], Double, Vector[Double], scaleAdd.type]].register(impl_scaleAdd_InPlace_DV_T_DV_Double)

  // TODO: try deleting? (pure)
  implicit val impl_OpAdd_DV_DV_eq_DV_Double: OpAdd.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] = {
    pureFromUpdate(implicitly, implicitly)
  }
  implicitly[BinaryRegistry[Vector[Double], Vector[Double], OpAdd.type, Vector[Double]]].register(impl_OpAdd_DV_DV_eq_DV_Double)

  implicit val impl_OpSub_InPlace_DV_DV_Double: OpSub.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] = {
    new OpSub.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        impl_scaleAdd_InPlace_DV_T_DV_Double(a, -1.0, b)
      }
      implicitly[BinaryUpdateRegistry[Vector[Double], Vector[Double], OpSub.type]].register(this)
    }

  }

  // TODO: try removing
  implicit val impl_OpSub_DV_DV_eq_DV_Double: OpSub.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] = {
    pureFromUpdate(implicitly, implicitly)
  }
  implicitly[BinaryRegistry[Vector[Double], Vector[Double], OpSub.type, Vector[Double]]].register(impl_OpSub_DV_DV_eq_DV_Double)

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

//  /**
//   *  Returns the p-norm of this Vector (specialized for Double).
//   */
//  implicit val impl_norm_DV_D_eq_D_Double: norm.Impl2[DenseVector[Double], Double, Double] = {
//    new norm.Impl2[DenseVector[Double], Double, Double] {
//      def apply(v: DenseVector[Double], p: Double): Double = {
//        if (p == 2) {
//          math.sqrt(v.dot(v))
//        } else if (p == 1) {
//          var sum = 0.0
//          cforRange(0 until v.length)(i => sum += math.abs(v(i)))
//          sum
//        } else if (p == Double.PositiveInfinity) {
//          var max = 0.0
//          cforRange(0 until v.length)(i => max = math.max(max, math.abs(v(i))))
//          max
//        } else if (p == 0) {
//          var nnz = 0.0
//          cforRange(0 until v.length)(i => if (v(i) != 0) nnz += 1)
//          nnz
//        } else {
//          var sum = 0.0
//          cforRange(0 until v.length)(i => sum += math.pow(math.abs(v(i)), p))
//          math.pow(sum, 1.0 / p)
//        }
//      }
//    }
//  }

  implicit def impl_dim_DV_eq_I[E]: dim.Impl[DenseVector[E], Int] = _.length
}

trait DenseVector_FloatOps extends DenseVectorExpandOps {

  // This takes higher precedence than [[DenseVector.canCopyDenseVector]] to avoid deadlock in
  // static initialization. See comment on [[DenseVectorDeps]].
  import DenseVectorDeps.canCopyDenseVector

  implicit object impl_scaledAdd_InPlace_DV_S_DV_Float
    extends scaleAdd.InPlaceImpl3[DenseVector[Float], Float, DenseVector[Float]]
      with Serializable {
    def apply(y: DenseVector[Float], a: Float, x: DenseVector[Float]): Unit = {
      require(x.length == y.length, s"Vectors must have same length")
      if (y.overlaps(x)) {
        apply(y, a, x.copy)
      } else if (x.noOffsetOrStride && y.noOffsetOrStride) {
        // using blas here is always a bad idea.
        val ad = x.data
        val bd = y.data

        cforRange(0 until x.length) { i =>
          bd(i) += ad(i) * a
        }

      } else {
        slowPath(y, a, x)
      }
    }

    private def slowPath(y: DenseVector[Float], a: Float, x: DenseVector[Float]): Unit = {
      cforRange(0 until x.length) { i =>
        y(i) += x(i) * a
      }
    }
  }
  implicitly[TernaryUpdateRegistry[Vector[Float], Float, Vector[Float], scaleAdd.type]]
    .register(impl_scaledAdd_InPlace_DV_S_DV_Float)

  implicit val impl_OpAdd_InPlace_DV_DV_Float: OpAdd.InPlaceImpl2[DenseVector[Float], DenseVector[Float]] = {
    new OpAdd.InPlaceImpl2[DenseVector[Float], DenseVector[Float]] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
        scaleAdd.inPlace(a, 1.0f, b)
      }
      implicitly[BinaryUpdateRegistry[Vector[Float], Vector[Float], OpAdd.type]].register(this)
    }
  }

  implicit val impl_OpAdd_DV_DV_eq_DV_Float: OpAdd.Impl2[DenseVector[Float], DenseVector[Float], DenseVector[Float]] = {
    pureFromUpdate(implicitly, implicitly)
  }
  implicitly[BinaryRegistry[Vector[Float], Vector[Float], OpAdd.type, Vector[Float]]].register(impl_OpAdd_DV_DV_eq_DV_Float)

  implicit val impl_OpSub_InPlace_DV_DV_Float: OpSub.InPlaceImpl2[DenseVector[Float], DenseVector[Float]] = {
    new OpSub.InPlaceImpl2[DenseVector[Float], DenseVector[Float]] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
        scaleAdd.inPlace(a, -1.0f, b)
      }
      implicitly[BinaryUpdateRegistry[Vector[Float], Vector[Float], OpSub.type]].register(this)
    }

  }
  implicit val impl_OpSub_DV_DV_eq_DV_Float: OpSub.Impl2[DenseVector[Float], DenseVector[Float], DenseVector[Float]] = {
    pureFromUpdate(implicitly, implicitly)
  }
  implicitly[BinaryRegistry[Vector[Float], Vector[Float], OpSub.type, Vector[Float]]].register(impl_OpSub_DV_DV_eq_DV_Float)

  implicit val impl_OpMulInner_DV_DV_eq_S_Float
  : breeze.linalg.operators.OpMulInner.Impl2[DenseVector[Float], DenseVector[Float], Float] = {
    new breeze.linalg.operators.OpMulInner.Impl2[DenseVector[Float], DenseVector[Float], Float] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
        require(a.length == b.length, s"Vectors must have same length")
        if (a.noOffsetOrStride && b.noOffsetOrStride && a.length < DenseVectorSupportMethods.MAX_SMALL_DOT_PRODUCT_LENGTH) {
          DenseVectorSupportMethods.smallDotProduct_Float(a.data, b.data, a.length)
        } else {
          blasPath(a, b)
        }
      }

      val UNROLL_FACTOR = 6

      private def blasPath(a: DenseVector[Float], b: DenseVector[Float]): Float = {
        if ((a.length <= 300 || !usingNatives) && a.stride == 1 && b.stride == 1) {
          DenseVectorSupportMethods.dotProduct_Float(a.data, a.offset, b.data, b.offset, a.length)
        } else {
          val boff = if (b.stride >= 0) b.offset else (b.offset + b.stride * (b.length - 1))
          val aoff = if (a.stride >= 0) a.offset else (a.offset + a.stride * (a.length - 1))
          blas.sdot(a.length, b.data, boff, b.stride, a.data, aoff, a.stride)
        }
      }
      implicitly[BinaryRegistry[Vector[Float], Vector[Float], OpMulInner.type, Float]].register(this)
    }
  }
}
