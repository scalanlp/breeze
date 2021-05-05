package breeze.linalg.operators

import breeze.generic.UFunc
import breeze.linalg._
import breeze.linalg.support.{CanSlice, CanTranspose}
import breeze.math.{Complex, Semiring}
import breeze.storage.Zero

import scala.reflect.ClassTag

trait TransposeOps extends TransposeOps_Generic with TransposeOps_Complex with CSCMatrix_TransposeOps {

}

trait TransposeOps_Generic extends TransposeOpsLowPrio {

  implicit def canUntranspose[T]: CanTranspose[Transpose[T], T] = {
    new CanTranspose[Transpose[T], T] {
      def apply(from: Transpose[T]): T = from.inner
    }

  }

  implicit def transTimesNormalFromDot[T, U, R](
                                                 implicit dot: OpMulInner.Impl2[T, U, R]): OpMulMatrix.Impl2[Transpose[T], U, R] = {
    new OpMulMatrix.Impl2[Transpose[T], U, R] {
      def apply(v: Transpose[T], v2: U): R = {
        dot(v.inner, v2)
      }
    }
  }

  implicit def transMulMatrix[T, U, R, RT](
                                            implicit op: OpMulMatrix.Impl2[T, U, R],
                                            canTranspose: CanTranspose[R, RT]): OpMulMatrix.Impl2[Transpose[U], Transpose[T], RT] = {
    new OpMulMatrix.Impl2[Transpose[U], Transpose[T], RT] {
      def apply(v: Transpose[U], v2: Transpose[T]): RT = canTranspose(op(v2.inner, v.inner))
    }
  }

  implicit def liftTransposeOps[Op, K, V, T, R, RT](
                                                     implicit ev: T <:< Tensor[K, V],
                                                     op: UFunc.UImpl2[Op, T, V, R],
                                                     canTranspose: CanTranspose[R, RT]): UFunc.UImpl2[Op, Transpose[T], V, RT] = {
    new UFunc.UImpl2[Op, Transpose[T], V, RT] {
      def apply(a: Transpose[T], b: V) = {
        canTranspose(op(a.inner, b))
      }
    }

  }

  implicit def liftTransposeInPlaceOps[Op, K, V, T](
                                                     implicit ev: T <:< Tensor[K, V],
                                                     op: UFunc.InPlaceImpl2[Op, T, V]): UFunc.InPlaceImpl2[Op, Transpose[T], V] = {
    new UFunc.InPlaceImpl2[Op, Transpose[T], V] {
      def apply(a: Transpose[T], b: V): Unit = {
        op(a.inner, b)
      }
    }

  }

  implicit def transposeTensor[K, V, T](implicit ev: T <:< Tensor[K, V]): CanTranspose[T, Transpose[T]] = {
    new CanTranspose[T, Transpose[T]] {
      def apply(from: T): Transpose[T] = new Transpose(from)
    }
  }

}

trait TransposeOpsLowPrio extends GenericOps {
  implicit def liftOps[Op, T, U, R, RT](
                                         implicit op: UFunc.UImpl2[Op, T, U, R],
                                         canTranspose: CanTranspose[R, RT]): UFunc.UImpl2[Op, Transpose[T], Transpose[U], RT] = {
    new UFunc.UImpl2[Op, Transpose[T], Transpose[U], RT] {
      def apply(a: Transpose[T], b: Transpose[U]) = {
        canTranspose(op(a.inner, b.inner))
      }
    }

  }

  implicit def liftInPlaceOps[Op, T, U, UT](implicit
                                            transU: CanTranspose[U, UT],
                                            op: UFunc.InPlaceImpl2[Op, T, UT]): UFunc.InPlaceImpl2[Op, Transpose[T], U] = {
    new UFunc.InPlaceImpl2[Op, Transpose[T], U] {
      def apply(a: Transpose[T], b: U): Unit = {
        op(a.inner, transU(b))
      }
    }

  }

  implicit class LiftApply[K, T](_trans: Transpose[Tensor[K, T]]) {
    def apply(i: K): T = _trans.inner(i)
  }

  // TODO: make CanSlice a UFunc
  implicit def liftSlice[Op, T, S, U, UT](
                                           implicit op: CanSlice[T, S, U],
                                           trans: CanTranspose[U, UT]): CanSlice[Transpose[T], S, UT] = {
    new CanSlice[Transpose[T], S, UT] {
      override def apply(from: Transpose[T], slice: S): UT = {
        op(from.inner, slice).t
      }
    }
  }

  implicit def liftUFunc[Op, T, U, UT](
                                        implicit op: UFunc.UImpl[Op, T, U],
                                        trans: CanTranspose[U, UT]): UFunc.UImpl[Op, Transpose[T], UT] = {
    new UFunc.UImpl[Op, Transpose[T], UT] {
      override def apply(v: Transpose[T]): UT = trans(op(v.inner))
    }
  }

  implicit def impl_Op_InPlace_Tt_lift_from_Op_T[Op, T, U](implicit op: UFunc.InPlaceImpl[Op, T]): UFunc.InPlaceImpl[Op, Transpose[T]] = {
    new UFunc.InPlaceImpl[Op, Transpose[T]] {
      override def apply(v: Transpose[T]) = op(v.inner)
    }
  }

  implicit def liftUFunc3_1[Op, T, T2, U2, T3, U3, R, RT](implicit
                                                          t2Trans: CanTranspose[T2, U2],
                                                          t3Trans: CanTranspose[T3, U3],
                                                          op: UFunc.UImpl3[Op, T, U2, U3, R],
                                                          transR: CanTranspose[R, RT]): UFunc.UImpl3[Op, Transpose[T], T2, T3, RT] = {
    new UFunc.UImpl3[Op, Transpose[T], T2, T3, RT] {

      override def apply(v: Transpose[T], v2: T2, v3: T3): RT = {
        transR(op(v.inner, t2Trans(v2), t3Trans(v3)))
      }
    }
  }

  implicit def liftUFuncInplace3_1[Op, T, T2, U2, U3, T3](
                                                   implicit
                                                   t2Trans: CanTranspose[T2, U2],
                                                   t3Trans: CanTranspose[T3, U3],
                                                   op: UFunc.InPlaceImpl3[Op, T, U2, U3]): UFunc.InPlaceImpl3[Op, Transpose[T], T2, T3] = {
    new UFunc.InPlaceImpl3[Op, Transpose[T], T2, T3] {

      override def apply(v: Transpose[T], v2: T2, v3: T3): Unit = {
        op(v.inner, t2Trans(v2), t3Trans(v3))
      }
    }
  }

}

trait TransposeOps_Complex extends TransposeOps_Generic with DenseMatrix_TransposeOps {

  implicit def canTranspose_DV_Complex: CanTranspose[DenseVector[Complex], DenseMatrix[Complex]] = {
    new CanTranspose[DenseVector[Complex], DenseMatrix[Complex]] {
      def apply(from: DenseVector[Complex]): DenseMatrix[Complex] = {
        new DenseMatrix(
          data = from.data.map { _.conjugate },
          offset = from.offset,
          cols = from.length,
          rows = 1,
          majorStride = from.stride)
      }
    }
  }

  implicit def canTranspose_SV_Complex: CanTranspose[SparseVector[Complex], CSCMatrix[Complex]] = {
    new CanTranspose[SparseVector[Complex], CSCMatrix[Complex]] {
      def apply(from: SparseVector[Complex]) = {
        val transposedMtx: CSCMatrix[Complex] = CSCMatrix.zeros[Complex](1, from.length)
        var i = 0
        while (i < from.activeSize) {
          val c = from.index(i)
          transposedMtx(0, c) = from.data(i).conjugate
          i += 1
        }
        transposedMtx
      }
    }
  }
}

trait DenseMatrix_TransposeOps extends TransposeOps_Generic {

  implicit def canTranspose[V]: CanTranspose[DenseMatrix[V], DenseMatrix[V]] = {
    new CanTranspose[DenseMatrix[V], DenseMatrix[V]] {
      def apply(from: DenseMatrix[V]): DenseMatrix[V] = {
        DenseMatrix.create(
          data = from.data,
          offset = from.offset,
          cols = from.rows,
          rows = from.cols,
          majorStride = from.majorStride,
          isTranspose = !from.isTranspose)
      }
    }
  }

  implicit def canTransposeComplex: CanTranspose[DenseMatrix[Complex], DenseMatrix[Complex]] = {
    new CanTranspose[DenseMatrix[Complex], DenseMatrix[Complex]] {
      def apply(from: DenseMatrix[Complex]) = {
        new DenseMatrix(
          data = from.data.map { _.conjugate },
          offset = from.offset,
          cols = from.rows,
          rows = from.cols,
          majorStride = from.majorStride,
          isTranspose = !from.isTranspose)
      }
    }
  }

}

trait CSCMatrix_TransposeOps extends TransposeOps_Generic {
  implicit def canTranspose_CSC[V: ClassTag: Zero: Semiring]: CanTranspose[CSCMatrix[V], CSCMatrix[V]] = {
    new CanTranspose[CSCMatrix[V], CSCMatrix[V]] {
      def apply(from: CSCMatrix[V]) = {
        val transposedMtx = new CSCMatrix.Builder[V](from.cols, from.rows, from.activeSize)

        var j = 0
        while (j < from.cols) {
          var ip = from.colPtrs(j)
          while (ip < from.colPtrs(j + 1)) {
            val i = from.rowIndices(ip)
            transposedMtx.add(j, i, from.data(ip))
            ip += 1
          }
          j += 1
        }
        // this doesn't hold if there are zeros in the matrix
        //        assert(transposedMtx.activeSize == from.activeSize,
        //          s"We seem to have lost some elements?!?! ${transposedMtx.activeSize} ${from.activeSize}")
        transposedMtx.result(false, false)
      }
    }
  }

  implicit def canTranspose_CSC_Complex: CanTranspose[CSCMatrix[Complex], CSCMatrix[Complex]] = {
    new CanTranspose[CSCMatrix[Complex], CSCMatrix[Complex]] {
      def apply(from: CSCMatrix[Complex]) = {
        val transposedMtx = CSCMatrix.zeros[Complex](from.cols, from.rows)

        var j = 0
        while (j < from.cols) {
          var ip = from.colPtrs(j)
          while (ip < from.colPtrs(j + 1)) {
            val i = from.rowIndices(ip)
            transposedMtx(j, i) = from.data(ip).conjugate
            ip += 1
          }
          j += 1
        }
        transposedMtx
      }
    }
  }
}