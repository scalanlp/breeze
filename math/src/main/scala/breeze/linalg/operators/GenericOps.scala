package breeze.linalg.operators

import breeze.generic.UFunc
import breeze.generic.UFunc.{InPlaceImpl, InPlaceImpl2, UImpl, UImpl2}
import breeze.gymnastics._
import breeze.linalg.{Matrix, Vector, scaleAdd}
import breeze.linalg.support.{CanCopy, CanTransformValues, CanZipMapValues, ScalarOf}
import breeze.math.{Ring, Semiring}
import breeze.util.WideningConversion

import scala.util._
import breeze.macros._

trait GenericOpsLowPrio3 {
  implicit def impl_T_S_eq_U_from_ZipMap[Tag, T, V1, VR, U](implicit handhold: ScalarOf[T, V1],
                                                            impl: UFunc.UImpl2[Tag, V1, V1, VR],
                                                            canZipMapValues: CanZipMapValues[T, V1, VR, U]): UFunc.UImpl2[Tag, T, T, U] = {
    (v1: T, v2: T) => canZipMapValues.map(v1, v2, impl.apply)
  }

}

trait CastOps extends GenericOpsLowPrio3 {

  implicit def castOps_V_V[M1 <: Vector[T], M2 <: Vector[T], T, Op <: OpType, MR](implicit
                                                                                  v1lt: M1 <:< Vector[T],
                                                                                  v2lt: M2 <:< Vector[T],
                                                                                  v1ne: NotGiven[ (M1 =:= Vector[T]) &:& (M2 =:= Vector[T])],
                                                                                  op: UImpl2[Op, Vector[T], Vector[T], MR]): UImpl2[Op, M1, M2, MR] = {
    op.asInstanceOf[UFunc.UImpl2[Op, M1, M2, MR]]
  }

  implicit def castUpdateOps_V_V[M1 <: Vector[T], M2 <: Vector[T], T, Op <: OpType](implicit
                                                                                    v1lt: M1 <:< Vector[T],
                                                                                    v2lt: M2 <:< Vector[T],
                                                                                    v1ne: NotGiven[ (M1 =:= Vector[T]) &:& (M2 =:= Vector[T])],
                                                                                    op: UFunc.InPlaceImpl2[Op, Vector[T], Vector[T]]): UFunc.InPlaceImpl2[Op, M1, M2] = {
    op.asInstanceOf[UFunc.InPlaceImpl2[Op, M1, M2]]
  }


  implicit def castOps_V_S[M1 <: Vector[T], T, Op <: OpType, MR](implicit v2: ScalarOf[M1, T],
                                                                 v1lt: M1 <:< Vector[T],
                                                                 v1ne: NotGiven[M1 =:= Vector[T]],
                                                                 op: UImpl2[Op, Vector[T], T, MR]): UImpl2[Op, M1, T, MR] = {
    op.asInstanceOf[UFunc.UImpl2[Op, M1, T, MR]]
  }

  implicit def castUpdateOps_V_S[M1 <: Vector[T], T, Op <: OpType](implicit
                                                                   v2: ScalarOf[M1, T],
                                                                   v1lt: M1 <:< Vector[T],
                                                                   v1ne: NotGiven[ (M1 =:= Vector[T])],
                                                                   op: UFunc.InPlaceImpl2[Op, Vector[T], T]): UFunc.InPlaceImpl2[Op, M1, T] = {
    op.asInstanceOf[UFunc.InPlaceImpl2[Op, M1, T]]
  }

  implicit def castOps_M_M[M1 <: Matrix[T], M2 <: Matrix[T], T, Op<: OpType, MR](implicit
                                                                                 v1lt: M1 <:< Matrix[T],
                                                                                 v2lt: M2 <:< Matrix[T],
                                                                                 v1ne: NotGiven[ (M1 =:= Matrix[T]) &:& (M2 =:= Matrix[T])],
                                                                                 op: UImpl2[Op, Matrix[T], Matrix[T], MR]): UImpl2[Op, M1, M2, MR] = {
    op.asInstanceOf[UFunc.UImpl2[Op, M1, M2, MR]]
  }

  implicit def castUpdateOps_M_M[M1 <: Matrix[T], M2 <: Matrix[T], T, Op <: OpType](implicit
                                                                                    v1lt: M1 <:< Matrix[T],
                                                                                    v2lt: M2 <:< Matrix[T],
                                                                                    v1ne: NotGiven[ (M1 =:= Matrix[T]) &:& (M2 =:= Matrix[T])],
                                                                                    op: UFunc.InPlaceImpl2[Op, Matrix[T], Matrix[T]]): UFunc.InPlaceImpl2[Op, M1, M2] = {
    op.asInstanceOf[UFunc.InPlaceImpl2[Op, M1, M2]]
  }

  implicit def castOps_M_V[M1 <: Matrix[T], M2 <: Vector[T], T, Op<: OpType, MR](implicit
                                                                                 v1lt: M1 <:< Matrix[T],
                                                                                 v2lt: M2 <:< Vector[T],
                                                                                 v1ne: NotGiven[ (M1 =:= Matrix[T]) &:& (M2 =:= Vector[T])],
                                                                                 //                                                                        v1ne: NotGiven[M1 =:= Matrix[T]],
                                                                                 //                                                                           v2ne: NotGiven[M2 =:= Vector[T]],
                                                                                 op: UImpl2[Op, Matrix[T], Vector[T], MR]): UImpl2[Op, M1, M2, MR] = {
    op.asInstanceOf[UFunc.UImpl2[Op, M1, M2, MR]]
  }

  implicit def castUpdateOps_M_V[M1 <: Matrix[T], M2 <: Vector[T], T, Op <: OpType](implicit
                                                                                    v1lt: M1 <:< Matrix[T],
                                                                                    v2lt: M2 <:< Vector[T],
                                                                                    v1ne: NotGiven[ (M1 =:= Matrix[T]) &:& (M2 =:= Vector[T])],
                                                                                    //                                                                                       v1ne: NotGiven[M1 =:= Matrix[T]],
                                                                                    //                                                                                       v2ne: NotGiven[M2 =:= Vector[T]],
                                                                                    op: UFunc.InPlaceImpl2[Op, Matrix[T], Vector[T]]): UFunc.InPlaceImpl2[Op, M1, M2] = {
    op.asInstanceOf[UFunc.InPlaceImpl2[Op, M1, M2]]
  }
  // TODO: why doesn't this work :( :(
  //  implicit def castReturnTypeOp1[Op, T1, R, R2](
  //                                                 implicit op: UFunc.UImpl[Op, T1, R],
  //                                                 conversion: WideningConversion[R, R2]): UFunc.UImpl[Op, T1, R2] = {
  //    (t1: T1) => conversion(op(t1))
  //  }
  //
  //  implicit def castReturnTypeOp2[Op, T1, T2, R, R2](implicit op: UFunc.UImpl2[Op, T1, T2, R],
  //                                                    conversion: WideningConversion[R, R2]): UFunc.UImpl2[Op, T1, T2, R2] = {
  //    (t1: T1, t2: T2) => conversion(op(t1, t2))
  //  }
  //
  //  implicit def castReturnTypeOp3[Op, T1, T2, T3, R, R2](implicit op: UFunc.UImpl3[Op, T1, T2, T3, R], conversion: WideningConversion[R, R2]): UFunc.UImpl3[Op, T1, T2, T3, R2] = {
  //    (t1: T1, t2: T2, t3: T3) => conversion(op(t1, t2, t3))
  //  }
  //
  //  implicit def castUpdateOps[T, Other, U <: Other, Op <: OpType](implicit
  //                                                                 ev: U <:< Other,
  //                                                                 v2ev: NotGiven[U =:= Other],
  //                                                        op: UFunc.InPlaceImpl2[Op,T, Other],
  //                                                       ): UFunc.InPlaceImpl2[Op, T, U] = {
  //    op.asInstanceOf[UFunc.InPlaceImpl2[Op, T, U]]
  //  }
  //
  //
  //  implicit def castOps[T, Other, U <: Other, R, Op <: OpType](implicit
  //                                                              ev: U <:< Other,
  //                                                              v2ev: NotGiven[U =:= Other],
  //                                                              op: UFunc.UImpl2[Op, T, Other, R],
  //  ): UFunc.UImpl2[Op, T, U, R] = {
  //    op.asInstanceOf[UFunc.UImpl2[Op, T, U, R]]
  //  }

}

trait GenericOpsLowPrio extends CastOps {

  implicit def pureFromUpdate[T, Other, Op <: OpType](implicit op: UFunc.InPlaceImpl2[Op, T, Other],
                                                      copy: CanCopy[T]): UFunc.UImpl2[Op, T, Other, T] =
    (a: T, b: Other) => {
      val c = copy(a)
      op(c, b)
      c
    }
}

trait GenericOps extends GenericOpsLowPrio {

  implicit def impl_OpAdd_InPlace_T_U_Generic_from_scaleAdd_InPlace[T, U, V](
      implicit sa: scaleAdd.InPlaceImpl3[T, U, V],
      semi: Semiring[U]): OpAdd.InPlaceImpl2[T, V] = { (t: T, v: V) =>
    scaleAdd.inPlace(t, semi.one, v)
  }

  implicit def impl_OpSub_InPlace_T_U_Generic_from_scaleAdd_InPlace[T, U, V](
      implicit sa: scaleAdd.InPlaceImpl3[T, U, V],
      ring: Ring[U]): OpSub.InPlaceImpl2[T, V] = { (t: T, v: V) =>
    scaleAdd.inPlace(t, ring.negate(ring.one), v)
  }

  implicit def impl_OpNeg_T_Generic_from_OpMulScalar[T, U, V](implicit scalarOf: ScalarOf[T, V], ring: Ring[V], scale: OpMulScalar.Impl2[T, V, U]): OpNeg.Impl[T, U] = {
    new OpNeg.Impl[T, U] {
      override def apply(a: T): U = {
        scale(a, ring.negate(ring.one))
      }
    }
  }
}

object GenericOps {
  // TODO: switch to using swap or sink or something
  def updateFromPure[Op, T, Other, R](implicit op: UFunc.UImpl2[Op, T, Other, R],
                                   set: OpSet.InPlaceImpl2[T, R]): UFunc.InPlaceImpl2[Op, T, Other] = {
      (a: T, b: Other) => {
        val result = op(a, b)
        set(a, result)
      }
  }
}