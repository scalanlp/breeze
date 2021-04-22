package breeze.linalg.operators

import breeze.generic.UFunc
import breeze.linalg.scaleAdd
import breeze.linalg.support.{CanCopy, CanZipMapValues, ScalarOf}
import breeze.math.{Ring, Semiring}

trait GenericOpsLowPrio {

  implicit def canZipMapValuesImpl[Tag, T, V1, VR, U](implicit handhold: ScalarOf[T, V1],
                                                 impl: UFunc.UImpl2[Tag, V1, V1, VR],
                                                 canZipMapValues: CanZipMapValues[T, V1, VR, U]): UFunc.UImpl2[Tag, T, T, U] = {
    new UFunc.UImpl2[Tag, T, T, U] {
      def apply(v1: T, v2: T): U = canZipMapValues.map(v1, v2, impl.apply)
    }
  }
}

trait GenericOps {

  def binaryOpFromUpdateOp[Op <: OpType, T, Other](
      implicit copy: CanCopy[T],
      op: UFunc.InPlaceImpl2[Op, T, Other]): UFunc.UImpl2[Op, T, Other, T] = {
    new UFunc.UImpl2[Op, T, Other, T] {
      override def apply(a: T, b: Other): T = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def pureFromUpdate[T, Other, Op <: OpType](
      implicit op: UFunc.InPlaceImpl2[Op, T, Other],
      copy: CanCopy[T]): UFunc.UImpl2[Op, T, Other, T] =
    new UFunc.UImpl2[Op, T, Other, T] {
      override def apply(a: T, b: Other): T = {
        val c = copy(a)
        op(c, b)
        c
      }
    }

  implicit def addIntoFromScaleAdd[T, U, V](
      implicit sa: scaleAdd.InPlaceImpl3[T, U, V],
      semi: Semiring[U]): OpAdd.InPlaceImpl2[T, V] = { (t: T, v: V) =>
    scaleAdd.inPlace(t, semi.one, v)
  }

  implicit def subIntoFromScaleAdd[T, U, V](
      implicit sa: scaleAdd.InPlaceImpl3[T, U, V],
      ring: Ring[U]): OpSub.InPlaceImpl2[T, V] = { (t: T, v: V) =>
    scaleAdd.inPlace(t, ring.negate(ring.one), v)
  }


  implicit def negFromScale[T, U, V](implicit scalarOf: ScalarOf[T, V], ring: Ring[V], scale: OpMulScalar.Impl2[T, V, U]): OpNeg.Impl[T, U] = {
    new OpNeg.Impl[T, U] {
      override def apply(a: T): U = {
        scale(a, ring.negate(ring.one))
      }
    }
  }
}
