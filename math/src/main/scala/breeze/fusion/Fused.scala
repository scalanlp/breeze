package breeze.fusion

import breeze.generic.UFunc
import breeze.linalg.operators.{OpMulMatrix, OpMulScalar, OpAdd}
import breeze.linalg.{scaleAdd, NumericOps, DenseVector}

sealed trait Fused[+T] extends NumericOps[Fused[T]] {
  def value: T
}

case class Atom[T](value: T) extends Fused[T] with NumericOps[Atom[T]] {
  def repr = this

}

case class BinOp[Op, T1, T2, T, F1, F2](t1: F1, t2: F2, op: UFunc.UImpl2[Op, T1, T2, T])(implicit f1: F1 <:< Fused[T1], f2: F2 <:< Fused[T2]) extends Fused[T] with NumericOps[BinOp[Op, T1, T2, T, F1, F2]] {
  def repr = this


  lazy val value = {
    op(t1.value, t2.value)
  }

}

object Fused extends LowPriorityFused {
  implicit def fuseScaleAdd[T1, T2, T, F1, F2](implicit f1: F1 <:< Fused[T1],
                                               f2: F2 <:< Fused[T2],
                                               scaleAddImpl: scaleAdd.InPlaceImpl3[T, T1, T2]
                                                    ): OpAdd.InPlaceImpl2[T, BinOp[OpMulScalar.type, T1, T2, T, F1, F2]] = {
    new OpAdd.InPlaceImpl2[T, BinOp[OpMulScalar.type, T1, T2, T, F1, F2]] {

      override def apply(v: T, v2: BinOp[OpMulScalar.type, T1, T2, T, F1, F2]): Unit = {
        scaleAddImpl(v, v2.t1.value, v2.t2.value)
      }
    }

  }

  implicit def fuseScaleAddMatrix[T1, T2, T, F1, F2](implicit f1: F1 <:< Fused[T1],
                                               f2: F2 <:< Fused[T2],
                                               scaleAddImpl: scaleAdd.InPlaceImpl3[T, T1, T2]
                                                ): OpAdd.InPlaceImpl2[T, BinOp[OpMulMatrix.type, T1, T2, T, F1, F2]] = {
    new OpAdd.InPlaceImpl2[T, BinOp[OpMulMatrix.type, T1, T2, T, F1, F2]] {

      override def apply(v: T, v2: BinOp[OpMulMatrix.type, T1, T2, T, F1, F2]): Unit = {
        scaleAddImpl(v, v2.t1.value, v2.t2.value)
      }
    }

  }

}

sealed trait LowPriorityFused {
  implicit def liftSubClasses2[Op, T1, T2, T, F1, F2](implicit f1: F1 <:< Fused[T1],
                                                      f2: F2 <:< Fused[T2],
                                                      base: UFunc.UImpl2[Op, T1, T2, T]):UFunc.UImpl2[Op, F1, F2, BinOp[Op, T1, T2, T, F1, F2]] = {
    new UFunc.UImpl2[Op, F1, F2, BinOp[Op, T1, T2, T, F1, F2]] {
      override def apply(v: F1, v2: F2): BinOp[Op, T1, T2, T, F1, F2] = {
        BinOp(v, v2, base)
      }
    }
  }

  implicit def liftInPlace2[Op, T1, T2, F1, F2](implicit f2: F2 <:< Fused[T2],
                                                base: UFunc.InPlaceImpl2[Op, T1, T2]):UFunc.InPlaceImpl2[Op, T1, F2] = {
    new UFunc.InPlaceImpl2[Op, T1, F2] {
      override def apply(v: T1, v2: F2): Unit = {
        base(v, v2.value)
      }
    }
  }
}

object Foo {
//  import Fused._
  val x = DenseVector.zeros[Double](100)
  x.+=(Atom(3.0) * Atom(x))

}
