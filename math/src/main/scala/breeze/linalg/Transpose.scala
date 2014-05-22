package breeze.linalg

import breeze.linalg.operators._
import breeze.macros.expand
import breeze.linalg.support.CanTranspose
import breeze.generic.UFunc

/**
 * TODO
 *
 * @author dlwh
 **/
final case class Transpose[+T](val inner: T) extends NumericOps[Transpose[T]] {

  def repr = this

}


object Transpose extends TransposeLowPrio {

  implicit def canUntranspose[T]: CanTranspose[Transpose[T], T] = {
    new CanTranspose[Transpose[T], T] {
      def apply(from: Transpose[T]): T = from.inner
    }

  }

  implicit def transTimesNormalFromDot[T, U, R](implicit dot: OpMulInner.Impl2[T, U, R]): OpMulMatrix.Impl2[Transpose[T], U, R] = {
    new OpMulMatrix.Impl2[Transpose[T], U, R] {
      def apply(v: Transpose[T], v2: U): R = {
        dot(v.inner, v2)
      }
    }
  }

  implicit def transMulMatrix[T, U, R, RT](implicit op: OpMulMatrix.Impl2[T, U, R],
                                           canTranspose: CanTranspose[R, RT]): OpMulMatrix.Impl2[Transpose[U], Transpose[T], RT] = {
    new OpMulMatrix.Impl2[Transpose[U], Transpose[T], RT] {
      def apply(v: Transpose[U], v2: Transpose[T]): RT = canTranspose(op(v2.inner, v.inner))
    }
  }


}

trait TransposeLowPrio {
  implicit def liftOps[Op, T, U, R, RT](implicit op: UFunc.UImpl2[Op, T, U, R], canTranspose: CanTranspose[R, RT]):UFunc.UImpl2[Op, Transpose[T], Transpose[U], RT] = {
    new UFunc.UImpl2[Op, Transpose[T], Transpose[U], RT] {
      def apply(a: Transpose[T], b: Transpose[U]) = {
        canTranspose(op(a.inner,b.inner))
      }
    }

  }

  implicit def liftInPlaceOps[Op, T, U](implicit op: UFunc.InPlaceImpl2[Op, T, U]):UFunc.InPlaceImpl2[Op, Transpose[T], Transpose[U]] = {
    new UFunc.InPlaceImpl2[Op, Transpose[T], Transpose[U]] {
      def apply(a: Transpose[T], b: Transpose[U]) {
        op(a.inner,b.inner)
      }
    }

  }

  implicit class LiftApply[K, T](_trans: Transpose[Tensor[K, T]]) {
    def apply(i: K):T = _trans.inner(i)
  }

}