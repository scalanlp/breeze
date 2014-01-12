package breeze.linalg

import breeze.linalg.operators._
import breeze.linalg.support.{CanCollapseAxis, CanIterateAxis}
import breeze.generic.UFunc.{InPlaceImpl, UImpl, InPlaceImpl2, UImpl2}

/**
 * Class for classes that are broadcasting their rows.
 * That is denseMatrix(*, ::) /= denseVector
 * @param underlying the tensor (or equivalent) being broadcasted
 * @tparam T the type of the tensor
 */
case class BroadcastedRows[T](underlying: T) extends BroadcastedLike[T, BroadcastedRows[T]] {
  def repr = this
}

object BroadcastedRows {

  implicit def broadcastOp[Op, T, RowType, OpResult, Result](implicit handhold: CanCollapseAxis.HandHold[T, Axis._0.type, RowType],
                                                                op: UImpl[Op, RowType, OpResult],
                                                                cc: CanCollapseAxis[T, Axis._1.type, RowType, OpResult, Result]):UImpl[Op, BroadcastedRows[T], Result] = {
    new UImpl[Op, BroadcastedRows[T], Result] {
      def apply(v: BroadcastedRows[T]): Result = {
        cc(v.underlying, Axis._1){op(_)}
      }
    }
  }

  implicit def broadcastInplaceOp[Op, T, RowType, RHS, OpResult](implicit handhold: CanCollapseAxis.HandHold[T, Axis._1.type, RowType],
                                                                    op: InPlaceImpl[Op, RowType],
                                                                    cc: CanIterateAxis[T, Axis._1.type, RowType]):InPlaceImpl[Op, BroadcastedRows[T]] = {
    new InPlaceImpl[Op, BroadcastedRows[T]] {
      def apply(v: BroadcastedRows[T]) {
        cc(v.underlying, Axis._1){op(_)}
      }
    }
  }

  implicit def broadcastOp2[Op, T, RowType, RHS, OpResult, Result](implicit handhold: CanCollapseAxis.HandHold[T, Axis._1.type, RowType],
                                                                     op: UImpl2[Op, RowType, RHS, OpResult],
                                                                     cc: CanCollapseAxis[T, Axis._1.type, RowType, OpResult, Result]):UImpl2[Op, BroadcastedRows[T], RHS, Result] = {
    new UImpl2[Op, BroadcastedRows[T], RHS, Result] {
      def apply(v: BroadcastedRows[T], v2: RHS): Result = {
        cc(v.underlying, Axis._1){op(_, v2)}
      }
    }
  }

  implicit def broadcastInplaceOp2[Op, T, RowType, RHS, OpResult](implicit handhold: CanCollapseAxis.HandHold[T, Axis._1.type, RowType],
                                                                    op: InPlaceImpl2[Op, RowType, RHS],
                                                                    cc: CanIterateAxis[T, Axis._1.type, RowType]):InPlaceImpl2[Op, BroadcastedRows[T], RHS] = {
    new InPlaceImpl2[Op, BroadcastedRows[T], RHS] {
      def apply(v: BroadcastedRows[T], v2: RHS) {
        cc(v.underlying, Axis._1){op(_, v2)}
      }
    }
  }


}
