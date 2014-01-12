package breeze.linalg

import breeze.linalg.operators._
import breeze.linalg.support.{CanCollapseAxis, CanIterateAxis}
import breeze.generic.UFunc.{InPlaceImpl, UImpl, InPlaceImpl2, UImpl2}

/**
 * Class for classes that are broadcasting their columns.
 * That is denseMatrix(::, *) /= denseVector
 * @param underlying the tensor (or equivalent) being broadcasted
 * @tparam T the type of the tensor
 */
case class BroadcastedColumns[T](underlying: T) extends BroadcastedLike[T, BroadcastedColumns[T]] {
  def repr = this
}

object BroadcastedColumns {
  implicit def broadcastOp[Op, T, ColumnType, OpResult, Result](implicit handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
                                                                     op: UImpl[Op, ColumnType, OpResult],
                                                                     cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]):UImpl[Op, BroadcastedColumns[T], Result] = {
    new UImpl[Op, BroadcastedColumns[T], Result] {
      def apply(v: BroadcastedColumns[T]): Result = {
        cc(v.underlying, Axis._0){op(_)}
      }
    }
  }

  implicit def broadcastInplaceOp[Op, T, ColumnType, RHS, OpResult](implicit handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
                                                                     op: InPlaceImpl[Op, ColumnType],
                                                                     cc: CanIterateAxis[T, Axis._0.type, ColumnType]):InPlaceImpl[Op, BroadcastedColumns[T]] = {
    new InPlaceImpl[Op, BroadcastedColumns[T]] {
      def apply(v: BroadcastedColumns[T]) {
        cc(v.underlying, Axis._0){op(_)}
      }
    }
  }

  implicit def broadcastOp2[Op, T, ColumnType, RHS, OpResult, Result](implicit handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
                                                                     op: UImpl2[Op, ColumnType, RHS, OpResult],
                                                                     cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]):UImpl2[Op, BroadcastedColumns[T], RHS, Result] = {
    new UImpl2[Op, BroadcastedColumns[T], RHS, Result] {
      def apply(v: BroadcastedColumns[T], v2: RHS): Result = {
        cc(v.underlying, Axis._0){op(_, v2)}
      }
    }
  }

  implicit def broadcastInplaceOp2[Op, T, ColumnType, RHS, OpResult](implicit handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
                                                                    op: InPlaceImpl2[Op, ColumnType, RHS],
                                                                    cc: CanIterateAxis[T, Axis._0.type, ColumnType]):InPlaceImpl2[Op, BroadcastedColumns[T], RHS] = {
    new InPlaceImpl2[Op, BroadcastedColumns[T], RHS] {
      def apply(v: BroadcastedColumns[T], v2: RHS) {
        cc(v.underlying, Axis._0){op(_, v2)}
      }
    }
  }



}

