package breeze.linalg

import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.generic.UFunc.{InPlaceImpl, UImpl, InPlaceImpl2, UImpl2}

/**
 * Class for classes that are broadcasting their columns.
 * That is denseMatrix(::, *) /= denseVector
 * @param underlying the tensor (or equivalent) being broadcasted
 * @tparam T the type of the tensor
 */
case class BroadcastedColumns[T, B](underlying: T) extends BroadcastedLike[T, B, BroadcastedColumns[T, B]] {
  def repr = this

}

object BroadcastedColumns {

  implicit def canMapValues[T, ColumnType, ResultColumn, Result]
                            (implicit cc: CanCollapseAxis[T, Axis._0.type, ColumnType, ResultColumn, Result])
                            :CanMapValues[BroadcastedColumns[T, ColumnType], ColumnType, ResultColumn, Result] = {
    new CanMapValues[BroadcastedColumns[T, ColumnType], ColumnType, ResultColumn, Result] {
      def map(from: BroadcastedColumns[T, ColumnType], fn: (ColumnType) => ResultColumn): Result = {
        cc(from.underlying, Axis._0){fn}
      }

      /** Maps all active key-value pairs from the given collection. */
      def mapActive(from: BroadcastedColumns[T, ColumnType], fn: (ColumnType) => ResultColumn): Result = {
        cc(from.underlying, Axis._0){fn}
      }
    }

  }


  implicit def broadcastOp[Op, T, ColumnType, OpResult, Result](implicit handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
                                                                     op: UImpl[Op, ColumnType, OpResult],
                                                                     cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]):UImpl[Op, BroadcastedColumns[T, ColumnType], Result] = {
    new UImpl[Op, BroadcastedColumns[T, ColumnType], Result] {
      def apply(v: BroadcastedColumns[T, ColumnType]): Result = {
        cc(v.underlying, Axis._0){op(_)}
      }
    }
  }

  implicit def broadcastInplaceOp[Op, T, ColumnType, RHS, OpResult](implicit handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
                                                                     op: InPlaceImpl[Op, ColumnType],
                                                                     cc: CanIterateAxis[T, Axis._0.type, ColumnType]):InPlaceImpl[Op, BroadcastedColumns[T, ColumnType]] = {
    new InPlaceImpl[Op, BroadcastedColumns[T, ColumnType]] {
      def apply(v: BroadcastedColumns[T, ColumnType]) {
        cc(v.underlying, Axis._0){op(_)}
      }
    }
  }

  implicit def broadcastOp2[Op, T, ColumnType, RHS, OpResult, Result](implicit handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
                                                                     op: UImpl2[Op, ColumnType, RHS, OpResult],
                                                                     cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]):UImpl2[Op, BroadcastedColumns[T, ColumnType], RHS, Result] = {
    new UImpl2[Op, BroadcastedColumns[T, ColumnType], RHS, Result] {
      def apply(v: BroadcastedColumns[T, ColumnType], v2: RHS): Result = {
        cc(v.underlying, Axis._0){op(_, v2)}
      }
    }
  }

  implicit def broadcastInplaceOp2[Op, T, ColumnType, RHS, OpResult](implicit handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
                                                                    op: InPlaceImpl2[Op, ColumnType, RHS],
                                                                    cc: CanIterateAxis[T, Axis._0.type, ColumnType]):InPlaceImpl2[Op, BroadcastedColumns[T, ColumnType], RHS] = {
    new InPlaceImpl2[Op, BroadcastedColumns[T, ColumnType], RHS] {
      def apply(v: BroadcastedColumns[T, ColumnType], v2: RHS) {
        cc(v.underlying, Axis._0){op(_, v2)}
      }
    }
  }



}

