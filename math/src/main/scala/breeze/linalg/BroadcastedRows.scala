package breeze.linalg

import breeze.linalg.support._
import breeze.generic.UFunc.{InPlaceImpl, UImpl, InPlaceImpl2, UImpl2}

/**
 * Class for classes that are broadcasting their rows.
 * That is denseMatrix(*, ::) /= denseVector
 * @param underlying the tensor (or equivalent) being broadcasted
 * @tparam T the type of the tensor
 */
case class BroadcastedRows[T, RowType](underlying: T) extends BroadcastedLike[T, RowType, BroadcastedRows[T, RowType]] {
  def repr = this
}

object BroadcastedRows {


  implicit def canMapValues[T, RowType, ResultRow, Result]
  (implicit cc: CanCollapseAxis[T, Axis._1.type, RowType, ResultRow, Result])
  :CanMapValues[BroadcastedRows[T, RowType], RowType, ResultRow, Result] = {
    new CanMapValues[BroadcastedRows[T, RowType], RowType, ResultRow, Result] {
      def map(from: BroadcastedRows[T, RowType], fn: (RowType) => ResultRow): Result = {
        cc(from.underlying, Axis._1){fn}
      }

      /** Maps all active key-value pairs from the given collection. */
      def mapActive(from: BroadcastedRows[T, RowType], fn: (RowType) => ResultRow): Result = {
        cc(from.underlying, Axis._1){fn}
      }
    }

  }

  implicit def scalarOf[T, RowType]: ScalarOf[BroadcastedRows[T, RowType], RowType] = ScalarOf.dummy


  implicit def broadcastOp[Op, T, RowType, OpResult, Result](implicit handhold: CanCollapseAxis.HandHold[T, Axis._1.type, RowType],
                                                                op: UImpl[Op, RowType, OpResult],
                                                                cc: CanCollapseAxis[T, Axis._1.type, RowType, OpResult, Result]):UImpl[Op, BroadcastedRows[T, RowType], Result] = {
    new UImpl[Op, BroadcastedRows[T, RowType], Result] {
      def apply(v: BroadcastedRows[T, RowType]): Result = {
        cc(v.underlying, Axis._1){op(_)}
      }
    }
  }

  implicit def broadcastInplaceOp[Op, T, RowType, RHS, OpResult](implicit handhold: CanCollapseAxis.HandHold[T, Axis._1.type, RowType],
                                                                    op: InPlaceImpl[Op, RowType],
                                                                    cc: CanIterateAxis[T, Axis._1.type, RowType]):InPlaceImpl[Op, BroadcastedRows[T, RowType]] = {
    new InPlaceImpl[Op, BroadcastedRows[T, RowType]] {
      def apply(v: BroadcastedRows[T, RowType]) {
        cc(v.underlying, Axis._1){op(_)}
      }
    }
  }

  implicit def broadcastOp2[Op, T, RowType, RHS, OpResult, Result](implicit handhold: CanCollapseAxis.HandHold[T, Axis._1.type, RowType],
                                                                     op: UImpl2[Op, RowType, RHS, OpResult],
                                                                     cc: CanCollapseAxis[T, Axis._1.type, RowType, OpResult, Result]):UImpl2[Op, BroadcastedRows[T, RowType], RHS, Result] = {
    new UImpl2[Op, BroadcastedRows[T, RowType], RHS, Result] {
      def apply(v: BroadcastedRows[T, RowType], v2: RHS): Result = {
        cc(v.underlying, Axis._1){op(_, v2)}
      }
    }
  }

  implicit def broadcastInplaceOp2[Op, T, RowType, RHS, OpResult](implicit handhold: CanCollapseAxis.HandHold[T, Axis._1.type, RowType],
                                                                    op: InPlaceImpl2[Op, RowType, RHS],
                                                                    cc: CanIterateAxis[T, Axis._1.type, RowType]):InPlaceImpl2[Op, BroadcastedRows[T, RowType], RHS] = {
    new InPlaceImpl2[Op, BroadcastedRows[T, RowType], RHS] {
      def apply(v: BroadcastedRows[T, RowType], v2: RHS) {
        cc(v.underlying, Axis._1){op(_, v2)}
      }
    }
  }

  implicit def canForeachRows[T, RowType, ResultRow, Result]
  (implicit iter: CanIterateAxis[T, Axis._1.type, RowType]):CanForeachValues[BroadcastedRows[T, RowType], RowType] = {
    new CanForeachValues[BroadcastedRows[T, RowType], RowType] {
      /** Maps all key-value pairs from the given collection. */
      override def foreach[U](from: BroadcastedRows[T, RowType], fn: (RowType) => U): Unit = {
        iter(from.underlying, Axis._1)(fn)
      }
    }

  }

}
