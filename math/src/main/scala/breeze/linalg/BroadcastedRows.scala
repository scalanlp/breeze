package breeze.linalg

import breeze.linalg.support._
import breeze.generic.UFunc.{InPlaceImpl, UImpl, InPlaceImpl2, UImpl2}
import breeze.linalg.operators._

/**
 * Class for classes that are broadcasting their rows.
 * That is denseMatrix(*, ::) /= denseVector
 * @param underlying the tensor (or equivalent) being broadcasted
 * @tparam T the type of the tensor
 */
case class BroadcastedRows[T, RowType](underlying: T) extends BroadcastedLike[T, RowType, BroadcastedRows[T, RowType]] {
  def repr = this

  def iterator(implicit canIterateAxis: CanIterateAxis[T, Axis._1.type, RowType]) = canIterateAxis(underlying, Axis._1)

  def foldLeft[B](z: B)(f: (B, RowType) => B)(
      implicit canTraverseAxis: CanTraverseAxis[T, Axis._1.type, RowType]): B = {
    var acc = z
    canTraverseAxis(underlying, Axis._1) { c =>
      acc = f(acc, c)
    }
    acc
  }
}

trait BroadcastedRowsOps {

  implicit def canMapValues_BRows[T, RowType, ResultRow, Result](
      implicit cc: CanCollapseAxis[T, Axis._1.type, RowType, ResultRow, Result])
    : CanMapValues[BroadcastedRows[T, RowType], RowType, ResultRow, Result] = {
    new CanMapValues[BroadcastedRows[T, RowType], RowType, ResultRow, Result] {
      def map(from: BroadcastedRows[T, RowType], fn: (RowType) => ResultRow): Result = {
        cc(from.underlying, Axis._1) { fn }
      }

      // TODO: should we handle sparse here?
      override def mapActive(from: BroadcastedRows[T, RowType], fn: RowType => ResultRow): Result = map(from, fn)
    }
  }

  implicit def broadcastOp_BRows[Op, T, RowType, OpResult, Result](
      implicit handhold: CanCollapseAxis.HandHold[T, Axis._1.type, RowType],
      op: UImpl[Op, RowType, OpResult],
      cc: CanCollapseAxis[T, Axis._1.type, RowType, OpResult, Result])
    : UImpl[Op, BroadcastedRows[T, RowType], Result] = {
    new UImpl[Op, BroadcastedRows[T, RowType], Result] {
      def apply(v: BroadcastedRows[T, RowType]): Result = {
        cc(v.underlying, Axis._1) { op(_) }
      }
    }
  }

  implicit def broadcastInplaceOp_BRows[Op, T, RowType, RHS, OpResult](
      implicit handhold: CanCollapseAxis.HandHold[T, Axis._1.type, RowType],
      op: InPlaceImpl[Op, RowType],
      cc: CanTraverseAxis[T, Axis._1.type, RowType]): InPlaceImpl[Op, BroadcastedRows[T, RowType]] = {
    new InPlaceImpl[Op, BroadcastedRows[T, RowType]] {
      def apply(v: BroadcastedRows[T, RowType]): Unit = {
        cc(v.underlying, Axis._1) { op(_) }
      }
    }
  }

  implicit def broadcastOp2_BRows[Op, T, RowType, RHS, OpResult, Result](
      implicit handhold: CanCollapseAxis.HandHold[T, Axis._1.type, RowType],
      op: UImpl2[Op, RowType, RHS, OpResult],
      cc: CanCollapseAxis[T, Axis._1.type, RowType, OpResult, Result])
    : UImpl2[Op, BroadcastedRows[T, RowType], RHS, Result] = {
    new UImpl2[Op, BroadcastedRows[T, RowType], RHS, Result] {
      def apply(v: BroadcastedRows[T, RowType], v2: RHS): Result = {
        cc(v.underlying, Axis._1) { op(_, v2) }
      }
    }
  }

  implicit def broadcastOp2_2_BRows[Op, T, RowType, LHS, OpResult, Result](
      implicit handhold: CanCollapseAxis.HandHold[T, Axis._1.type, RowType],
      op: UImpl2[Op, LHS, RowType, OpResult],
      cc: CanCollapseAxis[T, Axis._1.type, RowType, OpResult, Result])
  : UImpl2[Op, LHS, BroadcastedRows[T, RowType], Result] = {
    new UImpl2[Op, LHS, BroadcastedRows[T, RowType], Result] {
      def apply(v: LHS, v2: BroadcastedRows[T, RowType]): Result = {
        cc(v2.underlying, Axis._1) { op(v, _) }
      }
    }
  }

  implicit def broadcastInplaceOp2_BRows[Op, T, RowType, RHS, OpResult](
      implicit handhold: CanCollapseAxis.HandHold[T, Axis._1.type, RowType],
      op: InPlaceImpl2[Op, RowType, RHS],
      cc: CanTraverseAxis[T, Axis._1.type, RowType]): InPlaceImpl2[Op, BroadcastedRows[T, RowType], RHS] = {
    new InPlaceImpl2[Op, BroadcastedRows[T, RowType], RHS] {
      def apply(v: BroadcastedRows[T, RowType], v2: RHS): Unit = {
        cc(v.underlying, Axis._1) { op(_, v2) }
      }
    }
  }

  implicit def canForeachRows_BRows[T, RowType, ResultRow, Result](implicit iter: CanTraverseAxis[T, Axis._1.type, RowType])
    : CanForeachValues[BroadcastedRows[T, RowType], RowType] = {
    new CanForeachValues[BroadcastedRows[T, RowType], RowType] {

      /** Maps all key-value pairs from the given collection. */
      override def foreach[U](from: BroadcastedRows[T, RowType], fn: (RowType) => U): Unit = {
        iter(from.underlying, Axis._1)(fn)
      }
    }

  }


}

object BroadcastedRows {
  // This is a more memory efficient representation if the sequence is long-lived but rarely accessed.
  @SerialVersionUID(1L)
  class BroadcastedDMRowsISeq[T](val underlying: DenseMatrix[T])
    extends IndexedSeq[Transpose[DenseVector[T]]]
      with Serializable {
    override def length: Int = underlying.rows

    override def apply(idx: Int): Transpose[DenseVector[T]] = underlying(idx, ::)
  }

  implicit def scalarOf_BRows[T, RowType]: ScalarOf[BroadcastedRows[T, RowType], RowType] = ScalarOf.dummy

  implicit class BroadcastRowsDMToIndexedSeq[T](bc: BroadcastedRows[DenseMatrix[T], DenseVector[T]]) {
    def toIndexedSeq: IndexedSeq[Transpose[DenseVector[T]]] = new BroadcastedRows.BroadcastedDMRowsISeq[T](bc.underlying)
  }
}

