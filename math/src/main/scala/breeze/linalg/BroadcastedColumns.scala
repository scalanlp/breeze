/*
 *
 *  Copyright 2014 David Hall
 *
 *  Licensed under the Apache License, Version 2.0 (the "License")
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * /
 */

package breeze.linalg

import breeze.generic.UFunc.{InPlaceImpl, InPlaceImpl2, InPlaceImpl3, UImpl, UImpl2, UImpl3}
import breeze.linalg.support._

/**
 * Class for classes that are broadcasting their columns.
 * That is denseMatrix(::, *) /= denseVector
 * @param underlying the tensor (or equivalent) being broadcasted
 * @tparam T the type of the tensor
 */
case class BroadcastedColumns[T, ColType](underlying: T)
    extends BroadcastedLike[T, ColType, BroadcastedColumns[T, ColType]] {
  def repr = this

  def iterator(implicit canIterateAxis: CanIterateAxis[T, Axis._0.type, ColType]) = canIterateAxis(underlying, Axis._0)

  def foldLeft[B](
      z: B
  )(f: (B, ColType) => B)(implicit canTraverseAxis: CanTraverseAxis[T, Axis._0.type, ColType]): B = {
    var acc = z
    canTraverseAxis(underlying, Axis._0) { c =>
      acc = f(acc, c)
    }
    acc
  }

}

object BroadcastedColumns {

  implicit class BroadcastColumnsDMToIndexedSeq[T](bc: BroadcastedColumns[DenseMatrix[T], DenseVector[T]]) {
    def toIndexedSeq: IndexedSeq[DenseVector[T]] = new BroadcastedDMColsISeq(bc.underlying)
  }

  implicit def canMapValues[T, ColumnType, ResultColumn, Result](implicit
      cc: CanCollapseAxis[T, Axis._0.type, ColumnType, ResultColumn, Result]
  ): CanMapValues[BroadcastedColumns[T, ColumnType], ColumnType, ResultColumn, Result] = {
    new CanMapValues[BroadcastedColumns[T, ColumnType], ColumnType, ResultColumn, Result] {
      def apply(from: BroadcastedColumns[T, ColumnType], fn: (ColumnType) => ResultColumn): Result = {
        cc(from.underlying, Axis._0) { fn }
      }
    }
  }

  implicit def scalarOf[T, ColumnType]: ScalarOf[BroadcastedColumns[T, ColumnType], ColumnType] = ScalarOf.dummy

  implicit def broadcastOp[Op, T, ColumnType, OpResult, Result](implicit
      handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
      op: UImpl[Op, ColumnType, OpResult],
      cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]
  ): UImpl[Op, BroadcastedColumns[T, ColumnType], Result] = {
    new UImpl[Op, BroadcastedColumns[T, ColumnType], Result] {
      def apply(v: BroadcastedColumns[T, ColumnType]): Result = {
        cc(v.underlying, Axis._0) { op(_) }
      }
    }
  }

  implicit def broadcastInplaceOp[Op, T, ColumnType, RHS, OpResult](implicit
      handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
      op: InPlaceImpl[Op, ColumnType],
      cc: CanTraverseAxis[T, Axis._0.type, ColumnType]
  ): InPlaceImpl[Op, BroadcastedColumns[T, ColumnType]] = {
    new InPlaceImpl[Op, BroadcastedColumns[T, ColumnType]] {
      def apply(v: BroadcastedColumns[T, ColumnType]): Unit = {
        cc(v.underlying, Axis._0) { op(_) }
      }
    }
  }

  implicit def broadcastOp2[Op, T, ColumnType, RHS, OpResult, Result](implicit
      handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
      op: UImpl2[Op, ColumnType, RHS, OpResult],
      cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]
  ): UImpl2[Op, BroadcastedColumns[T, ColumnType], RHS, Result] = {
    new UImpl2[Op, BroadcastedColumns[T, ColumnType], RHS, Result] {
      def apply(v: BroadcastedColumns[T, ColumnType], v2: RHS): Result = {
        cc(v.underlying, Axis._0) { op(_, v2) }
      }
    }
  }

  implicit def broadcastOp2_2[Op, T, ColumnType, LHS, OpResult, Result](implicit
      handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
      op: UImpl2[Op, LHS, ColumnType, OpResult],
      cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]
  ): UImpl2[Op, LHS, BroadcastedColumns[T, ColumnType], Result] = {
    new UImpl2[Op, LHS, BroadcastedColumns[T, ColumnType], Result] {
      def apply(v: LHS, v2: BroadcastedColumns[T, ColumnType]): Result = {
        cc(v2.underlying, Axis._0) { op(v, _) }
      }
    }
  }

  implicit def broadcastInplaceOp2[Op, T, ColumnType, RHS, OpResult](implicit
      handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
      op: InPlaceImpl2[Op, ColumnType, RHS],
      cc: CanTraverseAxis[T, Axis._0.type, ColumnType]
  ): InPlaceImpl2[Op, BroadcastedColumns[T, ColumnType], RHS] = {
    new InPlaceImpl2[Op, BroadcastedColumns[T, ColumnType], RHS] {
      def apply(v: BroadcastedColumns[T, ColumnType], v2: RHS): Unit = {
        cc(v.underlying, Axis._0) { op(_, v2) }
      }
    }
  }

  implicit def broadcastOp3_1[Op, T, A1, ColumnType, RHS, OpResult, Result](implicit
      handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
      op: UImpl3[Op, A1, ColumnType, RHS, OpResult],
      cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]
  ): UImpl3[Op, A1, BroadcastedColumns[T, ColumnType], RHS, Result] = {
    new UImpl3[Op, A1, BroadcastedColumns[T, ColumnType], RHS, Result] {
      def apply(v: A1, v2: BroadcastedColumns[T, ColumnType], v3: RHS): Result = {
        cc(v2.underlying, Axis._0) { op(v, _, v3) }
      }
    }
  }

  implicit def broadcastInplaceOp3_1[Op, A1, T, ColumnType, RHS, OpResult](implicit
      handhold: CanCollapseAxis.HandHold[T, Axis._0.type, ColumnType],
      op: InPlaceImpl3[Op, A1, ColumnType, RHS],
      cc: CanTraverseAxis[T, Axis._0.type, ColumnType]
  ): InPlaceImpl3[Op, A1, BroadcastedColumns[T, ColumnType], RHS] = {
    new InPlaceImpl3[Op, A1, BroadcastedColumns[T, ColumnType], RHS] {

      override def apply(v: A1, v2: BroadcastedColumns[T, ColumnType], v3: RHS): Unit = {
        cc(v2.underlying, Axis._0) { op(v, _, v3) }
      }
    }
  }

  implicit def canForeachColumns[T, ColumnType, ResultColumn, Result](implicit
      iter: CanTraverseAxis[T, Axis._0.type, ColumnType]
  ): CanForeachValues[BroadcastedColumns[T, ColumnType], ColumnType] = {
    new CanForeachValues[BroadcastedColumns[T, ColumnType], ColumnType] {

      /** Maps all key-value pairs from the given collection. */
      override def foreach[U](from: BroadcastedColumns[T, ColumnType], fn: (ColumnType) => U): Unit = {
        iter(from.underlying, Axis._0)(fn)
      }
    }

  }

  // This is a more memory efficient representation if the sequence is long-lived but rarely accessed.
  @SerialVersionUID(1L)
  class BroadcastedDMColsISeq[T](val underlying: DenseMatrix[T]) extends IndexedSeq[DenseVector[T]] with Serializable {
    override def length: Int = underlying.cols

    override def apply(idx: Int): DenseVector[T] = underlying(::, idx)
  }
}
