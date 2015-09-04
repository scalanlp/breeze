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

  implicit def handholdCMV[T, ColumnType] = new CanMapValues.HandHold[BroadcastedColumns[T, ColumnType], ColumnType]


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


  implicit def canForeachColumns[T, ColumnType, ResultColumn, Result]
  (implicit iter: CanIterateAxis[T, Axis._0.type, ColumnType]):CanForeachValues[BroadcastedColumns[T, ColumnType], ColumnType] = {
    new CanForeachValues[BroadcastedColumns[T, ColumnType], ColumnType] {
      /** Maps all key-value pairs from the given collection. */
      override def foreach[U](from: BroadcastedColumns[T, ColumnType], fn: (ColumnType) => U): Unit = {
        iter(from.underlying, Axis._0)(fn)
      }
    }

  }

}

