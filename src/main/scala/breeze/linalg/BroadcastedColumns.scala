package breeze.linalg

import breeze.linalg.operators._
import breeze.linalg.support.{CanCollapseAxis, CanIterateAxis}

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
  // I shouldn't need to do one for each operator, but I get diverging implicit errors if i don't.
  implicit def broadcastBinaryOpAdd[T, ColumnType, RHS, OpResult, Result](implicit op: OpAdd.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpAdd.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpAdd.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpSub[T, ColumnType, RHS, OpResult, Result](implicit op: OpSub.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpSub.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpSub.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpMulMatrix[T, ColumnType, RHS, OpResult, Result](implicit op: OpMulMatrix.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpMulMatrix.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpMulMatrix.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpMulScalar[T, ColumnType, RHS, OpResult, Result](implicit op: OpMulScalar.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpMulScalar.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpMulScalar.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryOpDiv[T, ColumnType, RHS, OpResult, Result](implicit op: OpDiv.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpDiv.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpDiv.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryOpSet[T, ColumnType, RHS, OpResult, Result](implicit op: OpSet.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpSet.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpSet.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryOpMod[T, ColumnType, RHS, OpResult, Result](implicit op: OpMod.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpMod.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpMod.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }


  implicit def broadcastBinaryOpPow[T, ColumnType, RHS, OpResult, Result](implicit op: OpMod.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpMod.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpMod.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpLT[T, ColumnType, RHS, OpResult, Result](implicit op: OpLT.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpLT.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpLT.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpGT[T, ColumnType, RHS, OpResult, Result](implicit op: OpGT.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpGT.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpGT.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpLTE[T, ColumnType, RHS, OpResult, Result](implicit op: OpLTE.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpLTE.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpLTE.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpGTE[T, ColumnType, RHS, OpResult, Result](implicit op: OpGTE.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpGTE.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpGTE.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpEq[T, ColumnType, RHS, OpResult, Result](implicit op: OpEq.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpEq.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpEq.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpNe[T, ColumnType, RHS, OpResult, Result](implicit op: OpNe.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpNe.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpNe.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpAnd[T, ColumnType, RHS, OpResult, Result](implicit op: OpAnd.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpAnd.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpAnd.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpXor[T, ColumnType, RHS, OpResult, Result](implicit op: OpXor.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpXor.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpXor.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }


  // Update Ops.
  implicit def broadcastBinaryUpdateOpAdd[T, ColumnType, RHS](implicit op: OpAdd.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpAdd.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpAdd.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpSub[T, ColumnType, RHS](implicit op: OpSub.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpSub.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpSub.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpMulMatrix[T, ColumnType, RHS](implicit op: OpMulMatrix.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpMulMatrix.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpMulMatrix.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpMulScalar[T, ColumnType, RHS](implicit op: OpMulScalar.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpMulScalar.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpMulScalar.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryUpdateOpDiv[T, ColumnType, RHS](implicit op: OpDiv.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpDiv.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpDiv.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryUpdateOpSet[T, ColumnType, RHS](implicit op: OpSet.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpSet.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpSet.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryUpdateOpMod[T, ColumnType, RHS](implicit op: OpMod.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpMod.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpMod.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }


  implicit def broadcastBinaryUpdateOpPow[T, ColumnType, RHS](implicit op: OpMod.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpMod.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpMod.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpLT[T, ColumnType, RHS](implicit op: OpLT.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpLT.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpLT.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpGT[T, ColumnType, RHS](implicit op: OpGT.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpGT.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpGT.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpLTE[T, ColumnType, RHS](implicit op: OpLTE.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpLTE.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpLTE.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpGTE[T, ColumnType, RHS](implicit op: OpGTE.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpGTE.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpGTE.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpEq[T, ColumnType, RHS](implicit op: OpEq.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpEq.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpEq.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpNe[T, ColumnType, RHS](implicit op: OpNe.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpNe.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpNe.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpAnd[T, ColumnType, RHS](implicit op: OpAnd.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpAnd.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpAnd.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpXor[T, ColumnType, RHS](implicit op: OpXor.InPlaceImpl2[ColumnType, RHS], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): OpXor.InPlaceImpl2[BroadcastedColumns[T], RHS] = {
    new OpXor.InPlaceImpl2[BroadcastedColumns[T], RHS] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }


  implicit def broadcastBinaryOpMulInner[T, ColumnType, RHS, OpResult, Result](implicit op: OpMulInner.Impl2[ColumnType, RHS, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): OpMulInner.Impl2[BroadcastedColumns[T], RHS, Result] = {
    new OpMulInner.Impl2[BroadcastedColumns[T], RHS, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

}

