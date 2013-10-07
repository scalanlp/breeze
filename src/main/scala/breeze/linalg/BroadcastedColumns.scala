package breeze.linalg

import breeze.linalg.operators._
import breeze.generic.{CanIterateAxis, CanCollapseAxis}

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
  implicit def broadcastBinaryOpAdd[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpAdd, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpAdd, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpAdd, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpSub[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpSub, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpSub, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpSub, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpMulMatrix[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpMulMatrix, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpMulMatrix, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpMulMatrix, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpMulScalar[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpMulScalar, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpMulScalar, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpMulScalar, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryOpDiv[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpDiv, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpDiv, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpDiv, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryOpSet[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpSet, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpSet, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpSet, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryOpMod[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpMod, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpMod, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpMod, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }


  implicit def broadcastBinaryOpPow[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpMod, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpMod, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpMod, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpLT[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpLT, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpLT, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpLT, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpGT[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpGT, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpGT, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpGT, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpLTE[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpLTE, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpLTE, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpLTE, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpGTE[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpGTE, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpGTE, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpGTE, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpEq[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpEq, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpEq, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpEq, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpNe[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpNe, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpNe, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpNe, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpAnd[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpAnd, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpAnd, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpAnd, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpXor[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpXor, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpXor, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpXor, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }


  // Update Ops.
  implicit def broadcastBinaryUpdateOpAdd[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpAdd], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpAdd] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpAdd] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpSub[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpSub], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpSub] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpSub] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpMulMatrix[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpMulMatrix], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpMulMatrix] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpMulMatrix] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpMulScalar[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpMulScalar], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpMulScalar] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpMulScalar] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryUpdateOpDiv[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpDiv], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpDiv] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpDiv] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryUpdateOpSet[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpSet], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpSet] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpSet] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryUpdateOpMod[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpMod], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpMod] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpMod] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }


  implicit def broadcastBinaryUpdateOpPow[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpMod], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpMod] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpMod] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpLT[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpLT], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpLT] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpLT] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpGT[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpGT], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpGT] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpGT] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpLTE[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpLTE], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpLTE] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpLTE] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpGTE[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpGTE], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpGTE] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpGTE] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpEq[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpEq], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpEq] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpEq] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpNe[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpNe], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpNe] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpNe] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpAnd[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpAnd], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpAnd] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpAnd] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpXor[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpXor], cc: CanIterateAxis[T, Axis._0.type, ColumnType]): BinaryUpdateOp[BroadcastedColumns[T], RHS, OpXor] = {
    new BinaryUpdateOp[BroadcastedColumns[T], RHS, OpXor] {
      def apply(a: BroadcastedColumns[T], b: RHS) {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }


  implicit def broadcastBinaryOpMulInner[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpMulInner, OpResult], cc: CanCollapseAxis[T, Axis._0.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedColumns[T], RHS, OpMulInner, Result] = {
    new BinaryOp[BroadcastedColumns[T], RHS, OpMulInner, Result] {
      def apply(a: BroadcastedColumns[T], b: RHS): Result = {
        cc(a.underlying, Axis._0){op(_,b)}
      }
    }

  }

}

