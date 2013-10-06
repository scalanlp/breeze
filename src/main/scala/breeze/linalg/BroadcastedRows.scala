package breeze.linalg

import breeze.linalg.operators._
import breeze.generic.{CanIterateAxis, CanCollapseAxis}

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
  // I shouldn't need to do one for each operator, but I get diverging implicit errors if i don't.
  implicit def broadcastBinaryOpAdd[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpAdd, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpAdd, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpAdd, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpSub[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpSub, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpSub, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpSub, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpMulMatrix[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpMulMatrix, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpMulMatrix, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpMulMatrix, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpMulScalar[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpMulScalar, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpMulScalar, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpMulScalar, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }
  }

  implicit def broadcastBinaryOpMulInner[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpMulInner, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpMulInner, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpMulInner, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }
  }

  implicit def broadcastBinaryOpDiv[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpDiv, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpDiv, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpDiv, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryOpSet[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpSet, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpSet, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpSet, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryOpMod[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpMod, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpMod, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpMod, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }


  implicit def broadcastBinaryOpPow[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpMod, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpMod, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpMod, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpLT[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpLT, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpLT, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpLT, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpGT[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpGT, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpGT, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpGT, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpLTE[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpLTE, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpLTE, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpLTE, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpGTE[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpGTE, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpGTE, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpGTE, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpEq[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpEq, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpEq, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpEq, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpNe[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpNe, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpNe, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpNe, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpAnd[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpAnd, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpAnd, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpAnd, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryOpXor[T, ColumnType, RHS, OpResult, Result](implicit op: BinaryOp[ColumnType, RHS, OpXor, OpResult], cc: CanCollapseAxis[T, Axis._1.type, ColumnType, OpResult, Result]): BinaryOp[BroadcastedRows[T], RHS, OpXor, Result] = {
    new BinaryOp[BroadcastedRows[T], RHS, OpXor, Result] {
      def apply(a: BroadcastedRows[T], b: RHS): Result = {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }


  // Update Ops.
  implicit def broadcastBinaryUpdateOpAdd[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpAdd], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpAdd] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpAdd] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpSub[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpSub], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpSub] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpSub] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpMulMatrix[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpMulMatrix], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpMulMatrix] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpMulMatrix] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpMulScalar[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpMulScalar], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpMulScalar] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpMulScalar] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryUpdateOpDiv[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpDiv], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpDiv] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpDiv] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryUpdateOpSet[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpSet], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpSet] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpSet] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }
  implicit def broadcastBinaryUpdateOpMod[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpMod], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpMod] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpMod] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }


  implicit def broadcastBinaryUpdateOpPow[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpMod], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpMod] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpMod] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpLT[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpLT], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpLT] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpLT] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpGT[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpGT], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpGT] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpGT] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpLTE[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpLTE], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpLTE] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpLTE] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpGTE[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpGTE], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpGTE] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpGTE] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpEq[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpEq], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpEq] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpEq] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpNe[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpNe], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpNe] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpNe] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpAnd[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpAnd], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpAnd] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpAnd] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }

  implicit def broadcastBinaryUpdateOpXor[T, ColumnType, RHS](implicit op: BinaryUpdateOp[ColumnType, RHS, OpXor], cc: CanIterateAxis[T, Axis._1.type, ColumnType]): BinaryUpdateOp[BroadcastedRows[T], RHS, OpXor] = {
    new BinaryUpdateOp[BroadcastedRows[T], RHS, OpXor] {
      def apply(a: BroadcastedRows[T], b: RHS) {
        cc(a.underlying, Axis._1){op(_,b)}
      }
    }

  }
}

