package breeze.linalg

import breeze.linalg.operators._
import breeze.math.{Field, VectorSpace}

/**
 *
 * @author dlwh
 */
trait NumericOps[+This] {
  def repr : This

  final def unary_-[TT>:This,That](implicit op : UnaryOp[TT,OpNeg,That]) = op(repr)

  final def unary_![TT>:This,That](implicit op : UnaryOp[TT,OpNot,That]) = op(repr)

  /** Element-wise sum of this and b. */
  final def :+ [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpAdd,That]) = op(repr,b)

  /** Element-wise difference of this and b. */
  final def :- [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpSub,That]) = op(repr,b)

  /** Element-wise product of this and b. */
  final def :* [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpMulScalar,That]) = op(repr,b)

  /** Element-wise quotient of this and b. */
  final def :/ [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpDiv,That]) = op(repr,b)

  /** Element-wise modulo of this and b. */
  final def :% [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpMod,That]) = op(repr,b)

  /** Element-wise exponentiation of this and b. */
  final def :^ [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpPow,That]) = op(repr,b)

  /** Element-wise less=than comparator of this and b. */
  final def :< [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpLT,That]) = op(repr,b)

  /** Element-wise less-than-or-equal-to comparator of this and b. */
  final def :<= [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpLTE,That]) = op(repr,b)

  /** Element-wise greater-than comparator of this and b. */
  final def :> [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpGT,That]) = op(repr,b)

  /** Element-wise greater-than-or-equal-to comparator of this and b. */
  final def :>= [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpGTE,That]) = op(repr,b)

  /** Element-wise equality comparator of this and b. */
  final def :== [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpEq,That]) = op(repr,b)

  /** Element-wise inequality comparator of this and b. */
  final def :!= [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpNe,That]) = op(repr,b)

  /** Element-wise logical "and" operator -- returns true if corresponding elements are non-zero. */
  final def :&& [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpAnd,That]) = op(repr,b)

  /** Element-wise logical "or" operator -- returns true if either element is non-zero. */
  final def :|| [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpOr,That]) = op(repr,b)

  /** Element-wise logical "xor" operator -- returns true if only one of the corresponding elements is non-zero. */
  final def :^^ [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpXor,That]) = op(repr,b)

  /** Inner product of this and b. */
  final def dot [TT>:This,B,BB>:B, That](b : B)(implicit op : BinaryOp[TT,BB,OpMulInner,That]) = op(repr,b)

  //
  // Operator aliases
  //

  /** Alias for :+(b) for all b. */
  final def + [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpAdd,That]) = {
    op(repr, b)
  }

  /** Alias for :-(b) for all b. */
  final def - [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpSub,That]) = {
    op(repr, b)
  }

  /** Matrix multiplication */
  final def * [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpMulMatrix,That]) = {
    op(repr, b)
  }

  /** Alias for :/(b) when b is a scalar. */
  final def / [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpDiv,That]) = {
    op(repr, b)
  }

  /** Alias for :%(b) when b is a scalar. */
  final def % [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpMod,That]) = {
    op(repr, b)
  }

  /** Alias for :&&(b) for all b. */
  final def && [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpAnd,That]) = {
    op(repr, b)
  }

  /** Alias for :||(b) for all b. */
  final def || [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpOr,That]) = {
    op(repr, b)
  }

  /** Alias for :^^(b) for all b. */
  final def ^^ [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpXor,That]):That = {
    op(repr, b)
  }

  // Mutable Ops
  /** Mutates this by element-wise assignment of b into this. */
  final def := [TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpSet]) : This = {
    op(repr,b)
    repr
  }

  /** Mutates this by element-wise addition of b into this. */
  final def :+= [TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpAdd]) : This = {
    op(repr,b)
    repr
  }

  /** Mutates this by element-wise subtraction of b from this */
  final def :-= [TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpSub]) : This = {
    op(repr,b)
    repr
  }

  /** Mutates this by element-wise multiplication of b into this. */
  final def :*= [TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpMulScalar]) : This = {
    op(repr,b)
    repr
  }

  /** Mutates this by element-wise division of b into this */
  final def :/= [TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpDiv]) : This = {
    op(repr,b)
    repr
  }

  /** Mutates this by element-wise modulo of b into this. */
  final def :%= [TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpMod]) : This = {
    op(repr,b)
    repr
  }

  /** Mutates this by element-wise exponentiation of this by b. */
  final def :^= [TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpPow]) : This = {
    op(repr,b)
    repr
  }

  /** Alias for :+=(b) for all b. */
  final def += [TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpAdd]) =
    this.:+=[TT,B](b)

  /** Returns the operator delegate used in += */
  final def +=?[TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpAdd]) = op

  /** Alias for :-=(b) for all b. */
  final def -= [TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpSub]) =
    this.:-=[TT,B](b)

  /** Alias for :*=(b) when b is a scalar. */
  final def *= [TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpMulScalar]) =
    this.:*=[TT,B](b)

  /** Alias for :/=(b) when b is a scalar. */
  final def /= [TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpDiv]) =
    this.:/=[TT,B](b)

  /** Alias for :%=(b) when b is a scalar. */
  final def %= [TT>:This,B](b : B)(implicit op : BinaryUpdateOp[TT,B,OpMod]) =
    this.:%=[TT,B](b)

  // matrix-y ops

  /** A transposed view of this column. */
  final def t [TT>:This,That](implicit op : CanTranspose[TT,That]) =
    op.apply(repr)

  /** Shaped solve of this by b. */
  def \ [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpSolveMatrixBy,That]) =
    op.apply(repr,b)
}

object NumericOps {
  object Arrays {
    implicit def arrayIsNumericOps[V](arr: Array[V]):NumericOps[Array[V]] = new NumericOps[Array[V]] {
      def repr = arr
    }

    implicit def binaryOpFromDVOp[V,Other,Op<:OpType,U](implicit op: BinaryOp[DenseVector[V], Other, Op, DenseVector[U]], man: ClassManifest[U]) = {
      new BinaryOp[Array[V], Other, Op, Array[U]] {
        def apply(a: Array[V], b: Other): Array[U] = {
          val r = op(new DenseVector(a),b)
          if(r.offset != 0 || r.stride != 1) {
            val z = DenseVector.zeros[U](r.length)
            z := r
            z.data
          } else {
            r.data
          }
        }
      }
    }

    implicit def binaryUpdateOpFromDVOp[V,Other,Op<:OpType, U](implicit op: BinaryUpdateOp[DenseVector[V], Other, Op], man: ClassManifest[U]) = {
      new BinaryUpdateOp[Array[V], Other, Op] {
        def apply(a: Array[V], b: Other){
          op(new DenseVector(a),b)
        }
      }
    }
  }
}
