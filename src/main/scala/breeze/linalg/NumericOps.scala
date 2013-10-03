package breeze.linalg
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import breeze.linalg.operators._
import breeze.math.{Ring, Field, VectorSpace}
import support.CanSlice2
import breeze.storage.DefaultArrayValue
import scala.reflect.ClassTag

/**
 * In some sense, this is the real root of the linalg hierarchy. It provides
 * methods for doing operations on a Tensor-like thing. All methods farm out to some implicit or another.
 * We use this when we don't care about the index into the Tensor, or if we don't really have an index.
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

  /** A transposed view of this object. */
  final def t [TT>:This,That](implicit op : CanTranspose[TT,That]) =
    op.apply(repr)

  /** Shaped solve of this by b. */
  def \ [TT>:This,B,That](b : B)(implicit op : BinaryOp[TT,B,OpSolveMatrixBy,That]) =
    op.apply(repr,b)


  /** A transposed view of this object, followed by a slice. Sadly frequently necessary. */
  final def t[TT>:This,That,Slice1,Slice2,Result](a: Slice1,
                                                  b: Slice2)
                                                 (implicit op : CanTranspose[TT,That],
                                                  canSlice: CanSlice2[That, Slice1, Slice2, Result]):Result =
    canSlice(op.apply(repr), a, b)
}

object NumericOps {

  /*
  implicit class ScalarsAreNumericOps[@specialized(Int, Double, Long, Float) S](x: S) extends NumericOps[S] {
    def repr: S = x
  }
  */


  /**
   * If you import this object's members, you can treat Arrays as DenseVectors.
   */
  object Arrays extends ArraysLowPriority {
    implicit class ArrayIsNumericOps[V](arr: Array[V])extends NumericOps[Array[V]] {
      def repr = arr
    }

    // TODO these two really shouldn't be necessary, but there's interference(?) from any2StringAdd, or something.
    implicit def binaryOpFromDVOp2Add[V](implicit op: BinaryOp[DenseVector[V], DenseVector[V], OpAdd, DenseVector[V]]): BinaryOp[Array[V], Array[V], OpAdd, Array[V]] = {
      new BinaryOp[Array[V], Array[V], OpAdd, Array[V]] {
        def apply(a: Array[V], b: Array[V]): Array[V] = {
          val r = op(new DenseVector(a),new DenseVector[V](b))
          if(r.offset != 0 || r.stride != 1) {
            r.copy.data
          } else {
            r.data
          }
        }
      }
    }

    implicit def binaryOpAddFromDVUOpAdd2[V](implicit op: BinaryOp[DenseVector[V], V, OpAdd, DenseVector[V]]) = {
      new BinaryOp[Array[V], V, OpAdd, Array[V]] {
        def apply(a: Array[V], b: V): Array[V] = {
          val r = op(new DenseVector(a), b)
          if(r.offset != 0 || r.stride != 1) {
            r.copy.data
          } else {
            r.data
          }
        }
      }
    }


    implicit def binaryOpFromDVOp2[V,Op<:OpType](implicit op: BinaryOp[DenseVector[V], DenseVector[V], Op, DenseVector[V]]): BinaryOp[Array[V], Array[V], Op, Array[V]] = {
      new BinaryOp[Array[V], Array[V], Op, Array[V]] {
        def apply(a: Array[V], b: Array[V]): Array[V] = {
          val r = op(new DenseVector(a),new DenseVector[V](b))
          if(r.offset != 0 || r.stride != 1) {
            r.copy.data
          } else {
            r.data
          }
        }
      }
    }


    implicit def binaryUpdateOpFromDVDVOp[V,Op<:OpType](implicit op: BinaryUpdateOp[DenseVector[V], DenseVector[V], Op]) = {
      new BinaryUpdateOp[Array[V], Array[V], Op] {
        def apply(a: Array[V], b: Array[V]){
          op(new DenseVector(a),new DenseVector(b))
        }
      }
    }

    implicit def binaryOpFromDVUOp2[V,Op<:OpType](implicit op: BinaryOp[DenseVector[V], V, Op, DenseVector[V]]) = {
      new BinaryOp[Array[V], V, Op, Array[V]] {
        def apply(a: Array[V], b: V): Array[V] = {
          val r = op(new DenseVector(a), b)
          if(r.offset != 0 || r.stride != 1) {
            r.copy.data
          } else {
            r.data
          }
        }
      }
    }




  }

  sealed trait ArraysLowPriority {
    implicit def binaryUpdateOpFromDVOp[V,Other,Op<:OpType, U](implicit op: BinaryUpdateOp[DenseVector[V], Other, Op], man: ClassTag[U]) = {
      new BinaryUpdateOp[Array[V], Other, Op] {
        def apply(a: Array[V], b: Other){
          op(new DenseVector(a),b)
        }
      }
    }


    implicit def binaryOpFromDVOp[V,Other,Op<:OpType,U](implicit op: BinaryOp[DenseVector[V], Other, Op, DenseVector[U]], 
                                                        man: ClassTag[U],
                                                        dav: DefaultArrayValue[U]) = {
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
  }

  implicit def binaryUpdateOpFromDVVOp[V,Op<:OpType, U](implicit op: BinaryUpdateOp[DenseVector[V], U, Op], man: ClassTag[U]) = {
      new BinaryUpdateOp[Array[V], U, Op] {
        def apply(a: Array[V], b:U){
          op(new DenseVector(a),b)
        }
      }
    }

}
