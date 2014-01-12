package breeze.linalg.operators

import breeze.generic.UFunc
import breeze.math.{Field, Semiring, Ring}

/*
 Copyright 2012 Daniel Ramage

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


/**
 * Marker sealed trait for some operation, be it UnaryOp, BinaryOp, or
 * BinaryUpdateOp.
 *
 * @author dramage
 */
sealed trait OpType


/**
 * Type marker for BinaryOp A :+ B and BinaryUpdateOp A :+= B.
 *
 * @author dramage
 */
sealed trait OpAdd extends OpType
object OpAdd extends OpAdd with UFunc {
  implicit def opAddFromSemiring[S:Semiring]: Impl2[S, S, S] = new Impl2[S, S, S] {
    def apply(v: S, v2: S): S = implicitly[Semiring[S]].+(v, v2)
  }
}

/**
 * Type marker for BinaryOp A :- B and BinaryUpdateOp A :-= B.
 *
 * @author dramage
 */
sealed trait OpSub extends OpType
object OpSub extends OpSub with UFunc {
  implicit def opSubFromRing[S:Ring]: Impl2[S, S, S] = new Impl2[S, S, S] {
    def apply(v: S, v2: S): S = implicitly[Ring[S]].-(v, v2)
  }
}

/**
 * Type marker for BinaryOp A :* B and BinaryUpdateOp A :*= B.
 *
 * @author dramage
 */
sealed trait OpMulScalar extends OpType
object OpMulScalar extends OpMulScalar with UFunc {
  implicit def opMulScalarFromSemiring[S:Semiring]: Impl2[S, S, S] = new Impl2[S, S, S] {
    def apply(v: S, v2: S): S = implicitly[Semiring[S]].*(v, v2)
  }
}

/**
 * Type marker for BinaryOp A :/ B and BinaryUpdateOp A:/= B.
 *
 * @author dramage
 */
sealed trait OpDiv extends OpType
object OpDiv extends OpDiv with UFunc {
  implicit def opDivFromField[S:Field]: Impl2[S, S, S] = new Impl2[S, S, S] {
    def apply(v: S, v2: S): S = implicitly[Field[S]]./(v, v2)
  }
}

/**
 * Type marker for BinaryOp A :% B and BinaryUpdateOp A:%= B.
 *
 * @author dramage
 */
sealed trait OpMod extends OpType
object OpMod extends OpMod with UFunc

/**
 * Type marker for BinaryOp A :^ B and BinaryUpdateOp A:^= B.
 *
 * @author dramage
 */
sealed trait OpPow extends OpType
object OpPow extends OpPow with UFunc

/**
 * Type marker for BinaryOp A :&lt B.
 *
 * @author dramage
 */
sealed trait OpLT  extends OpType
object OpLT  extends OpLT with UFunc

/**
 * Type marker for BinaryOp A :&lt= B.
 *
 * @author dramage
 */
sealed trait OpLTE extends OpType
object OpLTE extends OpLTE with UFunc

/**
 * Type marker for BinaryOp A :&gt B.
 *
 * @author dramage
 */
sealed trait OpGT  extends OpType
object OpGT  extends OpGT with UFunc

/**
 * Type marker for BinaryOp A :&gt= B.
 *
 * @author dramage
 */
sealed trait OpGTE extends OpType
object OpGTE extends OpGTE with UFunc

/**
 * Type marker for BinaryOp A :== B.
 *
 * @author dramage
 */
sealed trait OpEq  extends OpType
object OpEq  extends OpEq with UFunc

/**
 * Type marker for BinaryOp A :!= B.
 *
 * @author dramage
 */
sealed trait OpNe  extends OpType
object OpNe  extends OpNe with UFunc

/**
 * Type marker for BinaryUpdateOp A := B.
 *
 * @author dramage
 */
sealed trait OpSet extends OpType
object OpSet extends OpSet with UFunc

/**
 * Type marker for BinaryOp A :&& B
 *
 * @author dramage
 */
sealed trait OpAnd extends OpType
object OpAnd extends OpAnd with UFunc

/**
 * Type marker for BinaryOp A :|| B
 *
 * @author dramage
 */
sealed trait OpOr extends OpType
object OpOr extends OpOr with UFunc

/**
 * Type marker for BinaryOp A :^^ B
 *
 * @author dramage
 */
sealed trait OpXor extends OpType
object OpXor extends OpXor with UFunc

/**
 * Type marker for UnaryOp -A.
 *
 * @author dramage
 */
sealed trait OpNeg extends OpType
object OpNeg extends OpNeg with UFunc {
  implicit def ringNegation[S:Ring] = new Impl[S, S] {
    def apply(v: S): S = implicitly[Ring[S]].negate(v)
  }
}

/**
 * Type marker for UnaryOp !A.
 *
 * @author dramage
 */
sealed trait OpNot extends OpType
object OpNot extends OpNot with UFunc


/**
 * Type marker for inner (dot) product of A and B.
 *
 * @author dramage
 */
sealed trait OpMulInner extends OpType
object OpMulInner extends OpMulInner with UFunc {
  def opMulInnerFromSemiring[S:Semiring]: OpMulInner.Impl2[S, S, S] = new Impl2[S, S, S] {
    def apply(v: S, v2: S): S = implicitly[Semiring[S]].*(v, v2)
  }
}


/**
 * Type marker for BinaryOp A \ B when A is a matrix.
 *
 * @author dramage
 */
sealed trait OpSolveMatrixBy extends OpType
object OpSolveMatrixBy extends OpSolveMatrixBy with UFunc


/**
 * Type marker for inner (dot) product of A and B.
 *
 * @author dramage
 */
sealed trait OpMulMatrix extends OpType
object OpMulMatrix extends OpMulMatrix with UFunc

