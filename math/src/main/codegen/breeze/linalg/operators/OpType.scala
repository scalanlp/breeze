package breeze.linalg.operators

import breeze.generic.UFunc.UImpl
import breeze.generic.{ElementwiseUFunc, MappingUFunc, UFunc}
import breeze.macros.expand
import breeze.math.{Field, Ring, Semiring}

/*
 Copyright 2017 David Hall, based on code by:
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
sealed trait OpType extends UFunc

/**
 * Type marker for BinaryOp A :+ B and BinaryUpdateOp A :+= B.
 *
 * @author dramage
 */
object OpAdd extends OpType with ElementwiseUFunc {
  implicit def opAddFromSemiring[S: Semiring]: Impl2[S, S, S] = new Impl2[S, S, S] {
    def apply(v: S, v2: S): S = implicitly[Semiring[S]].+(v, v2)
  }
}

/**
 * Type marker for BinaryOp A -:- B and BinaryUpdateOp A :-= B.
 *
 * @author dramage
 */
object OpSub extends OpType with ElementwiseUFunc {
  implicit def opSubFromRing[S: Ring]: Impl2[S, S, S] = new Impl2[S, S, S] {
    def apply(v: S, v2: S): S = implicitly[Ring[S]].-(v, v2)
  }
}

/**
 * Type marker for BinaryOp A *:* B and BinaryUpdateOp A :*= B.
 *
 * @author dramage
 */
object OpMulScalar extends OpType with ElementwiseUFunc {
  implicit def opMulScalarFromSemiring[S: Semiring]: Impl2[S, S, S] = new Impl2[S, S, S] {
    def apply(v: S, v2: S): S = implicitly[Semiring[S]].*(v, v2)
  }
}

/**
 * Type marker for BinaryOp A :/ B and BinaryUpdateOp A:/= B.
 *
 * @author dramage
 */
object OpDiv extends OpType with ElementwiseUFunc {
  implicit def opDivFromField[S: Field]: Impl2[S, S, S] = new Impl2[S, S, S] {
    def apply(v: S, v2: S): S = implicitly[Field[S]]./(v, v2)
  }
}

/**
 * Type marker for BinaryOp A :% B and BinaryUpdateOp A:%= B.
 *
 * @author dramage
 */
object OpMod extends OpType with ElementwiseUFunc

/**
 * Type marker for BinaryOp A :^ B and BinaryUpdateOp A:^= B.
 *
 * @author dramage
 */
object OpPow extends OpType with ElementwiseUFunc

/**
 * Type marker for BinaryOp A :&lt B.
 *
 * @author dramage
 */
object OpLT extends OpType with ElementwiseUFunc {
  implicit def impl2FromOrdering[T: Ordering]: Impl2[T, T, Boolean] = {
    val ord = implicitly[Ordering[T]]
    new Impl2[T, T, Boolean] {
      def apply(v: T, v2: T): Boolean = ord.lt(v, v2)
    }
  }
  @expand
  implicit def impl2[@expand.args(Int, Float, Long, Double) T, @expand.args(Int, Float, Long, Double) U]
    : Impl2[T, U, Boolean] = {
    new Impl2[T, U, Boolean] {
      def apply(v: T, v2: U): Boolean = v < v2
    }
  }
}

/**
 * Type marker for BinaryOp A :&lt= B.
 *
 * @author dramage
 */
object OpLTE extends OpType with ElementwiseUFunc {
  implicit def impl2FromOrdering[T: Ordering]: Impl2[T, T, Boolean] = {
    val ord = implicitly[Ordering[T]]
    new Impl2[T, T, Boolean] {
      def apply(v: T, v2: T): Boolean = ord.lteq(v, v2)
    }
  }

  @expand
  implicit def impl2[@expand.args(Int, Float, Long, Double) T, @expand.args(Int, Float, Long, Double) U]
    : Impl2[T, U, Boolean] = {
    new Impl2[T, U, Boolean] {
      def apply(v: T, v2: U): Boolean = v <= v2
    }
  }
}

/**
 * Type marker for BinaryOp A :&gt B.
 *
 * @author dramage
 */
object OpGT extends OpType with ElementwiseUFunc {
  implicit def impl2FromOrdering[T: Ordering]: Impl2[T, T, Boolean] = {
    val ord = implicitly[Ordering[T]]
    new Impl2[T, T, Boolean] {
      def apply(v: T, v2: T): Boolean = ord.gt(v, v2)
    }
  }
  @expand
  implicit def impl2[@expand.args(Int, Float, Long, Double) T, @expand.args(Int, Float, Long, Double) U]
    : Impl2[T, U, Boolean] = {
    new Impl2[T, U, Boolean] {
      def apply(v: T, v2: U): Boolean = v > v2
    }
  }
}

/**
 * Type marker for BinaryOp A :&gt= B.
 *
 * @author dramage
 */
object OpGTE extends OpType with ElementwiseUFunc {
  implicit def impl2FromOrdering[T: Ordering]: Impl2[T, T, Boolean] = {
    val ord = implicitly[Ordering[T]]
    new Impl2[T, T, Boolean] {
      def apply(v: T, v2: T): Boolean = ord.gteq(v, v2)
    }
  }

  @expand
  implicit def impl2[@expand.args(Int, Float, Long, Double) T, @expand.args(Int, Float, Long, Double) U]
    : Impl2[T, U, Boolean] = {
    new Impl2[T, U, Boolean] {
      def apply(v: T, v2: U): Boolean = v >= v2
    }
  }
}

/**
 * Type marker for BinaryOp A :== B.
 *
 * @author dramage
 */
object OpEq extends OpType with ElementwiseUFunc {
  implicit def impl2FromOrdering[T: Ordering]: Impl2[T, T, Boolean] = {
    val ord = implicitly[Ordering[T]]
    new Impl2[T, T, Boolean] {
      def apply(v: T, v2: T): Boolean = ord.equiv(v, v2)
    }
  }

  @expand
  implicit def impl2[@expand.args(Int, Float, Long, Double) T, @expand.args(Int, Float, Long, Double) U]
    : Impl2[T, U, Boolean] = {
    new Impl2[T, U, Boolean] {
      def apply(v: T, v2: U): Boolean = v == v2
    }
  }
}

/**
 * Type marker for BinaryOp A :!= B.
 *
 * @author dramage
 */
object OpNe extends OpType with ElementwiseUFunc {
  implicit def impl2FromOrdering[T: Ordering]: Impl2[T, T, Boolean] = {
    val ord = implicitly[Ordering[T]]
    new Impl2[T, T, Boolean] {
      def apply(v: T, v2: T): Boolean = !ord.equiv(v, v2)
    }
  }
}

/**
 * Type marker for BinaryUpdateOp A := B.
 *
 * @author dramage
 */
object OpSet extends OpType with ElementwiseUFunc

/**
 * Type marker for BinaryOp A :& B
 *
 * @author dramage
 */
object OpAnd extends OpType with ElementwiseUFunc {
  implicit object opAndBoolean extends Impl2[Boolean, Boolean, Boolean] {
    override def apply(v: Boolean, v2: Boolean): Boolean = v && v2
  }

}

/**
 * Type marker for BinaryOp A :| B
 *
 * @author dramage
 */
object OpOr extends OpType with ElementwiseUFunc {
  implicit object impl_OpOr_B_B_eq_B extends Impl2[Boolean, Boolean, Boolean] {
    override def apply(v: Boolean, v2: Boolean): Boolean = v || v2
  }

}

/**
 * Type marker for BinaryOp A :^^ B
 *
 * @author dramage
 */
object OpXor extends OpType with ElementwiseUFunc {
  implicit object opXorBoolean extends Impl2[Boolean, Boolean, Boolean] {
    override def apply(v: Boolean, v2: Boolean): Boolean = v ^ v2
  }

}

/**
 * Type marker for UnaryOp -A.
 *
 * @author dramage
 */
object OpNeg extends OpType with ElementwiseUFunc {
  implicit def ringNegation[S: Ring]: UImpl[OpNeg.this.type, S, S] = new Impl[S, S] {
    def apply(v: S): S = implicitly[Ring[S]].negate(v)
  }
}

/**
 * Type marker for UnaryOp !A.
 *
 * @author dramage
 */
object OpNot extends OpType with MappingUFunc {
  implicit object opNotBoolean extends Impl[Boolean, Boolean] {
    override def apply(v: Boolean): Boolean = !v
  }

}

/**
 * Type marker for inner (dot) product of A and B.
 *
 * @author dramage
 */
object OpMulInner extends OpType with UFunc {
  def opMulInnerFromSemiring[S: Semiring]: OpMulInner.Impl2[S, S, S] = new Impl2[S, S, S] {
    def apply(v: S, v2: S): S = implicitly[Semiring[S]].*(v, v2)
  }
}

/**
 * Type marker for BinaryOp A \ B when A is a matrix.
 *
 * @author dramage
 */
object OpSolveMatrixBy extends OpType with UFunc

/**
 * Type marker for inner (dot) product of A and B.
 *
 * @author dramage
 */
object OpMulMatrix extends OpType with UFunc {
  implicit def opMulMatrixFromSemiring[S: Semiring]: Impl2[S, S, S] = new Impl2[S, S, S] {
    def apply(v: S, v2: S): S = implicitly[Semiring[S]].*(v, v2)
  }
}
