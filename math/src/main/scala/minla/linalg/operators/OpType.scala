package minla.linalg.operators

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
object OpAdd extends OpAdd

/**
 * Type marker for BinaryOp A :- B and BinaryUpdateOp A :-= B.
 *
 * @author dramage
 */
sealed trait OpSub extends OpType
object OpSub extends OpSub

/**
 * Type marker for BinaryOp A :* B and BinaryUpdateOp A :*= B.
 *
 * @author dramage
 */
sealed trait OpMulScalar extends OpMulMatrix
object OpMulScalar extends OpMulScalar

/**
 * Type marker for BinaryOp A :/ B and BinaryUpdateOp A:/= B.
 *
 * @author dramage
 */
sealed trait OpDiv extends OpType
object OpDiv extends OpDiv

/**
 * Type marker for BinaryOp A :% B and BinaryUpdateOp A:%= B.
 *
 * @author dramage
 */
sealed trait OpMod extends OpType
object OpMod extends OpMod

/**
 * Type marker for BinaryOp A :^ B and BinaryUpdateOp A:^= B.
 *
 * @author dramage
 */
sealed trait OpPow extends OpType
object OpPow extends OpPow

/**
 * Type marker for BinaryOp A :&lt B.
 *
 * @author dramage
 */
sealed trait OpLT  extends OpType
object OpLT  extends OpLT

/**
 * Type marker for BinaryOp A :&lt= B.
 *
 * @author dramage
 */
sealed trait OpLTE extends OpType
object OpLTE extends OpLTE

/**
 * Type marker for BinaryOp A :&gt B.
 *
 * @author dramage
 */
sealed trait OpGT  extends OpType
object OpGT  extends OpGT

/**
 * Type marker for BinaryOp A :&gt= B.
 *
 * @author dramage
 */
sealed trait OpGTE extends OpType
object OpGTE extends OpGTE

/**
 * Type marker for BinaryOp A :== B.
 *
 * @author dramage
 */
sealed trait OpEq  extends OpType
object OpEq  extends OpEq

/**
 * Type marker for BinaryOp A :!= B.
 *
 * @author dramage
 */
sealed trait OpNe  extends OpType
object OpNe  extends OpNe

/**
 * Type marker for BinaryUpdateOp A := B.
 *
 * @author dramage
 */
sealed trait OpSet extends OpType
object OpSet extends OpSet

/**
 * Type marker for BinaryOp A :&& B
 *
 * @author dramage
 */
sealed trait OpAnd extends OpType
object OpAnd extends OpAnd

/**
 * Type marker for BinaryOp A :|| B
 *
 * @author dramage
 */
sealed trait OpOr extends OpType
object OpOr extends OpOr

/**
 * Type marker for BinaryOp A :^^ B
 *
 * @author dramage
 */
sealed trait OpXor extends OpType
object OpXor extends OpXor

/**
 * Type marker for UnaryOp -A.
 *
 * @author dramage
 */
sealed trait OpNeg extends OpType
object OpNeg extends OpNeg

/**
 * Type marker for UnaryOp !A.
 *
 * @author dramage
 */
sealed trait OpNot extends OpType
object OpNot extends OpNot


/**
 * Type marker for inner (dot) product of A and B.
 *
 * @author dramage
 */
sealed trait OpMulInner extends OpType
object OpMulInner extends OpMulInner


/**
 * Type marker for BinaryOp A \ B when A is a matrix.
 *
 * @author dramage
 */
sealed trait OpSolveMatrixBy extends OpType
object OpSolveMatrixBy extends OpSolveMatrixBy


/**
 * Type marker for inner (dot) product of A and B.
 *
 * @author dramage
 */
sealed trait OpMulMatrix extends OpType
object OpMulMatrix extends OpMulMatrix
