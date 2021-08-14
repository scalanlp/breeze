package breeze.linalg

import breeze.linalg.operators._
import breeze.macros._

trait SliceVectorOps extends VectorOps {
  import breeze.math.PowImplicits._

//  @expand
//  implicit def slv_v_Op[
//    K,
//    @expand.args(Int, Double, Float, Long) T,
//    @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
//    (implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) })
//  op: Op.Impl2[T, T, T]): BinaryRegistry[SliceVector[K, T], Vector[T], Op.type, DenseVector[T]] =
//    new BinaryRegistry[SliceVector[K, T], Vector[T], Op.type, DenseVector[T]] {
//
//      override protected def bindingMissing(a: SliceVector[K, T], b: Vector[T]): DenseVector[T] = {
//        require(a.length == b.length)
//        DenseVector.tabulate(a.length)(i => op(a(i), b(i)))
//      }
//      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
//    }

//  @expand
//  implicit def slv_s_Op[
//    K,
//    @expand.args(Int, Double, Float, Long) T,
//    @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
//  ( implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) })
//  op: Op.Impl2[T, T, T]): BinaryRegistry[SliceVector[K, T], T, Op.type, DenseVector[T]] =
//    new BinaryRegistry[SliceVector[K, T], T, Op.type, DenseVector[T]] {
//
//      override protected def bindingMissing(a: SliceVector[K, T], b: T): DenseVector[T] = {
//        DenseVector.tabulate(a.length)(i => op(a(i), b))
//      }
//      implicitly[BinaryRegistry[Vector[T], T, Op.type, Vector[T]]].register(this)
//    }

//  @expand
//  implicit def slv_v_InPlaceOp[
//    K,
//    @expand.args(Int, Double, Float, Long) T,
//    @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]( implicit @expand.sequence[Op]({ _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) })
//  op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[SliceVector[K, T], Vector[T], Op.type] =
//    new BinaryUpdateRegistry[SliceVector[K, T], Vector[T], Op.type] {
//
//      override protected def bindingMissing(a: SliceVector[K, T], b: Vector[T]): Unit = {
//        cforRange(0 until a.length) { i =>
//          a(i) = op(a(i), b(i))
//        }
//      }
//      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
//    }
//
//  @expand
//  implicit def slv_s_InPlaceOp[
//    K,
//    @expand.args(Int, Double, Float, Long) T,
//    @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
//  ( implicit @expand.sequence[Op]({ _ * _ }, { _ / _ }, {  (__x, __y) => __y }, { _ % _ }, { _.pow(_) }) op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[SliceVector[K, T], T, Op.type] =
//    new BinaryUpdateRegistry[SliceVector[K, T], T, Op.type] {
//
//      override protected def bindingMissing(a: SliceVector[K, T], b: T): Unit = {
//        cforRange(0 until a.length) { i =>
//          a(i) = op(a(i), b)
//        }
//      }
//      implicitly[BinaryUpdateRegistry[Vector[T], T, Op.type]].register(this)
//    }

}
