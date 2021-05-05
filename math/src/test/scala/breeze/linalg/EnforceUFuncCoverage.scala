package breeze.linalg

import breeze.generic.UFunc
import breeze.numerics._

object EnforceUFuncCoverage {

  sealed trait Witness[F, L, T]
  sealed trait One

  def vectors1[F <: UFunc](f: F)(implicit w: Witness[F, One, Vector.type]): Unit = ()
  implicit def wVectors1[F <: UFunc](
      implicit opV: UFunc.UImpl[F, Vector[Double], Any],
      opDV: UFunc.UImpl[F, DenseVector[Double], Any],
      opSV: UFunc.UImpl[F, SparseVector[Double], Any],
      opHV: UFunc.UImpl[F, HashVector[Double], Any],
      opSlV: UFunc.UImpl[F, SliceVector[Int, Double], Any]): Witness[F, One, Vector.type] = null

  implicit def matrices1[F <: UFunc](f: F)(implicit w: Witness[F, One, Matrix.type]): Unit = ()
  implicit def wMatrices1[F <: UFunc](
      implicit opV: UFunc.UImpl[F, Matrix[Double], Any],
      opDV: UFunc.UImpl[F, DenseMatrix[Double], Any],
      opSV: UFunc.UImpl[F, CSCMatrix[Double], Any],
      opSlV: UFunc.UImpl[F, SliceMatrix[Int, Int, Double], Any]): Witness[F, One, Matrix.type] = null

  implicit def counters1[F <: UFunc](f: F)(implicit w: Witness[F, One, Counter.type]): Unit = ()
  implicit def wCounters1[F <: UFunc](
      implicit opC: UFunc.UImpl[F, Counter[String, Double], Any],
      opC2: UFunc.UImpl[F, Counter2[String, Long, Double], Any]): Witness[F, One, Counter.type] = null

  implicit def vectorsAndMatrices1[F <: UFunc](f: F)(
      implicit wV: Witness[F, One, Vector.type],
      wM: Witness[F, One, Matrix.type]): Witness[F, One, Vector.type with Matrix.type] = null

  implicit def linalg1[F <: UFunc](f: F)(
      implicit wV: Witness[F, One, Vector.type],
      wM: Witness[F, One, Matrix.type],
      wC: Witness[F, One, Counter.type]): Witness[F, One, Tensor.type] = null

  /////////////////////////////////////
  // one arg UFuncs
  /////////////////////////////////////

  // reduce-y things
  EnforceUFuncCoverage.linalg1(max)
  EnforceUFuncCoverage.linalg1(min)
  EnforceUFuncCoverage.linalg1(ptp)
  EnforceUFuncCoverage.linalg1(argmax)
  EnforceUFuncCoverage.linalg1(argmin)
  EnforceUFuncCoverage.linalg1(all)
  EnforceUFuncCoverage.linalg1(any)
  EnforceUFuncCoverage.linalg1(sum)
  EnforceUFuncCoverage.linalg1(product)
  EnforceUFuncCoverage.linalg1(softmax)
  EnforceUFuncCoverage.linalg1(where)

  // linear algebra
  EnforceUFuncCoverage.vectors1(norm)
  EnforceUFuncCoverage.vectors1(normalize)(wVectors1[normalize.type](
    implicitly[normalize.Impl[Vector[Double], Any]],
    implicitly[normalize.Impl[DenseVector[Double], Any]],
    implicitly[normalize.Impl[SparseVector[Double], Any]],
    implicitly[normalize.Impl[HashVector[Double], Any]],
    implicitly[normalize.Impl[SliceVector[Int, Double], Any]]))

  EnforceUFuncCoverage.vectorsAndMatrices1(dim)

  // numerics
  EnforceUFuncCoverage.vectors1(sin)
  EnforceUFuncCoverage.vectors1(cos)
  EnforceUFuncCoverage.vectors1(tan)
  EnforceUFuncCoverage.vectors1(exp)
  EnforceUFuncCoverage.vectors1(lgamma)

  // Ensure things work
//  private object Q extends UFunc
//  shapeless.test.illTyped {
//    """
//      EnforceUFuncCoverage.vectors1(Q)
//    """
//  }
}
