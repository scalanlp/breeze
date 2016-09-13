package breeze.symbolic

import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg.operators.{OpAdd, OpMulScalar}
import breeze.numerics
import shapeless.LUBConstraint._
import shapeless.{::, <:!<, HList, HNil}

/**
  * Type-class to simplify a SymbolicFunction
  * @tparam F The type of the SymbolicFunction to simplify
  */
trait CanSimplify[F <: SymbolicFunction[F]] {
  type R <: SymbolicFunction[R]
  def simplify(f: F): R
}

object CanSimplify {
  type Aux[F <: SymbolicFunction[F], Out] = CanSimplify[F] { type R = Out }

  implicit class RichSimplifiable[F <: SymbolicFunction[F]](f: F) {
    def simplify(implicit canSimplify: CanSimplify[F]): canSimplify.R =
      canSimplify.simplify(f)
  }
  import breeze.macros.symbolic.FunctionTransformationMacros
  implicit def canSimplifyWithMacro[F <: SymbolicFunction[F]]: CanSimplify[F] =
    macro FunctionTransformationMacros.canSimplify_impl[F]
}

object simplify extends UFunc with MappingUFunc {
  implicit def implCanSimplify[F <: SymbolicFunction[F]](implicit canSimplify: CanSimplify[F]): Impl[F, canSimplify.R] = new Impl[F, canSimplify.R] {
    def apply(f: F) = canSimplify.simplify(f)
  }
}