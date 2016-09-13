package breeze.symbolic

import breeze.generic.{MappingUFunc, UFunc}

/**
  * Type class to calculate the symbolic derivative of a SymbolicFunction
  * @tparam F The type of the SymbolicFunction to derive
  */
trait CanDerive[F <: SymbolicFunction[F]] {
  type D <: SymbolicFunction[D]
  def derivation(f: F): D
}

object CanDerive {
  type Aux[F <: SymbolicFunction[F], D0 <: SymbolicFunction[D0]] = CanDerive[F] { type D = D0 }

  implicit class RichDerivable[F <: SymbolicFunction[F]](f: F) {
    def derivation(implicit canDerive: CanDerive[F]): canDerive.D = canDerive.derivation(f)
  }
  import breeze.macros.symbolic.FunctionTransformationMacros
  implicit def canDeriveWithMacro[F <: SymbolicFunction[F]]: CanDerive[F] =
    macro FunctionTransformationMacros.canDerive_impl[F]
}

object derive extends UFunc with MappingUFunc {
  implicit def implCanDerive[F <: SymbolicFunction[F]](implicit canDerive: CanDerive[F]): Impl[F, canDerive.D] = new Impl[F, canDerive.D] {
    def apply(f: F) = canDerive.derivation(f)
  }
}