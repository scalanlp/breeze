package breeze.symbolic

trait CanBuildSingleArgumentFunction[A, F[A <: SymbolicFunction[A]] <: SymbolicFunction[F[A]]] {
  def build[I <: SymbolicFunction[I]](i: I): F[I]
}

object CanBuildSingleArgumentFunction {
  implicit def canBuildExponential[A]: CanBuildSingleArgumentFunction[A, Exponential] = new CanBuildSingleArgumentFunction[A, Exponential] {
      override def build[I <: SymbolicFunction[I]](i: I): Exponential[I] = new Exponential(i)
    }

  implicit def canBuildLogarithm[A]: CanBuildSingleArgumentFunction[A, Logarithm] = new CanBuildSingleArgumentFunction[A, Logarithm] {
      override def build[I <: SymbolicFunction[I]](i: I): Logarithm[I] = new Logarithm(i)
    }

}