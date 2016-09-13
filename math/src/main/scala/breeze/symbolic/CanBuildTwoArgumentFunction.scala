package breeze.symbolic

trait CanBuildTwoArgumentFunction[A, B, F[A <: SymbolicFunction[A], B <: SymbolicFunction[B]] <: SymbolicFunction[F[A, B]]] {
  def build[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2]](f1: F1, f2: F2): F[F1, F2]
}

object CanBuildTwoArgumentFunction {

  implicit def canBuildDivision[A, B]: CanBuildTwoArgumentFunction[A, B, Division] = new CanBuildTwoArgumentFunction[A, B, Division] {
    override def build[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2]](f1: F1, f2: F2): Division[F1, F2] =
      Division(f1, f2)
  }
  implicit def canBuildDifference[A, B]: CanBuildTwoArgumentFunction[A, B, Difference] =
    new CanBuildTwoArgumentFunction[A, B, Difference] {
      override def build[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2]](f1: F1, f2: F2): Difference[F1, F2] =
        Difference(f1, f2)
    }
}
