package breeze.linalg.functions

/**
 * A Chebyshev distance metric implementation between two points
 */
object chebyshevDistance extends NormBasedDistance {
  override protected def normConstant: Double = Double.PositiveInfinity
}


