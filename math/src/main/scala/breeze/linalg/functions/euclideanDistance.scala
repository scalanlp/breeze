package breeze.linalg

/**
 * A Euclidean distance metric implementation between two points
 */
object euclideanDistance extends NormBasedDistance {
  override protected def normConstant: Double = 2.0
}
