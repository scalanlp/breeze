package breeze.generic

/**
 *
 * This trait is for multi-dimensional tensors that can logically have one of their
 * dimensions "collapsed", e.g. summing out all columns of a matrix to give a column
 * vector.
 *
 * @author dlwh
 * @tparam From the tensor being collapsed
 * @tparam Axis which axis is being collapsed. Usually a subtype of [[breeze.linalg.Axis.Value]]
 * @tparam ColType the type of the "column" (or row or...) being collapsed.
 * @tparam R What the column is being collapsed to.
 * @tparam TR result tensor type
 */
trait CanCollapseAxis[From, Axis, ColType, -R, TR] {
  def apply(from: From, axis: Axis)(f: ColType=>R):TR

}
