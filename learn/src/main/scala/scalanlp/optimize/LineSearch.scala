package scalanlp.optimize

/**
 * A line search optimizes a function of one variable without
 * analytic gradient information.
 * @author dlwh
 */
trait LineSearch extends ApproximateLineSearch {
  def minimize(f: (Double)=>Double):Double = iterations(f).reduceLeft( (a,b) => b).alpha
}

/**
 * A line search optimizes a function of one variable without
 * analytic gradient information. It's often used approximately (e.g. in
 * backtracking line search), where there is no intrinsic termination criterion, only extrinsic
 * @author dlwh
 */
trait ApproximateLineSearch {
  final case class State(alpha: Double, value: Double);
  def iterations(f: Double=>Double):Iterator[State]
}