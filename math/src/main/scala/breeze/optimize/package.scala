package breeze

/**
 *
 * @author dlwh
 */
package object optimize {

  /**
   * Minimizes a function, given an [[breeze.optimize.OptimizationPackage]] that knows how to minimize
   * @param fn
   * @param init
   * @param options
   * @param optimization
   * @tparam Objective
   * @tparam Vector
   * @return
   */
  def minimize[Objective, Vector](fn: Objective, init: Vector, options: OptimizationOption*)(
      implicit optimization: OptimizationPackage[Objective, Vector]) = {
    optimization.minimize(fn, init, options: _*)
  }

  /**
   * Returns a sequence of states representing the iterates of a solver, given an [[breeze.optimize.IterableOptimizationPackage]] that knows how to minimize
   * The actual state class varies with the kind of function passed in. Typically, they have a .x value of type Vector that is the current point being
   * evaluated, and .value is the current objective value
   * @param fn
   * @param init
   * @param options
   * @param optimization
   * @tparam Objective
   * @tparam Vector
   * @return
   */
  def iterations[Objective, Vector, State](fn: Objective, init: Vector, options: OptimizationOption*)(
      implicit optimization: IterableOptimizationPackage[Objective, Vector, State]) = {
    optimization.iterations(fn, init, options: _*)
  }
}
