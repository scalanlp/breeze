package breeze

/**
 *
 * @author dlwh
 */
package object optimize {
  def minimize[Objective, Vector](fn: Objective,
                                 init: Vector,
                                 options: OptimizationOption*)(implicit optimization: OptimizationPackage[Objective,Vector]) {
    optimization.minimize(fn, init, options:_*)
  }
}
