package breeze

/**
 *
 * @author dlwh
 */
package object optimize {
  def minimize[Function, Vector, Option, T](fn: Function,
                                         init: Vector,
                                         options: T*)(implicit optimization: OptimizationPackage[Function,Vector,Option], conv: T=>Option) {
    optimization.minimize(fn, init, options.map(conv): _*)
  }
}
