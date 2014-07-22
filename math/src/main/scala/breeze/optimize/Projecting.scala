package breeze.optimize

import breeze.math.{Module, NormedVectorSpace}

trait Projecting[T] {
  def projection: T => T
  def projectedVector(x: T, g: T)(implicit vspace: Module[T, Double]): T = vspace.subVV(projection(vspace.addVV(x, g)), x)
}
