package breeze.optimize

import breeze.math.NormedVectorSpace

trait Projecting[T] {
  def projection: T => T
  def projectedVector(x: T, g: T)(implicit vspace: NormedVectorSpace[T, Double]): T = vspace.subVV(projection(vspace.addVV(x, g)), x)
}
