package breeze.inference

trait Factor[F] { this: F =>

  /** Pointwise multiplication */
  def *(f: F):F
  /** Pointwise division */
  def /(f: F):F

  /** May be infinite */
  def logPartition: Double

  def isConvergedTo(f: F, diff: Double=1E-4):Boolean
}


trait ExpFactor[F] extends Factor[F] { this: F =>
  /** Exponentiation */
  def **(f: Double):F
}