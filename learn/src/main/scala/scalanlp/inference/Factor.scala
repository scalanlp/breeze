package scalanlp.inference

trait Factor[F] { this: F =>

  /** Pointwise multiplication */
  def *(f: F):F
  /** Pointwise division */
  def /(f: F):F
  /** Scalar multiplication in log space (i.e. actually addition)*/
  def *(f: Double):F

  /** May be infinite */
  def logPartition: Double

  def isConvergedTo(f: F, diff: Double=1E-4):Boolean
}

