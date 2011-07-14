package scalanlp.inference

trait FactorLike[+F] { this: F =>

  /** Pointwise multiplication */
  def *[F2,FR](f: F2)(implicit fp: FactorProduct[F,F2,FR]) = fp.product(this.asInstanceOf[F],f)
  /** Pointwise division */
  def /[F2,FR](f: F2)(implicit fp: FactorQuotient[F,F2,FR]) = fp.quotient(this.asInstanceOf[F],f)
  /** Scalar multiplication in log space (i.e. actually addition)*/
  def *(f: Double):F

  /** May be infinite */
  def logPartition: Double
}

/**
 * 
 * @author dlwh
 */

trait Factor[-A] extends FactorLike[Factor[A]] with (A=>Double) {
  /** Returns the unnormalized score in log space */
  def apply(a: A):Double
}

trait FactorProduct[-F1,-F2,+FR] {
  def product(f1: F1, f2: F2):FR
}

trait FactorQuotient[-F1,-F2,+FR] {
  def quotient(f1: F1, f2: F2):FR
}

