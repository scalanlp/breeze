package breeze.linalg


/**
 * Represents the Transpose of an instance of a type. The most common use is for Transpose[DenseVector[T]] to
 * represent row vectors
 *
 * @author dlwh
 **/
final case class Transpose[+T](inner: T) extends NumericOps[Transpose[T]] {

  def repr = this

}

