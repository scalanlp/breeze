package breeze.util

/**
 * An Isomorphism is defined by a reversible transformation between two types. useful
 * when one set of implicits is easily defined for some type, but not for some other type
 * @author dlwh
 */
@SerialVersionUID(1)
trait Isomorphism[T,U] extends Serializable { outer =>
  def forward(t: T):U
  def backward(u: U):T

  def reverse:Isomorphism[U,T] = new Isomorphism[U,T] {
    def forward(u: U) = outer.backward(u)
    def backward(t: T) = outer.forward(t)
  }
}

object Isomorphism {
  def apply[T,U](tu: T=>U,ut: U=>T):Isomorphism[T,U] = new Isomorphism[T,U] {
    def forward(t: T) = tu(t);
    def backward(t: U) = ut(t);
  }

  implicit def identity[T]: Isomorphism[T,T] = new Isomorphism[T,T] {
    def forward(t: T) = t

    def backward(u: T) = u
  }

  /*
  // Scalala lenses:

  */
}