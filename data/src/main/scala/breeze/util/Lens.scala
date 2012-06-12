package breeze.util

/**
 * A Lens defines a functional way of handling getters/setters. They're useful for
 * extending transformations on a part of a case class to the whole case class by updating that
 * one component
 * @author dlwh
 */
trait Lens[T,U] {
  def apply(t: T): U = get(t)
  def get(t: T):U
  def set(t: T, u: U):T
}

object Lens {
  implicit def identity[T]:Lens[T,T] = new Lens[T,T] {
    def get(t: T) = t
    def set(t: T, u: T) = u
  }

  implicit def isomorphismYieldsLens[T,U](implicit iso: Isomorphism[T,U]) = new Lens[T,U] {
    def get(t: T) = iso.forward(t)

    def set(t: T, u: U) = iso.backward(u)
  }
}
