package scalanlp.util

import scalala.tensor.dense.DenseVector

/**
 * A Lens is defined by an isomorphism between two types. useful
 * when one set of implicits is easily defined for some type, but not for some other type
 * @author dlwh
 */
trait Lens[T,U] { outer =>
  def forward(t: T):U
  def backward(u: U):T

  def reflect:Lens[U,T] = new Lens[U,T] {
    def forward(u: U) = outer.backward(u)
    def backward(t: T) = outer.forward(t)
  }
}

object Lens {
  def apply[T,U](tu: T=>U,ut: U=>T):Lens[T,U] = new Lens[T,U] {
    def forward(t: T) = tu(t);
    def backward(t: U) = ut(t);
  }

  // Scalala lenses:
  implicit object doubleIsVector extends Lens[Double,DenseVector[Double]] {
    def forward(t: Double) = DenseVector(t)
    def backward(t: DenseVector[Double]) = { assert(t.size == 1); t(0)}
  }

   implicit object pdoubleIsVector extends Lens[(Double,Double),DenseVector[Double]] {
    def forward(t: (Double,Double)) = DenseVector(t._1,t._2)
    def backward(t: DenseVector[Double]) = { assert(t.size == 2); (t(0),t(1))}
  }
}