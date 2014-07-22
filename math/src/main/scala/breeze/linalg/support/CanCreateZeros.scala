package breeze.linalg.support

import breeze.math.Semiring

import scala.reflect.ClassTag

/**
 * breeze
 * 7/10/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
trait CanCreateZeros[+T, I] {
  def apply(d: I): T
}

object ArrayCanCreateZeros {
  class OpArray[@specialized V:ClassTag:Semiring]
    extends CanCreateZeros[Array[V],Int] {
    override def apply(d: Int) = {
      Array.fill(d)(implicitly[Semiring[V]].zero)
    }
  }
  implicit object OpArrayI extends OpArray[Int]
  implicit object OpArrayS extends OpArray[Short]
  implicit object OpArrayL extends OpArray[Long]
  implicit object OpArrayF extends OpArray[Float]
  implicit object OpArrayD extends OpArray[Double]
  implicit def OpArrayAny[V:ClassTag:Semiring] : OpArray[V] = new OpArray[V]
}
