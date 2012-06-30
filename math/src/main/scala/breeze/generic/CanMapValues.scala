package breeze.generic

import breeze.math.Complex

/**
 * Marker for being able to map the keys and values in a value collection
 * to new values.
 *
 * @author dramage
 * @author dlwh
 */
trait CanMapValues[-From, +A, -B, +To] {
  /**Maps all key-value pairs from the given collection. */
  def map(from: From, fn: (A => B)): To

  /**Maps all active key-value pairs from the given collection. */
  def mapActive(from: From, fn: (A => B)): To
}

object CanMapValues {
  implicit def canMapSelf[V, V2]: CanMapValues[V, V, V2, V2] = {
    new CanMapValues[V, V, V2, V2] {
      def map(from: V, fn: (V) => V2) = fn(from)
      def mapActive(from: V, fn: (V) => V2) = fn(from)
    }
  }

  type Op[From, A, B, To] = CanMapValues[From, A, B, To]

  //
  // Arrays
  //

  class OpArray[@specialized(Int, Float, Double) A, @specialized(Int, Float, Double) B: ClassManifest]
    extends Op[Array[A], A, B, Array[B]] {

    /**Maps all values from the given collection. */
    def map(from: Array[A], fn: (A) => B): Array[B] = {
      val arr = new Array[B](from.length)
      for(i <- 0 until from.length) {
        arr(i) = fn(from(i))
      }
      arr
    }

    /**Maps all active key-value pairs from the given collection. */
    def mapActive(from: Array[A], fn: (A) => B): Array[B] = map(from, fn)
  }


  implicit def opArray[@specialized A, @specialized B: ClassManifest] =
    new OpArray[A, B]

  implicit object OpArrayII extends OpArray[Int, Int]

  implicit object OpArraySS extends OpArray[Short, Short]

  implicit object OpArrayLL extends OpArray[Long, Long]

  implicit object OpArrayFF extends OpArray[Float, Float]

  implicit object OpArrayDD extends OpArray[Double, Double]

  implicit object OpArrayCC extends OpArray[Complex, Complex]

  implicit object OpArrayID extends OpArray[Int, Double]

  implicit object OpArraySD extends OpArray[Short, Double]

  implicit object OpArrayLD extends OpArray[Long, Double]

  implicit object OpArrayFD extends OpArray[Float, Double]
}