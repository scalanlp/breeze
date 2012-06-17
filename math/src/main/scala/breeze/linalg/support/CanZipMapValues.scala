package breeze.linalg.support

import breeze.math.Complex

/**
 * Marker for being able to zip two From's and map the values to a new collection
 *
 * @author dlwh
 */
trait CanZipMapValues[From, @specialized A, @specialized B, +To] {
  /** Maps all corresponding values from the two collection. */
  def map(from : From, from2: From, fn : (A,A)=>B) : To
}

object CanZipMapValues {
  type Op[From, A, B, To] = CanZipMapValues[From, A, B, To]

  //
  // Arrays
  //

  class OpArray[@specialized A, @specialized B: ClassManifest]
    extends Op[Array[A], A, B, Array[B]] {

    /**Maps all values from the given collection. */
    def map(from: Array[A], from2: Array[A], fn: (A, A) => B) = {
      require(from.length == from2.length, "Array lengths don't match!")
      val arr = new Array[B](from.length)
      for(i <- 0 until from.length) {
        arr(i) = fn(from(i), from2(i))
      }
      arr
    }

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
