package breeze.linalg

import breeze.generic.UFunc

object zipValues extends UFunc {

}

/**
 * Usually used as the return type from zipValues
 * @tparam V1
 * @tparam V2
 */
trait ZippedValues[@specialized(Double) V1, @specialized(Double) V2] {
  def foreach[A](f: (V1,V2)=>A)
  // TODO: define map for this.
//  def map[A](a: Coll1, b: Coll2, f: (V1,V2)=>A)(implicit canZipMapValues)
}
