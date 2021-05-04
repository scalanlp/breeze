package breeze.collection

import scala.reflect.ClassTag

import scala.collection.compat.immutable.ArraySeq

package object compat {
  def arraySeqBuilder[K: ClassTag] = ArraySeq.newBuilder[K]
}