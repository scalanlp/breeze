package breeze.collection

import scala.collection.compat.immutable.ArraySeq
import scala.reflect.ClassTag

package object compat {
  def arraySeqBuilder[K: ClassTag] = ArraySeq.canBuildFrom[K].newBuilder
}