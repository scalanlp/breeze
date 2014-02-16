package breeze.linalg

import breeze.linalg.support.{LowPriorityArgTopK, LowPriorityArgSort}
import breeze.generic.UFunc

/**
 * Returns a sequence of keys sorted by value
 *
 * @author dlwh
 **/
object argsort extends UFunc with LowPriorityArgSort {


}


/**
 * Returns a sequence of keys sorted by value
 *
 * @author dlwh
 **/
object argtopk extends UFunc with LowPriorityArgTopK {


}