package breeze.linalg.traits

/**
 * Created by ktakagaki on 15/04/14.
 */
abstract class Indices
case class Indices1(index1: Int) extends Indices
case class Indices2(override val index1: Int, index2: Int) extends Indices1(index1)
case class Indices3(override val index1: Int, override val index2: Int, index3: Int) extends Indices2(index1, index2)