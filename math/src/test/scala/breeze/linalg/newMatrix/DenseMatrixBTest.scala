package breeze.linalg.newMatrix

import breeze.linalg.immutable.DenseMatrixB
import org.scalatest.FunSuite

/**
 * Created by ktakagaki on 15/04/15.
 */
class DenseMatrixBTest extends FunSuite {
  val dm1 = new DenseMatrixB()

  test( "mapping" ){
    val meanAll3: Double = mean( dm3 )
    val meanAll2: Double = mean( dm2 )
    val meanAll1: Double = mean( dm1 )

    val meanSpec: DMB1 = dm3.map( mean, (1, 2) )
    val meanSpec: Double = dm3.map( mean, (1, 2, 3) )
  }

}
