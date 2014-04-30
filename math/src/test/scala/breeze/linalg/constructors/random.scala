package breeze.linalg.constructors

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import breeze.linalg.{DenseVector, min, max, randomDouble}

/**
 * @author ktakagaki
 * @date 04/30/2014.
 */
@RunWith(classOf[JUnitRunner])
class random extends FunSuite {

  test("rand/randn"){
    val rand1000 = randomDouble( 1000 )
    val maxR1000 = max(rand1000)
    val minR1000 = min(rand1000)
//    assert( maxR1000 > 0.95 && maxR1000 <= 1 && minR1000 < 0.05 && minR1000 >= 0)
  }


}
