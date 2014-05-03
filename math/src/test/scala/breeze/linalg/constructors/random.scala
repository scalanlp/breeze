package breeze.linalg.constructors

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import breeze.linalg._
import breeze.stats.mean
import breeze.numerics.abs

/**
 * @author ktakagaki
 * @date 04/30/2014.
 */
@RunWith(classOf[JUnitRunner])
class randomTest extends FunSuite {

  test("rand/randn") {
    val rand1000 = rand( 1000 )
    val maxR1000: Double = max(rand1000)
    val minR1000: Double = min(rand1000)

    assert( maxR1000 > 0.95 && maxR1000 <= 1 && minR1000 < 0.05 && minR1000 >= 0)

    val randn1000 = randn( 1000 )
    assert( abs( mean(randn1000) ) < 0.05)
  }

  test("randomDouble/randomInt"){
    val randDoub1000 = randomDouble( 1000, (10d, 15d) )
    val maxRD1000: Double = max(randDoub1000)
    val minRD1000: Double = min(randDoub1000)
    assert( maxRD1000 > 14.5 && maxRD1000 <= 15 && minRD1000 < 10.5 && minRD1000 >= 10)

    val randI1000 = randomInt( (17, 202), (5, 9) )
    val maxRI1000: Int = max(randI1000)
    val minRI1000: Int = min(randI1000)
    assert(  maxRI1000 > 8 && maxRI1000 <= 9 && minRI1000 < 6 && minRI1000 >= 5 )
    assert( randI1000.rows == 17 && randI1000.cols == 202)
  }


}
