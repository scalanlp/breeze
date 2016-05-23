package breeze.linalg.functions

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

/**
  * @author Michael Petnuch
  * @version $Id$
  */
@RunWith(classOf[JUnitRunner])
class windowTest extends FunSuite with Checkers {

  test("UFunc") {
    import breeze.linalg._
    import breeze.stats._

    val vector = DenseVector.tabulate[Double](100)(i => i + 1)

    // works if we grab everything explicitly
    val op = mean.reduce_Double[DenseVector[Double]]
    val handHold = DenseVector.handholdCanWindowDenseVector[Double]
    val canCollapseWindow = DenseVector.canCollapseWindow[Double, Double]

    val windowedMeanOp = WindowedVector.windowedOp(handHold, canCollapseWindow, op)
    val windowedMeans = windowedMeanOp.apply(window(vector, 20))
    println(windowedMeans)

    // this fails with a compiler error
    // that it cannot find implicit for impl: mean.Impl[WindowedDenseVector[Double], DenseVector[Double]]
//     mean(window(vector, 20))
  }
}
