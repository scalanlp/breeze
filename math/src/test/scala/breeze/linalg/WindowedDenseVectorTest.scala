package breeze.linalg

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

/**
  * @author Michael Petnuch
  * @version $Id$
  */
@RunWith(classOf[JUnitRunner])
class WindowedDenseVectorTest extends FunSuite with Checkers {

  test("UFunc") {
    import breeze.linalg._
    import breeze.math._
    import breeze.numerics._
    import breeze.stats._

    val vector = DenseVector.tabulate[Double](100)(i => i + 1)
    val windowedVector = vector.windowed(20)
    val rollingMeans = mean(windowedVector)
  }
}
