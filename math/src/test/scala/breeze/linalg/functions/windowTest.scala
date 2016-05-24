package breeze.linalg.functions

import breeze.generic.UFunc
import breeze.generic.UFunc.UImpl
import breeze.linalg.Options.OptPadMode
import breeze.linalg.WindowedVector
import breeze.linalg.support.CanCollapseWindow
import com.sun.scenario.Settings
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

    val wmeans = mean(window(vector, 20))
    println(wmeans)
  }
}

