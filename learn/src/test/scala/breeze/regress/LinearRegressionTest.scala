package breeze.regress

/**
 * 
 * @author dlwh
 */

import org.scalatest._
import org.scalatest.junit._
import org.junit.runner.RunWith
import breeze.linalg._

@RunWith(classOf[JUnitRunner])
class LinearRegressionTest extends FunSuite {

  test("Simple univariate example") {
    //  http://easycalculation.com/statistics/learn-regression.php
    val data = DenseMatrix((60.0,1.0),(61.0, 1.0), (62.0, 1.0),(63.0, 1.0),(65.0, 1.0))
    val y = DenseVector(3.1,3.6,3.8,4.0,4.1)
    val beta = LinearRegression.regress(data,y)
    assert((beta(1) - -7.9635).abs < 1E-3, beta)
    assert((beta(0) - .1878).abs < 1E-3, beta)


  }

}