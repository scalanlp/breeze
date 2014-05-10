package breeze.util

import org.scalatest.fixture
import breeze.util.Select._
import breeze.linalg.DenseVector

/**
 * @author ktakagaki
 * @date 05/10/2014.
 */
class SelectTest extends fixture.FunSuite {

  test("quickSelect"){
    val testArray = Array(5, 3, 2, 1, 4, 0)
    assert( DenseVector( Range(0, 6).map( quickSelect(testArray, _ ) ) == DenseVector(0, 1, 2, 3, 4, 5) )
  }

}
