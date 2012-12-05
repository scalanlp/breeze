package breeze.data

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import breeze.linalg.SparseVector

/**
 * 
 * @author dlwh
 */

@RunWith(classOf[JUnitRunner])
class SparseFeatureDatasetTest extends FunSuite {
  test("Basic MLComp example") {
    val result = SparseFeatureDataset.fromSource[Int](scala.io.Source.fromString("""-1 1:1 3:1
1 2:1 3:1
-1 1:1 3:1
-1 1:1"""))
    assert(result.examples(0).label === -1)
    assert(result.examples(0).features === SparseVector(0.0,1.0,0.0,1.0), result.examples(0).features.length + " " + result.examples(0).features.valuesIterator.mkString(",") + " "+ SparseVector(0.0, 1.0, 0.0, 1.0).valuesIterator.mkString(", "))
    assert(result.examples(3).features === SparseVector(0.0,1.0,0.0,0.0))
  }

}