package breeze.linalg

import org.scalatest.FunSuite

import scala.collection.mutable.ArrayBuffer

class DenseTensorNTest extends FunSuite {

  test("testNdIndex") {
    val linearIndex = 10
    val shape = IndexedSeq(3, 3)
    val stride = 1
    val offset = 4
    val denseTensorN = new DenseTensorN[Long](new Array[Long](shape.product),shape, offset, stride)

    /* Wrote an imperative-style method to check correctness of the original ndIndex method. Need to write some proper test cases. */
    def imperativeNDIndex(linearIndex: Int, size: Int): IndexedSeq[Int] = {
      var denom = size
      var index = (linearIndex - offset) / (stride + 1)
      val ret = new ArrayBuffer[Int](shape.length)
      var i = 0
      for(i <- shape.indices){
        denom /= shape(i)
        ret.prepend( index / denom )
        index %= denom

      }

      ret.reverse
    }

    print(imperativeNDIndex(linearIndex, shape.product))
    assert(denseTensorN.ndIndex(linearIndex) == imperativeNDIndex(linearIndex, shape.product))
  }

}
