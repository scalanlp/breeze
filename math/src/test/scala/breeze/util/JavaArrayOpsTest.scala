package breeze.util

import org.scalatest.funsuite.AnyFunSuite
import breeze.util.JavaArrayOps._
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math._

/**
 * @author ktakagaki
 * @date 03/31/2014.
 */
class JavaArrayOpsTest extends AnyFunSuite {

  test("JavaOpts 1D conversions") {
    val dvI = DenseVector(0, 1, 2, 3, 4, 5)
    val dvC = DenseVector[Complex](-1d * i, 0d * i, 2d * breeze.math.i)
    assert(dvI == arrayIToDv(dvIToArray(dvI)))
    assert(arrayIToDv(dvIToArray(dvI)).length == 6)
    assert(dvC == arrayCToDv(dvCToArray(dvC)))
  }

  test("JavaOpts 2D conversions") {
    val dmD = DenseMatrix((0d, 1d, 2d, 3d, 4d, 5d), (10d, 8d, 6d, 4d, 2d, -2d))
    val dmC = DenseMatrix(
      (-1d * breeze.math.i, 0d * breeze.math.i, 2d * breeze.math.i),
      (10d * breeze.math.i, 9d * breeze.math.i, 7d * breeze.math.i))
    val dmCAC = array2CToDm(dmCToArray2(dmC))
    assert(dmD == array2DToDm(dmDToArray2(dmD)))
    assert(dmC == dmCAC)
    assert(dmCAC.rows == 2)
    assert(dmCAC.cols == 3)
  }

}
