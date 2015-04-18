package breeze.linalg.immutable

import breeze.linalg.product

import scala.{specialized => spec}

/**
 * Created by ktakagaki on 15/04/09.
 */
abstract class DenseMatrixB[@spec(Double, Int, Float, Long) V](
                                                              protected val internalData: Seq[V],
                                                              protected val dimensions: Seq[Int],
                                                              protected val offsets: Seq[Int],
                                                              protected val strides: Seq[Int]
                                                                )
  extends Matrix[V] {

  assert(dimensions != null, "argument dims cannot be null")
  assert(internalData != null, "argument data cannot be null")
  assert(offsets != null, "offset data cannot be null")
  assert(strides != null, "strides data cannot be null")
  //assert(transpose != null, "transpose data cannot be null")


  override val getOrder = dimensions.length
  assert(getOrder >= 1, "Argument dims must be larger than zero!") // cannot be negative")
  assert(!(getOrder == 0 && internalData.length != 1), "For a zero-order matrix (wrapped scalar), the data length must be 1!")
  assert(getOrder <= 3, "Currently supporting only up to 3D matrices")

  assert(getOrder == offsets.length, s"offsets length ($offsets.length) must be equal to dimensions length ($getOrder)")
  assert(getOrder == strides.length, s"strides length ($strides.length) must be equal to dimensions length ($getOrder)")
  //assert(getOrder == transpose.length, s"transpose length ($transpose.length) must be equal to dimensions length ($getOrder)")

  /** Dimensions returns a protective copy of the input dimensions. */
  final def getDimensions() = dimensions.toArray.clone()


}


class DenseMatrixB3[@spec(Double, Int, Float, Long) V](
                                                       override protected val internalData: Seq[V],
                                                       override protected val dimensions: Seq[Int],
                                                       override protected val offsets: Seq[Int],
                                                       override protected val strides: Seq[Int]
                                                       )
  extends DenseMatrixB[V](internalData, dimensions, offsets, strides) {

    override lazy val indicesToLinearIndex: (Seq[Int] => Int) = {
      val tempret = ((ind: Seq[Int]) =>
        (offsets(0) + strides(0)*ind(0)) + (offsets(1) + strides(1)*ind(1)) + (offsets(2) + strides(2)*ind(2))
      )
      assert(internalData.length > tempret(dimensions),
        s"Not enough input internalData specified (length = $internalData.length) for given input dimensions"  )
      tempret
    }

    override lazy val linearIndexToIndices: (Int => Seq[Int]) = {
      ((linearInd: Int) =>
      Array(  (linearInd - offsets(0))%strides(0), (linearInd - offsets(1))%strides(1), (linearInd - offsets(2))%strides(2) )
    }


}