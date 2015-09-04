package breeze.linalg.immutable

import breeze.linalg.product
import breeze.linalg.traits.{Indices3, Indices}

import scala.{specialized => spec}

/**Attempt at creating DenseMatrix equivalent which is immutable and allows higher order matrices.
  * Mutable versions should be simple to implement in the future,
  * by inheriting a mutability trait which allows update.
  * Seq[V] is used here to ensure immutability.
  *
 * Created by ktakagaki on 15/04/09.
 */
abstract class DenseMatrixB[@spec(Double, Int, Float, Long) V](
                                                              protected val internalData: Seq[V],
                                                              protected val dimensions: Indices,
                                                              protected val offsets:    Indices,
                                                              protected val strides:    Indices
                                                                )
  extends MatrixB[V] {

  assert(internalData != null, "argument data cannot be null")
  assert(dimensions != null, "argument dims cannot be null")
//  assert(offsets != null, "offset data cannot be null")
//  assert(strides != null, "strides data cannot be null")

  override val getOrder = dimensions.length
  assert(getOrder <= 3, "Currently supporting only up to 3D matrices")

//  assert(getOrder == offsets.length, s"offsets length ($offsets.length) must be equal to dimensions length ($getOrder)")
//  assert(getOrder == strides.length, s"strides length ($strides.length) must be equal to dimensions length ($getOrder)")

  /** Dimensions returns a protective copy of the input dimensions. */
  final def getDimensions() = dimensions.toArray.clone()


}


class DenseMatrixB3[@spec(Double, Int, Float, Long) V](
                                                       override protected val internalData: Seq[V],
                                                       override protected val dimensions: Indices3,
                                                       override protected val offsets:    Indices3,
                                                       override protected val strides:    Indices3
                                                       )
  extends DenseMatrixB[V](internalData, dimensions, offsets, strides) {

  override lazy val indicesToLinearIndex: (Seq[Int] => Int) = {
    val tempret = ((ind: Seq[Int]) =>
      (offsets.index0 + strides.index0*ind(0)) +
        (offsets.index1 + strides.index1*ind(1)) +
        (offsets.index2 + strides.index2*ind(2))
    )

    assert(internalData.length > tempret(dimensions),
      s"Not enough input internalData specified (length = $internalData.length) for given input dimensions"  )
    tempret
  }

  override lazy val linearIndexToIndices: (Int => Seq[Int]) = {
    ((linearInd: Int) =>
    Array(  (linearInd - offsets(0))%strides(0), (linearInd - offsets(1))%strides(1), (linearInd - offsets(2))%strides(2) )
  }

  override def apply(indices: Int*) = internalData( indicesToLinearIndex(indices) )


}