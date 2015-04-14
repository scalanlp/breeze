package breeze.linalg.immutable

import breeze.linalg.product

import scala.{specialized => spec}

/**
 * Created by ktakagaki on 15/04/09.
 */
class DenseMatrixB[@spec(Double, Int, Float, Long) V](private var data: Array[V], private var dimensions: Array[Int])
        extends Matrix[V]{

  assert( dimensions != null, "argument dims cannot be null")
  assert( data != null, "argument data cannot be null")

  protected val internalDimensions = dimensions.length
  assert( internalOrder >= 0, "Argument dims cannot be negative")
  assert( internalOrder <= 3, "Currently supporting only up to 3D matrices")
  final def getDimensions() = internalOrder
  
  /**Dimensions holds a protective copy of the input dimensions.
    *Reshaping functions should be able to access the contents, hence protected scope.*/
  protected val internalOrder: Seq[Int] = dimensions.clone
  dimensions = null //free up the input reference for potential GC
  final def getOrder() = internalOrder.toArray.clone()

  protected val internalDataLength: Int = product(internalOrder)
  final def getDataLength() = internalDataLength

  protected var internalData = data.clone()
  data = null //free up the input reference for potential GC

  //The following would have to be taken out for mutable sparse matrices...
  assert(
    data.length == internalDataLength,
    s"dataInput.length ({$internalData.length}) must be the length of the product of dims ($internalDataLength)!" )

  /** A list of transpositions. Take for example transposition=[0, 2, 1].
    * This means that:
    *    the 0th dimension of output is the 0th dimension internally
    *    the 1th dimension of output is the 2th dimension internally
    *    the 2th dimension of output is the 1th dimension internally
    */
  protected var outputDimensionToInternal = null
  protected var outputDimensionMultipliers: Array[Int] = null
  protected var indicesTolinearIndex: (Array[Int] => Int) = null
  protected var linearIndexToIndices: (Int => Array[Int]) = null

  protected def updateIndexing(): Unit = {
    if( internalTransposition == null ){
      internalDimensionMultipliers = new Array[Int](internalOrder)
      var cum = 1
      internalDimensionMultipliers(internalOrder-1) = 1
      for( c <- internalOrder-1 to 0 by -1){
        cum *=

      }

    }
  }
  private var linearIndexFunction = {
    if(dimensionality == 1) (d: Array[Int]) => d(0)
    else if(dimensionality == 2)
  }

  /** Calculates the index into the data array for row and column */
  def linearIndex(dimensions: Array[Int]): Int = {
    assert(dimensions != null, "cannot specify null dimensions!")

    if(isTranspose)
      offset + col + row * majorStride
    else
      offset + row + col * majorStride
  }

  def rowColumnFromLinearIndex(index: Int): (Int, Int) = {
    val r = (index - offset)%majorStride
    val c = (index - offset)/majorStride
    if(isTranspose) {
      (c, r)
    } else {
      (r,c)
    }
  }



}
