package breeze.linalg.immutable

import breeze.linalg.product

import scala.collection.mutable
import scala.{specialized=>spec}

/**
 * Created by ktakagaki on 15/04/09.
 */
class Matrix[@spec(Double, Int, Float, Long) V](private var data: Array[V], private var dimensions: Array[Int]) {

  assert( dimensions != null, "argument dims cannot be null")
  assert( data != null, "argument data cannot be null")

  protected val internalDimensionality = dimensions.length
  assert( internalDimensionality > 0, "argument dims cannot be zero-length")
  assert( internalDimensionality <= 3, "Currently supporting only up to 3D matrices")
  final def getDimensionality() = internalDimensionality
  
  /**Dimensions holds a protective copy of the input dimensions.
    *Reshaping functions should be able to access the contents, hence protected scope.*/
  protected val internalDimensions: Seq[Int] = dimensions.clone
  dimensions = null //free up the input reference for potential GC
  final def getDimensions() = internalDimensions.toArray.clone()

  protected val internalDataLength: Int = product(internalDimensions)
  final def getDataLength() = internalDataLength

  protected var internalData = data.clone()
  data = null //free up the input reference for potential GC

  //The following would have to be taken out for mutable sparse matrices...
  assert(
    data.length == internalDataLength,
    s"dataInput.length ({$internalData.length}) must be the length of the product of dims ($internalDataLength)!" )

  /** A list of transpositions. Take for example transposition=[0, 2, 1].
    * This means that:
    *    the 0th dimension in the internal array is the 0th dimension in the result
    *    the 1th dimension in the internal array is the 2th dimension in the result
    *    the 2th dimension in the internal array is the 1th dimension in the result
    */
  protected var internalTransposition = null
  protected var internalDimensionMultipliers: Array[Int] = null
  protected var indicesTolinearIndex: (Array[Int] => Int) = null
  protected var linearIndexToIndices: (Int => Array[Int]) = null

  protected def updateIndexing(): Unit = {
    if( internalTransposition == null ){
      internalDimensionMultipliers = new Array[Int](internalDimensionality)
      var cum = 1
      internalDimensionMultipliers(internalDimensionality-1) = 1
      for( c <- internalDimensionality-1 to 0 by -1){
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
