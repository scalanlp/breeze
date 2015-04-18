package breeze.linalg.immutable

import breeze.linalg.product
import breeze.linalg.traits.{Indices3, Indices}

import scala.collection.mutable
import scala.{specialized=>spec}

/**
 * Created by ktakagaki on 15/04/09.
 */
trait Matrix[@spec(Double, Int, Float, Long) V] {

  /** Order of the matrix, from 0(wrapped scalar) to 1(vector), ...
    */
  def getOrder(): Int
  /** Matrix dimensions. Order zero (wrapped scalar) will return an empty Array[Int]*/
  def getDimensions(): Array[Int]
  //def getDataLength(): Int

  def indicesToLinearIndex(): (Seq[Int] => Int)
  def linearIndexToIndices(): (Int => Seq[Int])

}