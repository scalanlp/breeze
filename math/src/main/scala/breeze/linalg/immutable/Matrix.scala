package breeze.linalg.immutable

import breeze.linalg.product
import breeze.linalg.traits.{Indices3, Indices}

import scala.collection.mutable
import scala.{specialized=>spec}

/**
 * Created by ktakagaki on 15/04/09.
 */
trait Matrix[@spec(Double, Int, Float, Long) V] {

  def getOrder(): Int
  def getDimensions(): Array[Int]
  //def getDataLength(): Int

  def indicesToLinearIndex(indices: Indices): Int
  def linearIndexToIndices(linearIndex: Int): Indices

}

trait Matrix3[@spec(Double, Int, Float, Long) V] extends Matrix[V] {

  override def indicesToLinearIndex(indices: Indices3)

}
