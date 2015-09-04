package breeze.linalg.immutable

import breeze.linalg.product
import breeze.linalg.traits.{Indices3, Indices}

import scala.collection.mutable
import scala.{specialized=>spec}

/**
 * Created by ktakagaki on 15/04/09.
 */
trait MatrixB[@spec(Double, Int, Float, Long) V] {

  /** Order of the matrix, from 0(wrapped scalar) to 1(vector), ...
    */
  def getOrder(): Int
  /** Matrix dimensions. Order zero (wrapped scalar) will return an empty Array[Int]*/
  def getDimensions(): Array[Int]

  /** This is the central point to implement for all children.
    * it should be implemented as a var or a val, such that the JVM can inline.
    */
  def indicesToLinearIndex(): (Seq[Int] => Int)
  /** This is the central point to implement for all children.
    * it should be implemented as a var or a val, such that the JVM can inline.
    */
  def linearIndexToIndices(): (Int => Seq[Int])

  def apply(indices: Int*): V

  def toArray(): Array = ???
  def equals(matrix: MatrixB): Boolean = ???
  def reshape(): MatrixB = ???

  def iterator: Iterator[(Int, V)] = ???
  def activeIterator: Iterator[(Int, V)] = ???
  def valuesIterator: Iterator[V] = ???
  def activeValuesIterator: Iterator[V] = ???
  def keysIterator: Iterator[Int] = ???
  def activeKeysIterator: Iterator[Int] = ???

  //ToDo: Make function delete as in DenseMatrix, perhaps not within object but as delete( DenseMatrixB, Int, Axis)

}