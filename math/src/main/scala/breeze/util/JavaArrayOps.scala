package breeze.util

import breeze.linalg.immutable.Matrix
import breeze.linalg.{DenseVector, DenseMatrix}
import scala.reflect.ClassTag
import breeze.math.Complex

/**This utility class facilitates transparent access of breeze data objects from plain Java without Scala-related Generic complications.
 * @author ktakagaki
 * @date 03/20/2014.
 */
object JavaArrayOps {

  def dvCToArray(data: DenseVector[Complex]): Array[Complex] = data.toArray
  def dvDToArray(data: DenseVector[Double]): Array[Double] = data.toArray
  def dvFToArray(data: DenseVector[Float]): Array[Float] = data.toArray
  def dvIToArray(data: DenseVector[Int]): Array[Int] = data.toArray
  def dvLToArray(data: DenseVector[Long]): Array[Long] = data.toArray

  def dmCToArray2(data: DenseMatrix[Complex]): Array[Array[Complex]] = dmToArray2(data)
  def dmDToArray2(data: DenseMatrix[Double]): Array[Array[Double]] = dmToArray2(data)
  def dmFToArray2(data: DenseMatrix[Float]): Array[Array[Float]] = dmToArray2(data)
  def dmIToArray2(data: DenseMatrix[Int]): Array[Array[Int]] = dmToArray2(data)
  def dmLToArray2(data: DenseMatrix[Long]): Array[Array[Long]] = dmToArray2(data)

  def arrayCToDv(array: Array[Complex]): DenseVector[Complex] = arrayToDv( array )
  def arrayDToDv(array: Array[Double]): DenseVector[Double] = arrayToDv( array )
  def arrayFToDv(array: Array[Float]): DenseVector[Float] = arrayToDv( array )
  def arrayIToDv(array: Array[Int]): DenseVector[Int] = arrayToDv( array )
  def arrayLToDv(array: Array[Long]): DenseVector[Long] = arrayToDv( array )

  def array2CToDm(array: Array[Array[Complex]]): DenseMatrix[Complex] = array2ToDm( array )
  def array2DToDm(array: Array[Array[Double]]): DenseMatrix[Double] = array2ToDm( array )
  def array2FToDm(array: Array[Array[Float]]): DenseMatrix[Float] = array2ToDm( array )
  def array2IToDm(array: Array[Array[Int]]): DenseMatrix[Int] = array2ToDm( array )
  def array2LToDm(array: Array[Array[Long]]): DenseMatrix[Long] = array2ToDm( array )

  // <editor-fold defaultstate="collapsed" desc=" implementations ">

  def dvToArray[@specialized(Int, Double, Long, Float) V: ClassTag](dv: DenseVector[V]): Array[V] = dv.toArray

  def dmToArray2[@specialized(Int, Double, Long, Float) V: ClassTag](dm: DenseMatrix[V]): Array[Array[V]] = {
    val ret = new Array[Array[V]](dm.rows)
    var rowI = 0
    while (rowI < dm.rows) {
      ret(rowI) = new Array[V](dm.cols)
      var colI = 0
      while(colI < dm.cols) {
        ret(rowI)(colI) = dm(rowI, colI)
        colI += 1
      }
      //ret(i) = dm( i, :: ).toArray //How else to circumvent Transpose[] wrapper?
      rowI += 1
    }
    ret
  }

  def arrayToDv[@specialized(Int, Double, Long, Float) V: ClassTag](array: Array[V]): DenseVector[V] = new DenseVector(array)

  /** Constructs DenseMatrix from Array[Array[V]] input. Input is in row-major like
    * format, similar to DenseMatrix( (1,2 3), (4,5,6),... ) syntax, which is defined in [[Matrix]].
    * This constructor was written for JavaCompatible.
    * @param values
    * @return
    */
  def array2ToDm[@specialized(Int, Double, Long, Float) V: ClassTag](values: Array[Array[V]]): DenseMatrix[V] = {

    val tempRows= values.length
    val tempCols = values(0).length
    val tempret = new Array[V]( tempRows*tempCols )

    var rowIndex = 0
    var tempretIndex = 0
    while(rowIndex < tempRows) {
      //raggedness check
      require(values(rowIndex).length == tempCols, "Input Array[Array[V]] is ragged!")
      rowIndex += 1
    }

    var colIndex = 0
    while(colIndex < tempCols) {
      rowIndex = 0
      while (rowIndex < tempRows) {
        tempret(tempretIndex) = values(rowIndex)(colIndex)
        tempretIndex += 1
        rowIndex += 1
      }
      colIndex += 1
    }
    new DenseMatrix(tempRows, tempCols, tempret)
  }

  // </editor-fold>

}
