package breeze.signal

import breeze.linalg.{DenseVector, DenseMatrix}
import scala.reflect.ClassTag
import breeze.math.Complex

/**
 * @author ktakagaki
 * @date 03/20/2014.
 */
object JavaArrayOps {

  def denseVectorCToArray(data: DenseVector[Complex]): Array[Complex] = data.toArray
  def denseVectorDToArray(data: DenseVector[Double]): Array[Double] = data.toArray
  def denseVectorIToArray(data: DenseVector[Int]): Array[Int] = data.toArray

  def denseMatrixCToArray2(data: DenseMatrix[Complex]): Array[Array[Complex]] = denseMatrixToArray2(data)
  def denseMatrixDToArray2(data: DenseMatrix[Double]): Array[Array[Double]] = denseMatrixToArray2(data)
  def denseMatrixIToArray2(data: DenseMatrix[Int]): Array[Array[Int]] = denseMatrixToArray2(data)

  def arrayCToDenseVector(array: Array[Complex]): DenseVector[Complex] = arrayToDenseVector( array )
  def arrayDToDenseVector(array: Array[Double]): DenseVector[Double] = arrayToDenseVector( array )
  def arrayIToDenseVector(array: Array[Int]): DenseVector[Int] = arrayToDenseVector( array )

  def array2CToDenseMatrix(array: Array[Array[Complex]]): DenseMatrix[Complex] = array2ToDenseMatrix( array )
  def array2DToDenseMatrix(array: Array[Array[Double]]): DenseMatrix[Double] = array2ToDenseMatrix( array )
  def array2IToDenseMatrix(array: Array[Array[Int]]): DenseMatrix[Int] = array2ToDenseMatrix( array )

  def denseVectorToArray[@specialized(Int, Double) V: ClassTag](dv: DenseVector[V]): Array[V] = dv.toArray

  def denseMatrixToArray2[@specialized(Int, Double) V: ClassTag](dm: DenseMatrix[V]): Array[Array[V]] = {
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

  def arrayToDenseVector[@specialized(Int, Double) V: ClassTag](array: Array[V]): DenseVector[V] = new DenseVector(array)

  /** Constructs DenseMatrix from Array[Array[V]] input. Input is in row-major like
    * format, similar to DenseMatrix( (1,2 3), (4,5,6),... ) syntax, which is defined in [[breeze.linalg.Matrix]].
    * This constructor was written for JavaCompatible.
    * @param values
    * @return
    */
  def array2ToDenseMatrix[@specialized(Int, Double) V: ClassTag](values: Array[Array[V]]): DenseMatrix[V] = {

    val tempRows= values.length
    val tempCols = values(0).length
    val tempret = new Array[V]( tempRows*tempCols )

    var rowIndex = 0
    var tempretIndex = 0
    while(rowIndex < tempCols) {
      require( values(rowIndex).length == tempCols, "Input Array[Array[V]] is not square!")
      var colIndex = 0
      while(colIndex < tempRows){
        tempret(tempretIndex)=values(colIndex)(rowIndex)
        colIndex += 1
        tempretIndex += 1
      }
      rowIndex += 1
    }
    new DenseMatrix(tempRows, tempCols, tempret)
  }
}
