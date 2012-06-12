package breeze.linalg

import scala.{specialized=>spec}
import breeze.storage.Storage
import org.netlib.blas.Dgemm

/**
 *
 * @author dlwh
 */

trait MatrixLike[@spec E, +Self <: Matrix[E]] extends Tensor[(Int, Int), E] with TensorLike[(Int, Int), E, Self] {

}

trait Matrix[@spec E] extends MatrixLike[E, Matrix[E]] {
  final def apply(i: (Int, Int)) = apply(i._1, i._2)
  final def update(i: (Int, Int), e: E) {
    update(i._1, i._2, e)
  }

  def apply(i: Int, j: Int):E
  def update(i: Int, j: Int, e: E)

  def size = rows * cols
  def rows: Int
  def cols: Int

  def iterator = for(i <- Iterator.range(0, rows); j <- Iterator.range(0, cols)) yield (i -> j) -> apply(i, j)

  def valuesIterator = for(i <- Iterator.range(0, rows); j <- Iterator.range(0, cols)) yield apply(i, j)

  def keysIterator = for(i <- Iterator.range(0, rows); j <- Iterator.range(0, cols)) yield (i -> j)

}


trait StorageMatrix[@spec E] extends Matrix[E] with MatrixLike[E, StorageMatrix[E]] with Storage[E] {
  def offset: Int
  def majorStride: Int
  def isTranspose: Boolean

  @inline final def apply(row: Int, col: Int) = {
    if(row < 0 || row > rows) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
    if(col < 0 || col > cols) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
      rawApply(linearIndex(row, col))
  }


  @inline final protected def linearIndex(row: Int, col: Int): Int = {
    if(isTranspose)
      offset + col + row * majorStride
    else
      offset + row + col * majorStride
  }

  @inline
  final def update(row: Int, col: Int, v: E) {
    if(row < 0 || row > rows) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
    if(col < 0 || col > cols) throw new IndexOutOfBoundsException((row,col) + " not in [0,"+rows+") x [0," + cols+")")
    rawUpdate(linearIndex(row, col), v)
  }

}