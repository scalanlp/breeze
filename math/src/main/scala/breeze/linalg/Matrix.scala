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

  def keySet: Set[(Int, Int)] = new Set[(Int, Int)] {
    def contains(elem: (Int, Int)): Boolean = elem._1 >= 0 && elem._1 < rows && elem._2 >= 0 && elem._2 < cols

    def +(elem: (Int, Int)): Set[(Int, Int)] = Set() ++ iterator + elem
    def -(elem: (Int, Int)): Set[(Int, Int)] = Set() ++ iterator - elem

    def iterator: Iterator[(Int, Int)] = for{ j <- Iterator.range(0, cols); i <- Iterator.range(0, rows)} yield (i, j)
  }

  def iterator = for(i <- Iterator.range(0, rows); j <- Iterator.range(0, cols)) yield (i -> j) -> apply(i, j)

  def valuesIterator = for(i <- Iterator.range(0, rows); j <- Iterator.range(0, cols)) yield apply(i, j)

  def keysIterator = for(i <- Iterator.range(0, rows); j <- Iterator.range(0, cols)) yield (i -> j)

}


trait StorageMatrix[@spec E] extends Matrix[E] with MatrixLike[E, StorageMatrix[E]] with Storage[E] {
  def offset: Int
  def majorStride: Int
  def isTranspose: Boolean



}