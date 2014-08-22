package breeze.util

import breeze.linalg._
import breeze.collection.mutable._
import breeze.math.Semiring
import breeze.storage._
import java.util
import scala.reflect.ClassTag

/**
 * For encoding counters as vectors and decoding vectors back to counters
 *
 * @author dlwh
 */
trait Encoder[T] {
  val index: Index[T]

  /**
   * Creates a SparseVector[Double] with the index's size
   */
  def mkSparseVector[S:Zero:ClassTag](): SparseVector[S] = {
    SparseVector.zeros[S](index.size)
  }

  /**
   * Creates a DenseVector[Double] with the index's size
   */
  final def mkDenseVector[S:Semiring:ClassTag](default: S): DenseVector[S] = {
    val array = Array.fill(index.size)(default)
    new DenseVector(array)
  }

  final def mkDenseVector[S:Semiring:ClassTag](): DenseVector[S] = mkDenseVector[S](implicitly[Semiring[S]].zero)

  /**
   * Creates a Vector[Double] of some sort with the index's size.
   */
  final def mkVector[S:Zero:ClassTag]():Vector[S] = mkSparseVector[S]()

  /**
   * makes a matrix of some sort with the index's size as rows and cols
   */
  final def mkMatrix[S:Zero:ClassTag]():DenseMatrix[S] = {
    DenseMatrix.zeros[S](index.size,index.size)
  }

  /**
   * Decodes a vector back to a Counter[T,Double]
   */
//  def decode(v: Vector[Double], keepZeros: Boolean = false):Counter[T,Double] = {
//    val ctr = Counter[T,Double]()
//    for( (i,v) <- v.active.pairs) {
//      if(keepZeros || v != 0.0)
//        ctr(index.get(i)) = v
//    }
//    ctr
//  }

  def decode[S:Zero:Semiring](v: Vector[S], keepZeros: Boolean = false): Counter[T,S] = {
    val ctr = Counter[T,S]()
    for( (i,v) <- v.active.pairs) {
      if(keepZeros || v != implicitly[Zero[S]].zero)
        ctr(index.get(i)) = v
    }
    ctr
  }

  /**
   * Encodes a DoubleCounter as a Vector[Double].
   * All elements in the counter must be in the index unless ignoreOutOfIndex is true
   */
  def encodeDense[S:Zero:Semiring:ClassTag](c: Tensor[T,S], ignoreOutOfIndex:Boolean = false):DenseVector[S] = {
    val vec = mkDenseVector[S]()
    for( (k,v) <- c.active.pairs) {
      val ki = index(k)
      if(ki < 0) {if(!ignoreOutOfIndex) throw new RuntimeException("Error, not in index: " + k)}
      else  vec(ki) = v
    }
    vec
  }

  /**
   * Encodes a DoubleCounter as a SparseVector[Double].
   * All elements in the counter must be in the index unless ignoreOutOfIndex is true
   */
  def encodeSparse[S:Zero:Semiring:ClassTag](c: Tensor[T,S], ignoreOutOfIndex: Boolean = false):SparseVector[S] = {
    val vec = new VectorBuilder[S](index.size)
    vec.reserve(c.activeSize)
    for( (k,v) <- c.active.pairs) {
      val ki = index(k)
      if(ki < 0) {if(!ignoreOutOfIndex) throw new RuntimeException("Error, not in index: " + k)}
      else  vec.add(ki, v)
    }
    vec.toSparseVector
  }

  /**
   * Encodes a DoubleCounter as a Vector[Double].
   * All elements in the counter must be in the index unless ignoreOutOfIndex is true
   */
  def encode[S:Zero:ClassTag](c: Tensor[T,S], ignoreOutOfIndex: Boolean = false):Vector[S] = {
    val vec = mkVector[S]()
    for( (k,v) <- c.active.pairs) {
      val ki = index(k)
      if(ki < 0) {if(!ignoreOutOfIndex) throw new RuntimeException("Error, not in index: " + k)}
      else  vec(ki) = v
    }
    vec
  }

    /**
   * Encodes a Tensor[(T,T),Double] as a DenseMatrix[Double].
   * All elements in the counter must be in the index unless ignoreOutOfIndex is true
   */
  def encode[S:Zero:ClassTag](c: Tensor[(T,T),S]):DenseMatrix[S] = {
    val vec = mkMatrix[S]()
    for( ((k,l),v) <- c.active.pairs) {
      val ki = index(k)
      val li = index(l)
      if(ki < 0) throw new RuntimeException("Error, not in index: " + k)
      if(li < 0) throw new RuntimeException("Error, not in index: " + k)

      vec(ki,li) = v
    }
    vec
  }

  /**
   * Creates an array of arbitrary type with the index's size.
   */
  def mkArray[V:ClassTag] = new Array[V](index.size)

  /**
   * Fills an array of arbitrary type with the value provided and with the index's size.
   */
  def fillArray[V:ClassTag](default : => V): Array[V] = Array.fill(index.size)(default)

  /**
   * Fills an array of arbitrary type by tabulating the function
   */
  def tabulateArray[V:ClassTag](f: T=>V): Array[V] = {
    val arr = new Array[V](index.size)
    for((e,i) <- index.pairs) {
      arr(i) = f(e)
    }
    arr
  }

  /**
   * Fills a DenseVector[Double] with each index given by the result of the function.
   */
  def tabulateDenseVector[S:ClassTag](f: T=>S)  = new DenseVector[S](tabulateArray[S](f))

  /**
   * Converts an array into a Map from T's to whatever was in the array.
   */
  def decode[V](array: Array[V]):Map[T,V] = {
    Map.empty ++ array.zipWithIndex.map{ case (v,i) => (index.get(i),v)}
  }

  def fillSparseArrayMap[V:ClassTag:Zero](default: =>V) = new SparseArrayMap[V](index.size, default)

  def mkSparseArray[V:ClassTag:Zero] = new SparseArray[V](index.size)
  def decode[V](array: SparseArray[V]):Map[T,V] = {
    Map.empty ++ array.iterator.map{ case (i,v) => (index.get(i),v)}
  }


}

object Encoder {
  def fromIndex[T](ind: Index[T]):Encoder[T] = new SimpleEncoder(ind)

  @SerialVersionUID(1)
  private class SimpleEncoder[T](val index: Index[T]) extends Encoder[T] with Serializable
}
