
package breeze.util

/*
 Copyright 2010 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import breeze.linalg._
import breeze.storage.DefaultArrayValue
import breeze.collection.mutable.{SparseArray, SparseArrayMap}
import java.util


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
  def mkSparseVector(): SparseVector[Double] = {
    SparseVector.zeros[Double](index.size)
  }

  /**
   * Creates a DenseVector[Double] with the index's size
   */
  final def mkDenseVector(default: Double=0.0):DenseVector[Double] = {
    val array = new Array[Double](index.size)
    util.Arrays.fill(array, default)
    new DenseVector(array)
  }

  /**
   * Creates a Vector[Double] of some sort with the index's size.
   */
  final def mkVector():Vector[Double] = mkSparseVector()

  /**
   * makes a matrix of some sort with the index's size as rows and cols
   */
  final def mkMatrix():DenseMatrix[Double] = {
    DenseMatrix.zeros(index.size,index.size)
  }

  /**
   * Decodes a vector back to a Counter[T,Double]
   */
  def decode(v: Vector[Double]):Counter[T,Double] = {
    val ctr = Counter[T,Double]()
    for( (i,v) <- v.active.pairs) {
      ctr(index.get(i)) = v
    }
    ctr
  }


  /**
   * Encodes a DoubleCounter as a Vector[Double]. All elements in the counter must be in the index.
   */
  def encodeDense(c: Tensor[T,Double], ignoreOutOfIndex:Boolean = false):DenseVector[Double] = {
    val vec = mkDenseVector()
    for( (k,v) <- c.active.pairs) {
      val ki = index(k)
      if(ki < 0) {if(!ignoreOutOfIndex) throw new RuntimeException("Error, not in index: " + k)}
      else  vec(ki) = v
    }
    vec
  }

  /**
   * Encodes a DoubleCounter as a SparseVector[Double]. All elements in the counter must be in the index.
   */
  def encodeSparse(c: Tensor[T,Double], ignoreOutOfIndex: Boolean = false):SparseVector[Double] = {
    val vec = new VectorBuilder[Double](index.size)
    vec.reserve(c.activeSize)
    for( (k,v) <- c.active.pairs) {
      val ki = index(k)
      if(ki < 0) {if(!ignoreOutOfIndex) throw new RuntimeException("Error, not in index: " + k)}
      else  vec.add(ki, v)
    }
    vec.toSparseVector
  }

  /**
   * Encodes a DoubleCounter as a Vector[Double]. All elements in the counter must be in the index.
   */
  def encode(c: Tensor[T,Double], ignoreOutOfIndex: Boolean = false):Vector[Double] = {
    val vec = mkVector()
    for( (k,v) <- c.active.pairs) {
      val ki = index(k)
      if(ki < 0) {if(!ignoreOutOfIndex) throw new RuntimeException("Error, not in index: " + k)}
      else  vec(ki) = v
    }
    vec
  }

    /**
   * Encodes a Tensor[(T,T),Double] as a DenseMatrix[Double]. All elements in the counter must be in the index.
   */
  def encode(c: Tensor[(T,T),Double]):DenseMatrix[Double] = {
    val vec = mkMatrix()
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
  def mkArray[V:ClassManifest] = new Array[V](index.size)
  
  /**
   * Fills an array of arbitrary type with the value provided and with the index's size.
   */
  def fillArray[V:ClassManifest](default : => V): Array[V] = Array.fill(index.size)(default)

  /**
   * Fills an array of arbitrary type by tabulating the function
   */
  def tabulateArray[V:ClassManifest](f: T=>V): Array[V] = {
    val arr = new Array[V](index.size)
    for((e,i) <- index.pairs) {
      arr(i) = f(e)
    }
    arr
  }

  /**
   * Fills a DenseVector[Double] with each index given by the result of the function.
   */
  def tabulateDenseVector(f: T=>Double)  = new DenseVector[Double](tabulateArray[Double](f))

  /**
   * Converts an array into a Map from T's to whatever was in the array.
   */
  def decode[V](array: Array[V]):Map[T,V] = {
    Map.empty ++ array.zipWithIndex.map{ case (v,i) => (index.get(i),v)}
  }

  def fillSparseArrayMap[V:ClassManifest:DefaultArrayValue](default: =>V) = new SparseArrayMap[V](index.size, default)

  def mkSparseArray[V:ClassManifest:DefaultArrayValue] = new SparseArray[V](index.size)
  def decode[V](array: SparseArray[V]):Map[T,V] = {
    Map.empty ++ array.iterator.map{ case (i,v) => (index.get(i),v)}
  }


}

/**
 * For encoding counters as vectors and decoding vectors back to counters
 *
 * @author dlwh
 */


object Encoder {
  def fromIndex[T](ind: Index[T]):Encoder[T] = new SimpleEncoder(ind)

  @SerialVersionUID(1)
  private class SimpleEncoder[T](val index: Index[T]) extends Encoder[T] with Serializable
}

