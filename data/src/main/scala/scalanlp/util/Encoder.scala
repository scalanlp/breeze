
package scalanlp.util

/*
 Copyright 2010 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import scalala.tensor.mutable._
import scalala.tensor.{Counter=>imCounter};
import scalala.collection.sparse.{DefaultArrayValue, SparseArray}

import scalala.tensor.dense.DenseVector
import scalala.tensor.dense.DenseVectorCol
import scalala.tensor.sparse._
import scalanlp.tensor.sparse.OldSparseVector
import scalanlp.collection.mutable.SparseArrayMap


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
    SparseVector.zeros[Double](index.size);
  }

  /** Creates an old style OldSparseVector with the given defaults */
  def mkOldSparseVector(default: Double = 0.0):OldSparseVector = {
    new OldSparseVector(index.size, default);
  }

  /**
   * Creates a DenseVector[Double] with the index's size
   */
  final def mkDenseVector(default: Double=0.0):DenseVectorCol[Double] = {
    val vec = DenseVector.fill[Double](index.size)(default);
    vec
  }

  /**
   * Creates a Vector[Double] of some sort with the index's size.
   */
  final def mkVector():Vector[Double] = mkSparseVector;

  /**
   * Decodes a vector back to a Counter[T,Double]
   */
  def decode(v: Vector[Double]):Counter[T,Double] = {
    val ctr = Counter[T,Double]();
    for( (i,v) <- v.nonzero.pairs) {
      ctr(index.get(i)) = v;
    }
    ctr
  }


  /**
   * Encodes a DoubleCounter as a Vector[Double]. All elements in the counter must be in the index.
   */
  def encodeDense(c: imCounter[T,Double]):DenseVector[Double] = {
    val vec = mkDenseVector();
    for( (k,v) <- c.nonzero.pairs) {
      val ki = index(k)
      if(ki < 0) throw new RuntimeException("Error, not in index: " + k)

      vec(ki) = v;
    }
    vec
  }

  /**
   * Encodes a DoubleCounter as a SparseVector[Double]. All elements in the counter must be in the index.
   */
  def encodeSparse(c: imCounter[T,Double]):SparseVector[Double] = {
    val vec = mkSparseVector();
    for( (k,v) <- c.nonzero.pairs) {
      vec(index(k)) = v;
    }
    vec
  }

  /**
   * Encodes a DoubleCounter as a SparseVector[Double]. All elements in the counter must be in the index.
   */
  def encodeOldSparse(c: imCounter[T,Double], default: Double=0.0):OldSparseVector = {
    val vec = mkOldSparseVector(default);
    for( (k,v) <- c.nonzero.pairs) {
      vec(index(k)) = v;
    }
    vec
  }

  /**
   * Encodes a DoubleCounter as a Vector[Double]. All elements in the counter must be in the index.
   */
  def encode(c: imCounter[T,Double]):Vector[Double] = {
    val vec = mkVector();
    for( (k,v) <- c.nonzero.pairs) {
      val ki = index(k)
      if(ki < 0) throw new RuntimeException("Error, not in index: " + k)

      vec(ki) = v;
    }
    vec
  }

  /**
   * Creates an array of arbitrary type with the index's size.
   */
  def mkArray[V:ClassManifest] = new Array[V](index.size);
  
  /**
   * Fills an array of arbitrary type with the value provided and with the index's size.
   */
  def fillArray[V:ClassManifest](default : => V): Array[V] = Array.fill(index.size)(default);

  /**
   * Fills an array of arbitrary type by tabulating the function
   */
  def tabulateArray[V:ClassManifest](f: T=>V): Array[V] = {
    val arr = new Array[V](index.size);
    for((e,i) <- index.pairs) {
      arr(i) = f(e);
    }
    arr;
  }

  /**
   * Fills a DenseVector[Double] with each index given by the result of the function.
   */
  def tabulateDenseVector(f: T=>Double)  = new DenseVectorCol[Double](tabulateArray[Double](f));

  /**
   * Converts an array into a Map from T's to whatever was in the array.
   */
  def decode[V](array: Array[V]):Map[T,V] = {
    Map.empty ++ array.zipWithIndex.map{ case (v,i) => (index.get(i),v)}
  }

  def fillSparseArrayMap[V:ClassManifest:DefaultArrayValue](default: =>V) = new SparseArrayMap[V](index.size, default)

  def mkSparseArray[V:ClassManifest:DefaultArrayValue] = new SparseArray[V](index.size);
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
  def fromIndex[T](ind: Index[T]):Encoder[T] = new SimpleEncoder(ind);

  @SerialVersionUID(1)
  private class SimpleEncoder[T](val index: Index[T]) extends Encoder[T] with Serializable;
}

