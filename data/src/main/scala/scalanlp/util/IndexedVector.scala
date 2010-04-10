/*
 Copyright 2009 David Hall, Daniel Ramage

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
package scalanlp.util;

import scalala.collection.MergeableSet;
import scalala.tensor.{Tensor1, Vector};
import scalala.tensor.dense.DenseVector;

/**
 * An IndexedVector is a view of an underlying Vector that acts like
 * a Tensor1[I] by using lookup functions to convert keys to offsets.
 * IndexedVectors are implicitly converted to the underlying Vector
 * type when needed.   An IndexedVector of a DenseVector is furthermore
 * implicitly converted to its underlying array, so it can be used with
 * any array-expecting function.
 *
 * @author dramage
 */
class IndexedVector[I,+V<:Vector](val vector : V, val index : Index[I])
extends Tensor1[I] {

  if (vector.size != index.size) {
    throw new Exception("Vector and index must be the same size");
  }

  override def default =
    vector.default;

  override def default_=(update : Double) =
    vector.default = update;

  override def like =
    new IndexedVector(vector.like, index);

  lazy val _domain = MergeableSet(index.toSet);
  override def domain =
    _domain;

  override def apply(key : I) =
    vector(index(key));

  override def update(key : I, value : Double) =
    vector(index(key)) = value;

  override def activeDomain =
    MergeableSet(vector.activeDomain.view.map(index.get));

  override def activeElements =
    vector.activeElements.map(tup => (index.get(tup._1), tup._2));

  override def activeKeys =
    vector.activeKeys.map(index.get);

  override def activeValues =
    vector.activeValues;

  override def iterator =
    vector.iterator.map(tup => (index.get(tup._1), tup._2));

  override def keysIterator =
    index.iterator;

  override def valuesIterator =
    vector.valuesIterator;
}

object IndexedVector {
  /** Implicit conversion of an IndexedVector to its underlying vector */
  implicit def iIndexedVectorToVector[I,V<:DenseVector](indexedVector : IndexedVector[I,V]) : V =
    indexedVector.vector;

  /** Implicit conversion of a DenseVector backed IndexedVector to its underlying array. */
  implicit def iIndexedVectorToArray[I,V<:DenseVector](indexedVector : IndexedVector[I,V]) : Array[Double] =
    indexedVector.vector.data;
}
