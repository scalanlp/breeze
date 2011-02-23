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



// import scalala.tensor.dense.DenseVector
// import scalala.tensor.sparse.SparseVector
// import scalanlp.collection.mutable.SparseArray
// import scalala.tensor.counters.Counters._;
// import scalala.tensor._;
// import scalala.tensor.adaptive._;
// import java.util.Arrays
// import scalala.Scalala._;


// /**
//  * For encoding counters as vectors and decoding vectors back to counters
//  *
//  * @author dlwh
//  */
// trait Encoder[T] {
//   protected val index: Index[T]

//   /**
//    * Creates a Vector of some sort with the index's size.
//    */
//   def mkVector(default: Double=0.0): Vector = {
//     val vec = new AdaptiveVector(index.size)
//     vec.default = default;
//     vec
//   }

//   /**
//    * Creates a DenseVector with the index's size
//    */
//   final def mkDenseVector(default: Double=0.0):DenseVector = {
//     val vec = new DenseVector(index.size)
//     if(default != 0.0)
//       Arrays.fill(vec.data,default);
//     vec
//   }

//   /**
//    * Creates a DenseVector with the index's size
//    */
//   final def mkSparseVector(default: Double=0.0):SparseVector = {
//     val vec = new SparseVector(index.size)
//     vec.default = default;
//     vec;
//   }

//   /**
//    * Decodes a vector back to a DoubleCounter[T]
//    */
//   def decode(v: Vector):DoubleCounter[T] = {
//     val ctr = DoubleCounter[T]();
//     for( (i,v) <- v.activeElements) {
//       ctr(index.get(i)) = v;
//     }
//     ctr
//   }

//   /**
//    * Encodes a DoubleCounter as a Vector. All elements in the counter must be in the index.
//    */
//   def encodeDense(c: DoubleCounter[T]):DenseVector = {
//     val vec = mkDenseVector(c.default);
//     for( (k,v) <- c) {
//       vec(index(k)) = v;
//     }
//     vec
//   }

//   /**
//    * Encodes a DoubleCounter as a SparseVector. All elements in the counter must be in the index.
//    */
//   def encodeSparse(c: DoubleCounter[T]):SparseVector = {
//     val vec = mkSparseVector(c.default);
//     for( (k,v) <- c) {
//       vec(index(k)) = v;
//     }
//     vec
//   }

//   /**
//    * Encodes a DoubleCounter as a Vector. All elements in the counter must be in the index.
//    */
//   def encode(c: DoubleCounter[T]):Vector = {
//     val vec = mkVector(c.default);
//     for( (k,v) <- c) {
//       vec(index(k)) = v;
//     }
//     vec
//   }

//   /**
//    * Creates an array of arbitrary type with the index's size.
//    */
//   def mkArray[V:ClassManifest] = new Array[V](index.size);
  
//   /**
//    * Fills an array of arbitrary type with the value provided and with the index's size.
//    */
//   def fillArray[V:ClassManifest](default : => V): Array[V] = Array.fill(index.size)(default);

//   /**
//    * Fills an array of arbitrary type by tabulating the function
//    */
//   def tabulateArray[V:ClassManifest](f: T=>V): Array[V] = {
//     val arr = new Array[V](index.size);
//     for((e,i) <- index.pairs) {
//       arr(i) = f(e);
//     }
//     arr;
//   }

//   /**
//    * Fills a DenseVector with each index given by the result of the function.
//    */
//   def tabulateDenseVector(f: T=>Double): DenseVector = new DenseVector(tabulateArray[Double](f));

//   /**
//    * Converts an array into a Map from T's to whatever was in the array.
//    */
//   def decode[V](array: Array[V]):Map[T,V] = {
//     Map.empty ++ array.zipWithIndex.map{ case (v,i) => (index.get(i),v)}
//   }

//   def mkSparseArray[V:ClassManifest:SparseArray.DefaultValue] = SparseArray[V](index.size);
//   def fillSparseArray[V:ClassManifest](deflt : => V) = {
//     new SparseArray[V](index.size,deflt);
//   }

//   def decode[V](array: SparseArray[V]):Map[T,V] = {
//     Map.empty ++ array.iterator.map{ case (i,v) => (index.get(i),v)}
//   }


// }

// /**
//  * For encoding counters as vectors and decoding vectors back to counters
//  *
//  * @author dlwh
//  */


// object Encoder {
//   def fromIndex[T](ind: Index[T]):Encoder[T] = new Encoder[T] {
//     val index = ind;
//   }
// }

