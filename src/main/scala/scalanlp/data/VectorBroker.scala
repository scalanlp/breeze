package scalanlp.data

import scalanlp.util.Index;
import scalala.tensor.dense.DenseVector
import scalanlp.collection.mutable.SparseArray
import scalanlp.counters.Counters._;
import scalala.tensor._;
import scalala.tensor.adaptive._;
import java.util.Arrays
import scalala.Scalala._;


/**
 * For encoding counters as vectors and decoding vectors back to counters
 *
 * @author dlwh
 */
trait VectorBroker[T] {
  val index: Index[T]

  def mkVector(default: Double=0.0):Vector = {
    val vec = new AdaptiveVector(index.size)
    vec.default = default;
    vec
  }

  final def mkDenseVector(default: Double=0.0):DenseVector = {
    val vec = new DenseVector(index.size)
    Arrays.fill(vec.data,default);
    vec
  }

  def decode(v: Vector):DoubleCounter[T] = {
    val ctr = DoubleCounter[T]();
    for( (i,v) <- v.activeElements) {
      ctr(index.get(i)) = v;
    }
    ctr
  }

  def encode(c: DoubleCounter[T]):Vector = {
    val vec = mkVector(c.default);
    for( (k,v) <- c) {
      vec(index(k)) = v;
    }
    vec
  }

  def mkArray[V:ClassManifest] = new Array[V](index.size);
  def fillArray[V:ClassManifest](default : => V) = Array.fill(index.size)(default);

  def decode[V](array: Array[V]):Map[T,V] = {
    Map.empty ++ array.zipWithIndex.map{ case (v,i) => (index.get(i),v)}
  }

  def mkSparseArray[V:ClassManifest] = new SparseArray[V](index.size);
  def fillSparseArray[V:ClassManifest](deflt : => V) = {
    val arr = new SparseArray[V](index.size) {
      override def default(k: Int) = {
        val v = deflt;
        update(k,v);
        v
      }
    }
    arr
  }

  def decode[V](array: SparseArray[V]):Map[T,V] = {
    Map.empty ++ array.map{ case (i,v) => (index.get(i),v)}
  }


}

object VectorBroker {
  def fromIndex[T](ind: Index[T]):VectorBroker[T] = new VectorBroker[T] {
    val index = ind;
  }
}