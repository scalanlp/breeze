package breeze.linalg

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.{specialized => spec}

 /**
  * A DenseTensorN is an N-dimensional tensor with all elements flattened into an array. It is
  * column major unless isTranspose is true.
  *
  * @author sujithjay
  * @param data The underlying data.
  *             Column-major unless isTranspose is true.
  *             Mutate at your own risk.
  *             Note that this tensor may be a view of the data.
  * @param shape
  * @param offset
  * @param stride
  * @param isTranspose
  */
@SerialVersionUID(1L)
final class DenseTensorN[@spec(Double, Int, Float, Long) V](
  val data: Array[V],
  val shape: IndexedSeq[Int],
  val offset: Int,
  val stride: Int,
  val isTranspose: Boolean = false)
  extends TensorN[V]
  with TensorNLike[V, DenseTensorN[V]]
  with Serializable {

  def this(data: Array[V]) = this(data, IndexedSeq[Int](data.length), 0, 1)
  def this(shape: IndexedSeq[Int])(implicit man: ClassTag[V]) = this(new Array[V](shape.product), shape, 0, 1)

  def apply(index: IndexedSeq[Int]): V = {
    val isValidIndex = index
      .zipWithIndex
      .forall{ idx =>
        idx._2 > -shape(idx._1) && idx._2 < shape(idx._1)
      }
    if(!isValidIndex) throw new IndexOutOfBoundsException("")
    val trueIndex: IndexedSeq[Int] = index
      .zipWithIndex
      .map{ idx =>
        if(idx._2 < 0) idx._2 + shape(idx._1) else idx._2
      }
    data(linearIndex(trueIndex))
  }

  def update(index: IndexedSeq[Int], v: V): Unit = {
    val isValidIndex = index
      .zipWithIndex
      .forall{ idx =>
        idx._2 > -shape(idx._1) && idx._2 < shape(idx._1)
    }
    if(!isValidIndex) throw new IndexOutOfBoundsException("")
    val trueIndex: IndexedSeq[Int] = index
      .zipWithIndex
      .map{ idx =>
        if(idx._2 < 0) idx._2 + shape(idx._1) else idx._2
      }
    data(linearIndex(trueIndex)) = v
  }

  def size: Int = shape.product

  def activeSize: Int = data.length

  def iterator: Iterator[(IndexedSeq[Int], V)] = for(i <- Iterator.range(0, size)) yield ndIndex(i) -> data(i)

  def activeIterator: Iterator[(IndexedSeq[Int], V)] = iterator

  def valuesIterator: Iterator[V] = for(i <- Iterator.range(0, size)) yield data(i)

  def activeValuesIterator: Iterator[V] = valuesIterator

  def keysIterator: Iterator[IndexedSeq[Int]] = for(i <- Iterator.range(0, size)) yield ndIndex(i)

  def activeKeysIterator: Iterator[IndexedSeq[Int]] = keysIterator

  def repr: DenseTensorN[V] = this

   /**
     * Calculates the linear index from its equivalent n-dimensional index
     * @param ndIndex
     * @return The linear index
     */
  def linearIndex(ndIndex: IndexedSeq[Int]): Int = {
    val logicalIndex = if(isTranspose){
      ndIndex.zipWithIndex.foldLeft(0){(prev, idx) =>
        idx._2 + shape(idx._1) * prev
      }
    }
    else {
      ndIndex.zipWithIndex.foldRight(0){(idx, prev) =>
        idx._2 + shape(idx._1) * prev
      }
    }

    logicalIndex * (stride + 1) + offset
  }

  /**
   * Calculates the n-dimensional index from its equivalent linear index
   * @param linearIndex
   * @return The n-dimension index
   */
  def ndIndex(linearIndex: Int): IndexedSeq[Int] = {
    val logicalIndex = (linearIndex - offset) / (stride + 1)
    val ret = mutable.ArrayBuffer[Int]()
    if(isTranspose){
      shape.foldRight((this.size, logicalIndex)) { (dim, tup) =>
        val sz = tup._1 / dim
        val rem = tup._2 % sz
        val idx = tup._2 / sz
        ret += idx
        (sz, rem)
      }
    }
    else {
      shape.foldLeft((this.size, logicalIndex)) { (tup, dim) =>
        val sz = tup._1 / dim
        val rem = tup._2 % sz
        val idx = tup._2 / sz
        ret += idx
        (sz, rem)
      }
    }
    ret
  }

}

