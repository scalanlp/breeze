package breeze.linalg

import breeze.linalg.support.CanMapValues

import scala.annotation.unchecked.uncheckedVariance
import scala.{specialized => spec}



trait TensorNLike[@spec(Double, Int, Float, Long) V, +Self <: TensorN[V]]
  extends Tensor[IndexedSeq[Int], V]
    with TensorLike[IndexedSeq[Int], V, Self]{
  def map[V2, That](fn: V => V2)(implicit canMapValues: CanMapValues[Self @uncheckedVariance, V, V2, That]): That =
    values.map(fn)

}

/**
  * @author sujithjay
  * @tparam V
  */
trait TensorN[@spec(Int, Long, Double, Float) V]
  extends TensorNLike[V, TensorN[V]]{

  def foreach[U](fn: V => U): Unit = { values.foreach(fn) }

  def shape: IndexedSeq[Int]

  def keySet: Set[IndexedSeq[Int]] = new Set[IndexedSeq[Int]]{
    def contains(elem: IndexedSeq[Int]): Boolean = elem.zipWithIndex.forall{ idx => {
        idx._2 >= 0 && idx._2 < shape(idx._1)
      }
    }

    def +(elem: IndexedSeq[Int]): Set[IndexedSeq[Int]] = throw new UnsupportedOperationException(" member '+' is not supported in TensorN.keySet() ")
    def -(elem: IndexedSeq[Int]): Set[IndexedSeq[Int]] = throw new UnsupportedOperationException(" member '-' is not supported in TensorN.keySet() ")
    def iterator: Iterator[IndexedSeq[Int]] = throw new UnsupportedOperationException(" member 'iterator' is not supported in TensorN.keySet() ")
  }

}
