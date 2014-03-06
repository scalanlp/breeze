package breeze.linalg
/*
 Copyright 2012 David Hall

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
import breeze.linalg.Counter2.Curried
import breeze.storage.DefaultArrayValue
import collection.mutable.HashMap
import breeze.math.Semiring
import breeze.linalg.support._
import scala.collection.Set
import breeze.generic._
import scala.reflect.ClassTag
import breeze.linalg.operators._
import CanTraverseValues.ValuesVisitor

/**
 *
 * @author dlwh
 */
/**
 * A map-like tensor that acts like a collection of key-value pairs where
 * the set of values may grow arbitrarily.
 *
 * @author dlwh
 */
trait Counter2Like
[K1, K2, V,
+M1[VV] <: Curried[scala.collection.mutable.Map,K1]#Result[VV],
+T <: Counter[K2,V],
+This<:Counter2[K1,K2,V]] extends TensorLike[(K1,K2),V,This] { self =>

  def data : M1[_<:T]

  def default: V

  override def size = {
    var s = 0
    for (m <- data.valuesIterator) {
      s += m.size
    }
    s
  }


  def apply(i: (K1, K2)) = apply(i._1, i._2)

  def apply(k : K1, k2: K2) = data.get(k).map(t => t(k2)) getOrElse default

  def contains(k: K1) = data.contains(k)

  def contains(k1: K1, k2: K2) = data.contains(k1) && data(k1).contains(k2)


  def update(i: (K1, K2), v: V) {update(i._1, i._2, v)}

  def update(k1 : K1, k2: K2, v : V) =
    innerGetOrElseUpdate(k1,data)(k2) = v

  private[linalg] def innerGetOrElseUpdate[M](k:K1, m: scala.collection.mutable.Map[K1,M]): M = {
    m.getOrElseUpdate(k,m.default(k))
  }

  override def keysIterator = for ((k1,m) <- data.iterator; k2 <- m.keysIterator) yield (k1,k2)

  override def valuesIterator = for (m <- data.valuesIterator; v <- m.valuesIterator) yield v

  override def iterator = for ((k1,m) <- data.iterator; (k2,v) <- m.iterator) yield (k1,k2)->v

  def activeSize = size

  def activeIterator = iterator
  def activeKeysIterator = keysIterator
  def activeValuesIterator = valuesIterator

  def repr = this.asInstanceOf[This]

  override def toString: String = {
    data.iterator.map { case (k1, c) => k1 + " -> " + c.toString}.mkString("Counter2(",",\n",")")
  }

  override def equals(p1: Any): Boolean = p1 match {
    case x:Counter2[_, _, _] =>
      x.activeIterator.toSet == activeIterator.toSet
    case _ => false
  }


}


trait Counter2
[K1, K2, V]
  extends Tensor[(K1,K2),V] with Counter2Like[K1,K2,V,Curried[scala.collection.mutable.Map,K1]#Result,Counter[K2,V],Counter2[K1,K2,V]]

object Counter2 extends LowPriorityCounter2 with Counter2Ops {

  @SerialVersionUID(1L)
  class Impl[K1, K2, V]
  (override val data : scala.collection.mutable.Map[K1,Counter[K2,V]])
  (implicit scalar : DefaultArrayValue[V])
    extends Counter2[K1,K2,V] with Serializable {
    def default = scalar.value

    def keySet: Set[(K1, K2)] = new Set[(K1, K2)] {
      def contains(k: (K1, K2)): Boolean = data.contains(k._1) && data(k._1).contains(k._2)

      def +(elem: (K1, K2)): Set[(K1, K2)] = Set.empty ++ iterator + elem
      def -(elem: (K1, K2)): Set[(K1, K2)] = Set.empty ++ iterator - elem

      def iterator: Iterator[(K1, K2)] = for( (k1,m) <- data.iterator; k2 <- m.keysIterator) yield (k1, k2)
    }
  }

  /** Returns a new empty counter. */
  def apply[K1,K2,V:DefaultArrayValue:Semiring]() : Counter2[K1,K2,V] = {
    val map = new HashMap[K1,Counter[K2,V]] {
      override def default(k: K1) = Counter[K2,V]()
    }
    new Impl[K1,K2,V](map)
  }


  /** Aggregates the counts in the given items. */
  def apply[K1,K2,V:Semiring:DefaultArrayValue](values : (K1,K2,V)*) : Counter2[K1,K2,V] =
    apply(values)

  /** Aggregates the counts in the given items. */
  def apply[K1,K2,V:Semiring:DefaultArrayValue](values : TraversableOnce[(K1,K2,V)]) : Counter2[K1,K2,V] = {
    val rv = apply[K1,K2,V]()
    values.foreach({ case (k1,k2,v) => rv(k1,k2) = implicitly[Semiring[V]].+(rv(k1,k2), v) })
    rv
  }

  /** Counts the given elements. */
  def count[K1,K2](values : TraversableOnce[(K1,K2)]) : Counter2[K1,K2,Int] = {
    val rv = apply[K1,K2,Int]()
    values.foreach({ case (k1,k2) => rv(k1,k2) += 1; })
    rv
  }


  implicit def CanMapValuesCounter[K1, K2, V, RV:Semiring:DefaultArrayValue]: CanMapValues[Counter2[K1, K2, V], V, RV, Counter2[K1, K2, RV]]
  = new CanMapValues[Counter2[K1, K2, V],V,RV,Counter2[K1, K2, RV]] {
    override def map(from : Counter2[K1, K2, V], fn : (V=>RV)) = {
      val rv = Counter2[K1, K2, RV]()
      for( (k,v) <- from.iterator) {
        rv(k) = fn(v)
      }
      rv
    }

    override def mapActive(from : Counter2[K1, K2, V], fn : (V=>RV)) = {
      val rv = Counter2[K1,K2, RV]()
      for( (k,v) <- from.activeIterator) {
        rv(k) = fn(v)
      }
      rv
    }
  }


  implicit def canIterateValues[K1, K2, V]: CanTraverseValues[Counter2[K1, K2,V], V] = new CanTraverseValues[Counter2[K1, K2, V], V] {


    def isTraversableAgain(from: Counter2[K1, K2, V]): Boolean = true

    /** Iterates all key-value pairs from the given collection. */
    def traverse(from: Counter2[K1, K2, V], fn: ValuesVisitor[V]): Unit = {
      for( v <- from.valuesIterator) {
        fn.visit(v)
      }
    }

  }

  // slicing


  implicit def canSliceRow[K1,K2,V] : CanSlice2[Counter2[K1,K2,V],K1,::.type, Counter[K2,V]]
  = new CanSlice2[Counter2[K1,K2,V],K1, ::.type, Counter[K2,V]] {
    override def apply(from : Counter2[K1,K2,V], row : K1, unused: ::.type) = from.innerGetOrElseUpdate(row, from.data)
  }

  implicit def canSliceCol[K1,K2,V]: CanSlice2[Counter2[K1,K2,V], ::.type, K2,Counter[K1,V]]
  = new CanSlice2[Counter2[K1,K2,V],::.type, K2,Counter[K1,V]] {
    def apply(from: Counter2[K1, K2, V], x: ::.type, col: K2) = new Counter[K1,V] {
      def default = from.default

      override val data = new scala.collection.mutable.Map[K1,V] {
        override def apply(k1 : K1) =
          from(k1,col)

        override def update(k1 : K1, v : V) =
          from(k1,col) = v

        override def -=(k1 : K1) = {
          from.data(k1)(col) = from.default
          this
        }

        override def +=(tup : (K1,V)) = {
          from.data(tup._1)(col) = (tup._2)
          this
        }

        override def iterator =
          for ((k1,map) <- from.data.iterator; v <- map.get(col)) yield (k1,v)

        override def get(k1 : K1) =
          from.data.get(k1).map(_(col))

        override def keySet = from.data.keySet

        override def size = from.data.size
      }
    }
  }


  /**
   * Returns a Counter[K2, V]
   * @tparam V
   * @return
   */
  implicit def canMapRows[K1, K2, V:ClassTag:DefaultArrayValue:Semiring]: CanCollapseAxis[Counter2[K1, K2,V], Axis._0.type, Counter[K1, V], Counter[K1, V], Counter2[K1, K2, V]]  = new CanCollapseAxis[Counter2[K1, K2,V], Axis._0.type, Counter[K1, V], Counter[K1, V], Counter2[K1,K2,V]] {
    def apply(from: Counter2[K1, K2,V], axis: Axis._0.type)(f: (Counter[K1, V]) => Counter[K1, V]): Counter2[K1, K2, V] = {
      val result = Counter2[K1, K2, V]()
      for( dom <- from.keySet.map(_._2)) {
        result(::, dom) := f(from(::, dom))
      }
      result
    }
  }

  implicit def handholdCanMapRows[K1, K2, V]: CanCollapseAxis.HandHold[Counter2[K1, K2, V], Axis._0.type, Counter[K1, V]] = new CanCollapseAxis.HandHold[Counter2[K1, K2, V], Axis._0.type, Counter[K1, V]]()

  /**
   * Returns a Counter[K1, V]
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def canMapCols[K1, K2, V:ClassTag:DefaultArrayValue:Semiring]: CanCollapseAxis[Counter2[K1, K2,V], Axis._1.type, Counter[K2, V], Counter[K2, V], Counter2[K1, K2, V]]  = new CanCollapseAxis[Counter2[K1, K2,V], Axis._1.type, Counter[K2, V], Counter[K2, V], Counter2[K1,K2,V]] {
    def apply(from: Counter2[K1, K2,V], axis: Axis._1.type)(f: (Counter[K2, V]) => Counter[K2, V]): Counter2[K1, K2, V] = {
      val result = Counter2[K1, K2, V]()
      for( (dom,c) <- from.data) {
        result(dom, ::) := f(c)
      }
      result
    }
  }
  implicit def handholdCanMapCols[K1, K2, V]: CanCollapseAxis.HandHold[Counter2[K1, K2, V], Axis._1.type, Counter[K2, V]] = new CanCollapseAxis.HandHold[Counter2[K1, K2, V], Axis._1.type, Counter[K2, V]]()





  /**
   * This is just a curried version of scala.collection.Map.
   * Used to get around Scala's lack of partially applied types.
   *
   * @author dlwh
   */
  trait Curried[M[_,_],K] {
    type Result[V] = M[K,V]
  }
}


trait LowPriorityCounter2 {

  /**
   * Returns a Counter[K2, V]
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def canCollapseRows[K1, K2, V, R:ClassTag:DefaultArrayValue:Semiring]: CanCollapseAxis[Counter2[K1, K2, V], Axis._0.type, Counter[K1, V], R, Counter[K2, R]]  = new CanCollapseAxis[Counter2[K1, K2,V], Axis._0.type, Counter[K1, V], R, Counter[K2,R]] {
    def apply(from: Counter2[K1, K2,V], axis: Axis._0.type)(f: (Counter[K1, V]) => R): Counter[K2, R] = {
      val result = Counter[K2, R]()
      for( dom <- from.keySet.map(_._2)) {
        result(dom) = f(from(::, dom))
      }
      result
    }
  }


  /**
   * Returns a Counter[K1, V]
   * @tparam V
   * @tparam R
   * @return
   */
  implicit def canCollapseCols[K1, K2, V, R:ClassTag:DefaultArrayValue:Semiring]: CanCollapseAxis[Counter2[K1, K2, V], Axis._1.type, Counter[K2, V], R, Counter[K1, R]]  = new CanCollapseAxis[Counter2[K1, K2,V], Axis._1.type, Counter[K2, V], R, Counter[K1,R]] {
    def apply(from: Counter2[K1, K2,V], axis: Axis._1.type)(f: (Counter[K2, V]) => R): Counter[K1, R] = {
      val result = Counter[K1, R]()
      for( (dom,c) <- from.data) {
        result(dom) = f(c)
      }
      result
    }
  }



}



