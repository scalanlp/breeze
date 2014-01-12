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
import breeze.math.{Ring, Semiring, Field}
import breeze.linalg.support._
import scala.collection.Set
import breeze.generic._
import scala.reflect.ClassTag
import breeze.linalg.operators._
import CanTraverseValues.ValuesVisitor
import breeze.generic.UFunc.{InPlaceImpl, UImpl2, InPlaceImpl2}

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

  // the canmapvalues implicit in UFunc should take care of this, but limits of scala type inference, blah blah blah
  implicit def mapUFuncImpl[Tag, K1, K2, V,  U](implicit impl: UFunc.UImpl[Tag, V, U], canMapValues: CanMapValues[Counter2[K1, K2, V], V, U, Counter2[K1, K2, U]]): UFunc.UImpl[Tag, Counter2[K1, K2, V], Counter2[K1, K2, U]] = {
    new UFunc.UImpl[Tag, Counter2[K1, K2, V], Counter2[K1, K2, U]] {
      def apply(v: Counter2[K1, K2, V]): Counter2[K1, K2, U] = canMapValues.map(v, impl.apply)
    }
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

  implicit def handholdCanMapRows[K1, K2, V]: CanCollapseAxis.HandHold[Counter2[K1, K2, V], Axis._0.type, Counter[K1, V]] = new CanCollapseAxis.HandHold[Counter2[K1, K2, V], Axis._0.type, Counter[K1, V]]()

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
  implicit def handholdCanMapCols[K1, K2, V]: CanCollapseAxis.HandHold[Counter2[K1, K2, V], Axis._1.type, Counter[K2, V]] = new CanCollapseAxis.HandHold[Counter2[K1, K2, V], Axis._1.type, Counter[K2, V]]()

}


trait Counter2Ops {
  implicit def canCopy[K1, K2, V:DefaultArrayValue:Semiring]:CanCopy[Counter2[K1, K2, V]] = new CanCopy[Counter2[K1, K2, V]] {
    def apply(t: Counter2[K1, K2, V]): Counter2[K1, K2, V] = {
      Counter2(t.iterator.map{case ((k1,k2), v) => (k1, k2, v)})
    }
  }


  private def binaryOpFromBinaryUpdateOp[K1, K2, V, Other, Op<:OpType](implicit copy: CanCopy[Counter2[K1, K2, V]],
                                                                       op: InPlaceImpl2[Op, Counter2[K1, K2, V], Other]) = {
    new UImpl2[Op, Counter2[K1, K2, V], Other, Counter2[K1, K2, V]] {
      override def apply(a : Counter2[K1, K2, V], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }


  implicit def addIntoVV[K1, K2, V:Semiring]:OpAdd.InPlaceImpl2[Counter2[K1, K2, V], Counter2[K1, K2, V]] = {
    new OpAdd.InPlaceImpl2[Counter2[K1, K2, V], Counter2[K1, K2, V]] {
      val field = implicitly[Semiring[V]]
      def apply(a: Counter2[K1, K2, V], b: Counter2[K1, K2, V]) {
        for( (k,v) <- b.activeIterator) {
          a(k) = field.+(a(k), v)
        }
      }
    }
  }

  implicit def canAxpy[K1, K2, V:Semiring]:CanAxpy[V, Counter2[K1, K2, V], Counter2[K1, K2, V]] = new CanAxpy[V, Counter2[K1, K2, V], Counter2[K1, K2, V]] {
    val field = implicitly[Semiring[V]]
    def apply(s: V, b: Counter2[K1, K2, V], a: Counter2[K1, K2, V]) {
      for( (k,v) <- b.activeIterator) {
        a(k) = field.+(a(k), field.*(s, v))
      }
    }
  }

  implicit def addVV[K1, K2, V:Semiring:DefaultArrayValue]:OpAdd.Impl2[Counter2[K1, K2, V], Counter2[K1, K2, V], Counter2[K1, K2, V]] = {
    binaryOpFromBinaryUpdateOp(canCopy, addIntoVV)
  }

  implicit def addIntoVS[K1, K2, V:Semiring]:OpAdd.InPlaceImpl2[Counter2[K1, K2, V], V] = new OpAdd.InPlaceImpl2[Counter2[K1, K2, V], V] {
    val field = implicitly[Semiring[V]]
    def apply(a: Counter2[K1, K2, V], b: V) {
      for( (k,v) <- a.activeIterator) {
        a(k) = field.+(v, b)
      }
    }
  }


  implicit def addVS[K1, K2, V:Semiring:DefaultArrayValue]:OpAdd.Impl2[Counter2[K1, K2, V], V, Counter2[K1, K2, V]] = {
    binaryOpFromBinaryUpdateOp(canCopy, addIntoVS)
  }

  implicit def subIntoVV[K1, K2, V:Ring]:OpSub.InPlaceImpl2[Counter2[K1, K2, V], Counter2[K1, K2, V]] = new OpSub.InPlaceImpl2[Counter2[K1, K2, V], Counter2[K1, K2, V]] {
    val field = implicitly[Ring[V]]
    def apply(a: Counter2[K1, K2, V], b: Counter2[K1, K2, V]) {
      for( (k,v) <- b.activeIterator) {
        a(k) = field.-(a(k), v)
      }
    }
  }


  implicit def subVV[K1, K2, V:Ring:DefaultArrayValue]:OpSub.Impl2[Counter2[K1, K2, V], Counter2[K1, K2, V], Counter2[K1, K2, V]] = {
    binaryOpFromBinaryUpdateOp(canCopy, subIntoVV)
  }

  implicit def subIntoVS[K1, K2, V:Ring]:OpSub.InPlaceImpl2[Counter2[K1, K2, V], V] = new OpSub.InPlaceImpl2[Counter2[K1, K2, V], V] {
    val field = implicitly[Ring[V]]
    def apply(a: Counter2[K1, K2, V], b: V) {
      for( (k,v) <- a.activeIterator) {
        a(k) = field.-(v, b)
      }
    }
  }


  implicit def subVS[K1, K2, V:Ring:DefaultArrayValue]:OpSub.Impl2[Counter2[K1, K2, V], V, Counter2[K1, K2, V]] = {
    binaryOpFromBinaryUpdateOp(canCopy, subIntoVS)
  }

  implicit def canMulIntoVV[K1, K2, V:Semiring]:OpMulScalar.InPlaceImpl2[Counter2[K1, K2, V], Counter2[K1, K2, V]] = new OpMulScalar.InPlaceImpl2[Counter2[K1, K2, V], Counter2[K1, K2, V]] {
    val field = implicitly[Semiring[V]]
    def apply(a: Counter2[K1, K2, V], b: Counter2[K1, K2, V]) {
      for( (k,v) <- a.activeIterator) {
        a(k) = field.*(v, b(k))
      }
    }
  }

  implicit def canMulVV[K1, K2, V](implicit semiring: Semiring[V],
                                   d: DefaultArrayValue[V]):OpMulScalar.Impl2[Counter2[K1, K2, V], Counter2[K1, K2, V], Counter2[K1, K2, V]] = {
    new OpMulScalar.Impl2[Counter2[K1, K2, V], Counter2[K1, K2, V], Counter2[K1, K2, V]] {
      override def apply(a : Counter2[K1, K2, V], b : Counter2[K1, K2, V]) = {
        val r = Counter2[K1, K2, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = semiring.*(v, b(k))
          if(vr != semiring.zero)
            r(k) = vr
        }
        r
      }
    }
  }


  implicit def canMulIntoVS[K1, K2, V:Semiring]:OpMulScalar.InPlaceImpl2[Counter2[K1, K2, V], V] = new OpMulScalar.InPlaceImpl2[Counter2[K1, K2, V], V] {
    val field = implicitly[Semiring[V]]
    def apply(a: Counter2[K1, K2, V], b: V) {
      for( (k,v) <- a.activeIterator) {
        a(k) = field.*(v, b)
      }
    }
  }

  implicit def canMulIntoVS_M[K1, K2, V:Semiring]:OpMulMatrix.InPlaceImpl2[Counter2[K1, K2, V], V] = new OpMulMatrix.InPlaceImpl2[Counter2[K1, K2, V], V] {
    val field = implicitly[Semiring[V]]
    def apply(a: Counter2[K1, K2, V], b: V) {
      for( (k,v) <- a.activeIterator) {
        a(k) = field.*(v, b)
      }
    }
  }

  implicit def canMulVS[K1, K2, V](implicit semiring: Semiring[V],
                                   d: DefaultArrayValue[V]):OpMulScalar.Impl2[Counter2[K1, K2, V], V, Counter2[K1, K2, V]] = {
    new OpMulScalar.Impl2[Counter2[K1, K2, V], V, Counter2[K1, K2, V]] {
      override def apply(a : Counter2[K1, K2, V], b : V) = {
        val r = Counter2[K1, K2, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = semiring.*(v, b)
          r(k) = vr
        }
        r
      }
    }
  }

  implicit def canMulVS_M[K1, K2, V](implicit semiring: Semiring[V],
                                     d: DefaultArrayValue[V]):OpMulMatrix.Impl2[Counter2[K1, K2, V], V, Counter2[K1, K2, V]] = {
    new OpMulMatrix.Impl2[Counter2[K1, K2, V], V, Counter2[K1, K2, V]] {
      override def apply(a : Counter2[K1, K2, V], b : V) = {
        val r = Counter2[K1, K2, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = semiring.*(v, b)
          r(k) = vr
        }
        r
      }
    }
  }


  implicit def canDivIntoVV[K1, K2, V:Field]:OpDiv.InPlaceImpl2[Counter2[K1, K2, V], Counter2[K1, K2, V]] = {
    new OpDiv.InPlaceImpl2[Counter2[K1, K2, V], Counter2[K1, K2, V]] {
      val field = implicitly[Field[V]]
      def apply(a: Counter2[K1, K2, V], b: Counter2[K1, K2, V]) {
        for( (k,v) <- a.activeIterator) {
          a(k) = field./(v, b(k))
        }
      }
    }
  }

  implicit def canDivVV[K1, K2, V](implicit copy: CanCopy[Counter2[K1, K2, V]],
                                   semiring: Field[V],
                                   d: DefaultArrayValue[V]):OpDiv.Impl2[Counter2[K1, K2, V], Counter2[K1, K2, V], Counter2[K1, K2, V]] = {
    new OpDiv.Impl2[Counter2[K1, K2, V], Counter2[K1, K2, V], Counter2[K1, K2, V]] {
      override def apply(a : Counter2[K1, K2, V], b : Counter2[K1, K2, V]) = {
        val r = Counter2[K1, K2, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = semiring./(v, b(k))
          r(k) = vr
        }
        r
      }
    }
  }


  implicit def canDivVS[K1, K2, V](implicit copy: CanCopy[Counter2[K1, K2, V]],
                                   semiring: Field[V],
                                   d: DefaultArrayValue[V]):OpDiv.Impl2[Counter2[K1, K2, V], V, Counter2[K1, K2, V]] = {
    new OpDiv.Impl2[Counter2[K1, K2, V], V, Counter2[K1, K2, V]] {
      override def apply(a : Counter2[K1, K2, V], b : V) = {
        val r = Counter2[K1, K2, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = semiring./(v, b)
          r(k) = vr
        }
        r
      }
    }
  }

  implicit def canDivIntoVS[K1, K2, V:Field]:OpDiv.InPlaceImpl2[Counter2[K1, K2, V], V] = new OpDiv.InPlaceImpl2[Counter2[K1, K2, V], V] {
    val field = implicitly[Field[V]]
    def apply(a: Counter2[K1, K2, V], b: V) {
      for( (k,v) <- a.activeIterator) {
        a(k) = field./(v, b)
      }
    }
  }


  implicit def canSetIntoVV[K1, K2, V]: OpSet.InPlaceImpl2[Counter2[K1, K2, V], Counter2[K1, K2, V]] = new OpSet.InPlaceImpl2[Counter2[K1, K2, V], Counter2[K1, K2, V]] {
    def apply(a: Counter2[K1, K2, V], b: Counter2[K1, K2, V]) {
      a.data.clear()
      for( (k,v) <- b.activeIterator) {
        a(k) = v
      }
    }
  }


  implicit def canSetIntoVS[K1, K2, V]:OpSet.InPlaceImpl2[Counter2[K1, K2, V], V] = new OpSet.InPlaceImpl2[Counter2[K1, K2, V], V] {
    def apply(a: Counter2[K1, K2, V], b: V) {
      for( k <- a.keysIterator) {
        a(k) = b
      }
    }
  }

  implicit def canNegate[K1, K2, V](implicit  ring: Ring[V], d: DefaultArrayValue[V]):OpNeg.Impl[Counter2[K1, K2, V], Counter2[K1, K2, V]] = {
    new OpNeg.Impl[Counter2[K1, K2, V], Counter2[K1, K2, V]] {
      override def apply(a : Counter2[K1, K2, V]) = {
        val result = Counter2[K1, K2, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = ring.negate(v)
          result(k) = vr
        }
        result
      }
    }
  }


  /** Returns the k-norm of this Vector.
  implicit def canNorm[K1, K2, V:Ring]:CanNorm[Counter2[K1, K2, V]] = new CanNorm[Counter2[K1, K2, V]] {
    val field = implicitly[Ring[V]]
    def apply(c: Counter2[K1, K2, V], n: Double): Double = {
      import c._

      if (n == 1) {
        var sum = 0.0
        activeValuesIterator foreach (v => sum += field.norm(v))
        sum
      } else if (n == 2) {
        var sum = 0.0
        activeValuesIterator foreach (v => { val nn = field.norm(v); sum += nn * nn })
        math.sqrt(sum)
      } else if (n == Double.PositiveInfinity) {
        var max = Double.NegativeInfinity
        activeValuesIterator foreach (v => { val nn = field.norm(v); if (nn > max) max = nn })
        max
      } else {
        var sum = 0.0
        activeValuesIterator foreach (v => { val nn = field.norm(v); sum += math.pow(nn,n) })
        math.pow(sum, 1.0 / n)
      }
    }
  }*/


  implicit def canMultiplyC2C1[K1, K2, V](implicit semiring: Semiring[V], d: DefaultArrayValue[V]):OpMulMatrix.Impl2[Counter2[K1, K2, V], Counter[K2, V], Counter[K1, V]] = {
    new OpMulMatrix.Impl2[Counter2[K1, K2, V], Counter[K2, V], Counter[K1, V]] {
      override def apply(a : Counter2[K1, K2, V], b : Counter[K2, V]) = {
        val r = Counter[K1, V]()
        for( (row, ctr) <- a.data.iterator) {
          r(row) = ctr dot b
        }
        r
      }
    }
  }

  implicit def canMultiplyC2C2[K1, K2, K3, V](implicit semiring: Semiring[V], d: DefaultArrayValue[V]):OpMulMatrix.Impl2[Counter2[K1, K2, V], Counter2[K2, K3, V], Counter2[K1, K3, V]] = {
    new OpMulMatrix.Impl2[Counter2[K1, K2, V], Counter2[K2, K3, V], Counter2[K1, K3, V]] {
      override def apply(a : Counter2[K1, K2, V], b : Counter2[K2, K3, V]) = {
        val r = Counter2[K1, K3, V]()
        for( (row, ctr) <- a.data.iterator; (k2, v) <- ctr.activeIterator; (k3, v2) <- b(k2, ::).data) {
          r(row, k3) = semiring.+(r(row,k3), semiring.*(v, v2))
        }
        r
      }
    }
  }

  class CanZipMapValuesCounter2[K1, K2, V, RV:DefaultArrayValue:Semiring] extends CanZipMapValues[Counter2[K1, K2, V],V,RV,Counter2[K1, K2, RV]] {

    /**Maps all corresponding values from the two collection. */
    def map(from: Counter2[K1, K2, V], from2: Counter2[K1, K2, V], fn: (V, V) => RV) = {
      val result = Counter2[K1, K2, RV]
      for ( k <- (from.keySet ++ from2.keySet)) {
        result(k) = fn(from(k), from2(k))
      }
      result
    }
  }


  implicit def zipMap[K1, K2, V, R:DefaultArrayValue:Semiring] = new CanZipMapValuesCounter2[K1, K2, V, R]
}



