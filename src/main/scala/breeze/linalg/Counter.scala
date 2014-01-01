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

import breeze.storage.DefaultArrayValue
import breeze.math.{TensorSpace, Ring, Semiring, Field}
import breeze.generic._
import collection.Set
import operators._
import support.{CanCreateZerosLike, CanZipMapValues, CanCopy}
import breeze.generic.CanTraverseValues.ValuesVisitor

/**
 * A map-like tensor that acts like a collection of key-value pairs where
 * the set of values may grow arbitrarily.
 *
 * @author dramage, dlwh
 */
@SerialVersionUID(1)
trait CounterLike[K, V, +M<:scala.collection.mutable.Map[K,V], +This<:Counter[K,V]] extends TensorLike[K,V,This] with Serializable {
  def data : M
  def default: V


  def keySet: Set[K] = data.keySet

  def repr = this.asInstanceOf[This]

  override def size = data.size
  def activeSize = data.size

  def isEmpty = data.isEmpty

  def contains(k: K) = data.contains(k)

  override def apply(k : K) = {
    data.get(k) getOrElse default
  }

  def update(k : K, v : V) { data(k) = v }

  def get(k: K) = data.get(k)


  override def keysIterator = data.keysIterator

  override def valuesIterator = data.valuesIterator

  override def iterator = data.iterator


  def activeIterator = iterator

  def activeValuesIterator = valuesIterator

  def activeKeysIterator = keysIterator

  override def toString: String = data.mkString("Counter(",", ", ")")

  override def equals(p1: Any): Boolean = p1 match {
    case x:Counter[K, V] => x.data == this.data
    case _ => false
  }

  override def hashCode(): Int = data.hashCode()

  def toMap = data.toMap
}

trait Counter[K, V] extends Tensor[K,V] with CounterLike[K, V, collection.mutable.Map[K, V], Counter[K,V]] {

}

object Counter extends CounterOps {
  /** Returns an empty counter. */
  def apply[K,V:DefaultArrayValue:Semiring]() : Counter[K,V] =
    new Impl(scala.collection.mutable.HashMap[K,V]())

  /** Returns a counter by summing all the given values. */
  def apply[K,V:DefaultArrayValue:Semiring](values : (K,V)*) : Counter[K,V] =
    apply(values)

  /** Returns a counter by summing all the given values. */
  def apply[K,V:DefaultArrayValue:Semiring](values : TraversableOnce[(K,V)]) : Counter[K,V] = {
    val rv = apply[K,V]()
    val field = implicitly[Semiring[V]]
    values.foreach({ case (k,v) => rv(k) = field.+(v,rv(k)) })
    rv
  }

  /** Counts each of the given items. */
  def countTraversable[K](items : TraversableOnce[K]) : Counter[K,Int] = {
    val rv = apply[K,Int]()
    items.foreach(rv(_) += 1)
    rv
  }

  def count[K](items: K*): Counter[K,Int] = countTraversable(items)

  @SerialVersionUID(2872445575657408160L)
  class Impl[K, V]
  (override val data : scala.collection.mutable.Map[K,V])
  (implicit defaultArrayValue : DefaultArrayValue[V])
  extends Counter[K,V] {
    def default = defaultArrayValue.value
  }

  implicit def canMapValues[K, V, RV:Semiring:DefaultArrayValue]: CanMapValues[Counter[K, V], V, RV, Counter[K, RV]]
  = new CanMapValues[Counter[K,V],V,RV,Counter[K,RV]] {
    override def map(from : Counter[K,V], fn : (V=>RV)) = {
      val rv = Counter[K,RV]()
      for( (k,v) <- from.iterator) {
        rv(k) = fn(from.data(k))
      }
      rv
    }

    override def mapActive(from : Counter[K,V], fn : (V=>RV)) = {
      val rv = Counter[K,RV]()
      for( (k,v) <- from.activeIterator) {
        rv(k) = fn(from.data(k))
      }
      rv
    }
  }

  implicit def canIterateValues[K, V]: CanTraverseValues[Counter[K, V], V] = new CanTraverseValues[Counter[K,V], V] {
    /** Iterates all values from the given collection. */
    def traverse(from: Counter[K, V], fn: ValuesVisitor[V]): Unit = {
      for( v <- from.valuesIterator) {
        fn.visit(v)
      }
    }

  }

  implicit def tensorspace[K, V](implicit field: Field[V], dfv: DefaultArrayValue[V], normImpl: norm.Impl[V, Double]) = {
    implicit def zipMap = Counter.zipMap[K, V, V]
    // sigh...
    TensorSpace.make[Counter[K, V], K, V](canNorm,
      normImpl,
      canMapValues[K, V, V],
      canIterateValues[K, V],
      zipMap,
      addVS,
      subVS,
      canMulVV,
      canDivVV,
      canCopy,
      canMulIntoVS,
      canDivIntoVS,
      addIntoVV[K, V],
      subIntoVV[K, V],
      addIntoVS[K, V],
      subIntoVS[K, V],
      canMulIntoVV,
      canDivIntoVV,
      canSetIntoVV,
      canSetIntoVS,
      canAxpy,
      implicitly[Field[V]],
      implicitly[CanCreateZerosLike[Counter[K, V], Counter[K, V]]],
      canMulVS,
      canDivVS,
      addVV,
      subVV,
      canNegate,
      implicitly[<:<[breeze.linalg.Counter[K,V],breeze.linalg.NumericOps[breeze.linalg.Counter[K,V]] with breeze.linalg.QuasiTensor[K,V]]],
      canMulInner[K, V]
    )
  }
}

trait CounterOps {
  implicit def canCopy[K1, V:DefaultArrayValue:Semiring]:CanCopy[Counter[K1, V]] = new CanCopy[Counter[K1, V]] {
    def apply(t: Counter[K1, V]): Counter[K1, V] = {
      Counter(t.iterator)
    }
  }


  def binaryOpFromBinaryUpdateOp[K, V, Other, Op<:OpType](implicit copy: CanCopy[Counter[K, V]],
                                                          op: UFunc.InPlaceImpl2[Op, Counter[K, V], Other]): UFunc.UImpl2[Op, Counter[K, V], Other, Counter[K, V]] = {
    new UFunc.UImpl2[Op, Counter[K, V], Other, Counter[K, V]] {
      override def apply(a : Counter[K, V], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }


  implicit def addIntoVV[K1, V:Semiring]:OpAdd.InPlaceImpl2[Counter[K1, V], Counter[K1, V]] =  new OpAdd.InPlaceImpl2[Counter[K1, V], Counter[K1, V]] {
    val field = implicitly[Semiring[V]]
    def apply(a: Counter[K1, V], b: Counter[K1, V]) {
      for( (k,v) <- b.activeIterator) {
        a(k) = field.+(a(k), v)
      }
    }
  }

  implicit def canAxpy[K1, V:Semiring]:CanAxpy[V, Counter[K1, V], Counter[K1, V]] = new CanAxpy[V, Counter[K1, V], Counter[K1, V]] {
    val field = implicitly[Semiring[V]]
    def apply(s: V, b: Counter[K1, V], a: Counter[K1, V]) {
      for( (k,v) <- b.activeIterator) {
        a(k) = field.+(a(k), field.*(s, v))
      }
    }
  }

  implicit def addVV[K1, V:Semiring:DefaultArrayValue]:OpAdd.Impl2[Counter[K1, V], Counter[K1, V], Counter[K1,V]] = {
    binaryOpFromBinaryUpdateOp(canCopy, addIntoVV)
  }

  implicit def addIntoVS[K1, V:Semiring]:OpAdd.InPlaceImpl2[Counter[K1, V], V] = new OpAdd.InPlaceImpl2[Counter[K1, V], V] {
    val field = implicitly[Semiring[V]]
    def apply(a: Counter[K1, V], b: V) {
      for( (k,v) <- a.activeIterator) {
        a(k) = field.+(v, b)
      }
    }
  }


  implicit def addVS[K1, V:Semiring:DefaultArrayValue]:OpAdd.Impl2[Counter[K1, V], V, Counter[K1,V]] = {
    binaryOpFromBinaryUpdateOp(canCopy, addIntoVS)
  }

  implicit def subIntoVV[K1, V:Ring]:OpSub.InPlaceImpl2[Counter[K1, V], Counter[K1, V]] = {
    new OpSub.InPlaceImpl2[Counter[K1, V], Counter[K1, V]] {
      val field = implicitly[Ring[V]]
      def apply(a: Counter[K1, V], b: Counter[K1, V]) {
        for( (k,v) <- b.activeIterator) {
          a(k) = field.-(a(k), v)
        }
      }
    }
  }


  implicit def subVV[K1, V:Ring:DefaultArrayValue]:OpSub.Impl2[Counter[K1, V], Counter[K1, V], Counter[K1,V]] = {
    binaryOpFromBinaryUpdateOp(canCopy, subIntoVV)
  }

  implicit def subIntoVS[K1, V:Ring]:OpSub.InPlaceImpl2[Counter[K1, V], V] = new OpSub.InPlaceImpl2[Counter[K1, V], V] {
    val field = implicitly[Ring[V]]
    def apply(a: Counter[K1, V], b: V) {
      for( (k,v) <- a.activeIterator) {
        a(k) = field.-(v, b)
      }
    }
  }


  implicit def subVS[K1, V:Ring:DefaultArrayValue]:OpSub.Impl2[Counter[K1, V], V, Counter[K1,V]] = {
    binaryOpFromBinaryUpdateOp(canCopy, subIntoVS)
  }

  implicit def canMulIntoVV[K2, K1 <: K2, V:Semiring]:OpMulScalar.InPlaceImpl2[Counter[K1, V], Counter[K2, V]] = new OpMulScalar.InPlaceImpl2[Counter[K1, V], Counter[K2, V]] {
    val field = implicitly[Semiring[V]]
    def apply(a: Counter[K1, V], b: Counter[K2, V]) {
      for( (k,v) <- a.activeIterator) {
        a(k) = field.*(v, b(k))
      }
    }
  }

  implicit def canMulVV[K1, V](implicit semiring: Semiring[V],
                               d: DefaultArrayValue[V]): OpMulScalar.Impl2[Counter[K1, V], Counter[K1, V], Counter[K1, V]] = {
    new OpMulScalar.Impl2[Counter[K1, V], Counter[K1, V], Counter[K1, V]] {
      override def apply(a : Counter[K1, V], b : Counter[K1, V]) = {
        val r = Counter[K1, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = semiring.*(v, b(k))
          if(vr != semiring.zero)
            r(k) = vr
        }
        r
      }
    }
  }


  implicit def canMulIntoVS[K2, K1 <: K2, V:Semiring]:OpMulScalar.InPlaceImpl2[Counter[K1, V], V] = new OpMulScalar.InPlaceImpl2[Counter[K1, V], V] {
    val field = implicitly[Semiring[V]]
    def apply(a: Counter[K1, V], b: V) {
      for( (k,v) <- a.activeIterator) {
        a(k) = field.*(v, b)
      }
    }
  }

  implicit def canMulIntoVS_M[K2, K1 <: K2, V:Semiring]:OpMulMatrix.InPlaceImpl2[Counter[K1, V], V] = new OpMulMatrix.InPlaceImpl2[Counter[K1, V], V] {
    val field = implicitly[Semiring[V]]
    def apply(a: Counter[K1, V], b: V) {
      for( (k,v) <- a.activeIterator) {
        a(k) = field.*(v, b)
      }
    }
  }

  implicit def canMulVS[K2, K1<:K2, V](implicit semiring: Semiring[V],
                                       d: DefaultArrayValue[V]):OpMulScalar.Impl2[Counter[K1, V], V, Counter[K1, V]] = {
    new OpMulScalar.Impl2[Counter[K1, V], V, Counter[K1, V]] {
      override def apply(a : Counter[K1, V], b : V) = {
        val r = Counter[K1, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = semiring.*(v, b)
            r(k) = vr
        }
        r
      }
    }
  }

  implicit def canMulVS_M[K2, K1<:K2, V](implicit semiring: Semiring[V],
                                         d: DefaultArrayValue[V]):OpMulMatrix.Impl2[Counter[K1, V], V, Counter[K1, V]] = {
    new OpMulMatrix.Impl2[Counter[K1, V], V, Counter[K1, V]] {
      override def apply(a : Counter[K1, V], b : V) = {
        val r = Counter[K1, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = semiring.*(v, b)
            r(k) = vr
        }
        r
      }
    }
  }


  implicit def canDivIntoVV[K1, V:Field]:OpDiv.InPlaceImpl2[Counter[K1, V], Counter[K1, V]] = {
    new OpDiv.InPlaceImpl2[Counter[K1, V], Counter[K1, V]] {
      val field = implicitly[Field[V]]
      def apply(a: Counter[K1, V], b: Counter[K1, V]) {
        for( (k,v) <- a.activeIterator) {
          a(k) = field./(v, b(k))
        }
      }
    }
  }

  implicit def canDivVV[K1, V](implicit copy: CanCopy[Counter[K1, V]],
                                       semiring: Field[V],
                                       d: DefaultArrayValue[V]):OpDiv.Impl2[Counter[K1, V], Counter[K1, V], Counter[K1, V]] = {
    new OpDiv.Impl2[Counter[K1, V], Counter[K1, V], Counter[K1, V]] {
      override def apply(a : Counter[K1, V], b : Counter[K1, V]) = {
        val r = Counter[K1, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = semiring./(v, b(k))
            r(k) = vr
        }
        r
      }
    }
  }


  implicit def canDivVS[K1, V](implicit copy: CanCopy[Counter[K1, V]],
                               semiring: Field[V],
                               d: DefaultArrayValue[V]):OpDiv.Impl2[Counter[K1, V], V, Counter[K1, V]] = {
    new OpDiv.Impl2[Counter[K1, V], V, Counter[K1, V]] {
      override def apply(a : Counter[K1, V], b : V) = {
        val r = Counter[K1, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = semiring./(v, b)
            r(k) = vr
        }
        r
      }
    }
  }

  implicit def canDivIntoVS[K1, V:Field]:OpDiv.InPlaceImpl2[Counter[K1, V], V] = new OpDiv.InPlaceImpl2[Counter[K1, V], V] {
    val field = implicitly[Field[V]]
    def apply(a: Counter[K1, V], b: V) {
      for( (k,v) <- a.activeIterator) {
        a(k) = field./(v, b)
      }
    }
  }


  implicit def canSetIntoVV[K1, K2 <: K1, V]:OpSet.InPlaceImpl2[Counter[K1, V], Counter[K2, V]] = new OpSet.InPlaceImpl2[Counter[K1, V], Counter[K2, V]] {
    def apply(a: Counter[K1, V], b: Counter[K2, V]) {
      a.data.clear()
      for( (k,v) <- b.activeIterator) {
        a(k) = v
      }
    }
  }


  implicit def canSetIntoVS[K1, V]:OpSet.InPlaceImpl2[Counter[K1, V], V] = {
    new OpSet.InPlaceImpl2[Counter[K1, V], V] {
      def apply(a: Counter[K1, V], b: V) {
        for( k <- a.keysIterator) {
          a(k) = b
        }
      }
    }
  }

  implicit def canNegate[K1, V](implicit ring: Ring[V], d: DefaultArrayValue[V]):OpNeg.Impl[Counter[K1, V], Counter[K1, V]] = {
    new OpNeg.Impl[Counter[K1, V], Counter[K1, V]] {
      override def apply(a : Counter[K1, V]) = {
        val result = Counter[K1, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = ring.negate(v)
          result(k) = vr
        }
        result
      }
    }
  }


  implicit def canMulInner[K1, V](implicit copy: CanCopy[Counter[K1, V]],
                                  semiring: Semiring[V],
                                  d: DefaultArrayValue[V]):OpMulInner.Impl2[Counter[K1, V], Counter[K1, V], V] = {
    new OpMulInner.Impl2[Counter[K1, V], Counter[K1, V], V] {
      val zero = semiring.zero
      override def apply(a : Counter[K1, V], b : Counter[K1, V]) = {
        var result = zero
        for( (k, v) <- a.activeIterator) {
          val vr = semiring.*(v, b(k))
            result  = semiring.+(result, vr)
        }
        result
      }
    }
  }
  /** Returns the k-norm of this Vector. */
  implicit def canNorm[K, V](implicit normImpl: norm.Impl[V, Double]):norm.Impl2[Counter[K, V], Double, Double] = new norm.Impl2[Counter[K, V], Double, Double] {
    def apply(c: Counter[K, V], n: Double): Double = {
      import c.{norm => _, _}

      if (n == 1) {
        var sum = 0.0
        activeValuesIterator foreach (v => sum += norm(v))
        sum
      } else if (n == 2) {
        var sum = 0.0
        activeValuesIterator foreach (v => { val nn = norm(v); sum += nn * nn })
        math.sqrt(sum)
      } else if (n == Double.PositiveInfinity) {
        var max = 0.0
        activeValuesIterator foreach (v => { val nn = norm(v); if (nn > max) max = nn })
        max
      } else {
        var sum = 0.0
        activeValuesIterator foreach (v => { val nn = norm(v); sum += math.pow(nn,n) })
        math.pow(sum, 1.0 / n)
      }
    }
  }

  class CanZipMapValuesCounter[K, V, RV:DefaultArrayValue:Semiring] extends CanZipMapValues[Counter[K, V],V,RV,Counter[K, RV]] {

    /**Maps all corresponding values from the two collection. */
    def map(from: Counter[K, V], from2: Counter[K, V], fn: (V, V) => RV) = {
      val result = Counter[K, RV]
      for ( k <- (from.keySet ++ from2.keySet)) {
        result(k) = fn(from(k), from2(k))
      }
      result
    }
  }


  implicit def zipMap[K, V, R:DefaultArrayValue:Semiring] = new CanZipMapValuesCounter[K, V, R]


  implicit def canTransformValues[L, V]:CanTransformValues[Counter[L, V], V, V] = {
    new CanTransformValues[Counter[L, V], V, V] {
      def transform(from: Counter[L, V], fn: (V) => V) {
        for( (k,v) <- from.activeIterator) {
          from(k) = fn(v)
        }
      }

      def transformActive(from: Counter[L, V], fn: (V) => V) {
        transform(from, fn)
      }
    }
  }
}
