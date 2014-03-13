package breeze.linalg.operators

import breeze.storage.DefaultArrayValue
import breeze.math.{Ring, Semiring, TemporaryFieldTranslation}
import breeze.linalg.support.{CanTransformValues, CanZipMapValues, CanAxpy, CanCopy}
import breeze.generic.UFunc
import breeze.linalg._
import spire.algebra.Field
import spire.implicits._

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
          a(k) = field.quot(v, b(k))
        }
      }
    }
  }

  implicit def canDivVV[K1, V](implicit copy: CanCopy[Counter[K1, V]],
                               field: Field[V],
                               d: DefaultArrayValue[V]):OpDiv.Impl2[Counter[K1, V], Counter[K1, V], Counter[K1, V]] = {
    new OpDiv.Impl2[Counter[K1, V], Counter[K1, V], Counter[K1, V]] {
      override def apply(a : Counter[K1, V], b : Counter[K1, V]) = {
        val r = Counter[K1, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = field.quot(v, b(k))
          r(k) = vr
        }
        r
      }
    }
  }


  implicit def canDivVS[K1, V](implicit copy: CanCopy[Counter[K1, V]],
                               field: Field[V],
                               d: DefaultArrayValue[V]):OpDiv.Impl2[Counter[K1, V], V, Counter[K1, V]] = {
    new OpDiv.Impl2[Counter[K1, V], V, Counter[K1, V]] {
      override def apply(a : Counter[K1, V], b : V) = {
        val r = Counter[K1, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = field.quot(v, b)
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
        a(k) = field.quot(v, b)
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
