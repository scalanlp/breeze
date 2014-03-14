package breeze.linalg
package operators

import breeze.storage.DefaultArrayValue
import breeze.math.{Ring, Semiring, TemporaryFieldTranslation}
import breeze.linalg.support.{CanZipMapValues, CanAxpy, CanCopy}
import breeze.generic.UFunc.{UImpl2, InPlaceImpl2}
import spire.algebra.Field



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
          a(k) = field.quot(v, b(k))
        }
      }
    }
  }

  implicit def canDivVV[K1, K2, V](implicit copy: CanCopy[Counter2[K1, K2, V]],
                                   field: Field[V],
                                   d: DefaultArrayValue[V]):OpDiv.Impl2[Counter2[K1, K2, V], Counter2[K1, K2, V], Counter2[K1, K2, V]] = {
    new OpDiv.Impl2[Counter2[K1, K2, V], Counter2[K1, K2, V], Counter2[K1, K2, V]] {
      private implicit val ring = new TemporaryFieldTranslation(field)
      override def apply(a : Counter2[K1, K2, V], b : Counter2[K1, K2, V]) = {
        val r = Counter2[K1, K2, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = field.quot(v, b(k))
          r(k) = vr
        }
        r
      }
    }
  }


  implicit def canDivVS[K1, K2, V](implicit copy: CanCopy[Counter2[K1, K2, V]],
                                   field: Field[V],
                                   d: DefaultArrayValue[V]):OpDiv.Impl2[Counter2[K1, K2, V], V, Counter2[K1, K2, V]] = {
    new OpDiv.Impl2[Counter2[K1, K2, V], V, Counter2[K1, K2, V]] {
      private implicit val ring = new TemporaryFieldTranslation(field)
      override def apply(a : Counter2[K1, K2, V], b : V) = {
        val r = Counter2[K1, K2, V]()
        for( (k, v) <- a.activeIterator) {
          val vr = field.quot(v, b)
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
        a(k) = field.quot(v, b)
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
