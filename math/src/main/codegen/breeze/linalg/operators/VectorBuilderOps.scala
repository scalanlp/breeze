package breeze.linalg.operators

import breeze.generic.UFunc.{InPlaceImpl2, UImpl2}
import breeze.linalg.{DenseMatrix, DenseVector, HashVector, StorageVector, Vector, VectorBuilder, axpy, norm, scaleAdd}
import breeze.macros._
import breeze.math.{Field, MutableModule, MutableVectorField, Ring, Semiring}
import breeze.storage.Zero
import breeze.math.PowImplicits._

import scala.reflect.ClassTag

trait VectorBuilderOps { self: VectorBuilder.type =>
  @expand
  @expand.valify
  implicit def canOpInto_V_S[@expand.args(OpMulScalar, OpDiv) Op, @expand.args(Double, Long, Float, Int) T](
                                                                                                             implicit @expand.sequence[Op]((_ * _), (_ / _)) op: Q): Op.InPlaceImpl2[VectorBuilder[T], T] = {
    new Op.InPlaceImpl2[VectorBuilder[T], T] {
      def apply(a: VectorBuilder[T], b: T): Unit = {
        var i = 0
        while (i < a.activeSize) {
          a.data(i) = op(a.data(i), b)
          i += 1
        }
      }
    }

  }

  implicit def canMulInto_V_S[T: Semiring : ClassTag]: OpMulScalar.InPlaceImpl2[VectorBuilder[T], T] = {
    new OpMulScalar.InPlaceImpl2[VectorBuilder[T], T] {
      val sr = implicitly[Semiring[T]]

      def apply(a: VectorBuilder[T], b: T): Unit = {
        var i = 0
        while (i < a.activeSize) {
          a.data(i) = sr.*(a.data(i), b)
          i += 1
        }
      }
    }
  }

  implicit def canDivInto_V_S[T: Field : ClassTag]: OpDiv.InPlaceImpl2[VectorBuilder[T], T] = {
    new OpDiv.InPlaceImpl2[VectorBuilder[T], T] {
      val f = implicitly[Field[T]]

      def apply(a: VectorBuilder[T], b: T): Unit = {
        var i = 0
        while (i < a.activeSize) {
          a.data(i) = f./(a.data(i), b)
          i += 1
        }
      }
    }
  }

  @expand
  @expand.valify
  implicit def canOpInto_V_V[@expand.args(OpAdd, OpSub) Op, @expand.args(Double, Long, Float, Int) T](
                                                                                                       implicit @expand.sequence[Op]((x => x), (-_)) op: Q): Op.InPlaceImpl2[VectorBuilder[T], VectorBuilder[T]] = {
    new Op.InPlaceImpl2[VectorBuilder[T], VectorBuilder[T]] {
      def apply(a: VectorBuilder[T], b: VectorBuilder[T]): Unit = {
        require(a.length < 0 || b.length < 0 || a.length == b.length, "Dimension mismatch!")
        a.reserve(a.activeSize + b.activeSize)
        var i = 0
        // read once here in case we're doing a += a
        val bActiveSize = b.activeSize
        while (i < bActiveSize) {
          a.add(b.index(i), op(b.data(i)))
          i += 1
        }
      }
    }
  }

  @expand
  implicit def canOpInto_V_V[@expand.args(OpAdd, OpSub) Op, T: Ring : ClassTag](implicit @expand.sequence[Op]((x => x), {
    r.negate(_)
  }) op: Q): Op.InPlaceImpl2[VectorBuilder[T], VectorBuilder[T]] = {
    new Op.InPlaceImpl2[VectorBuilder[T], VectorBuilder[T]] {
      val r = implicitly[Ring[T]]

      def apply(a: VectorBuilder[T], b: VectorBuilder[T]): Unit = {
        require(a.length < 0 || b.length < 0 || a.length == b.length, "Dimension mismatch!")
        a.reserve(a.activeSize + b.activeSize)
        var i = 0
        // read once here in case we're doing a += a
        val bActiveSize = b.activeSize
        while (i < bActiveSize) {
          a.add(b.index(i), op(b.data(i)))
          i += 1
        }
      }
    }
  }

  @expand
  implicit def canOpInto_V_S[@expand.args(OpAdd, OpSub) Op, T: Ring : ClassTag](implicit @expand.sequence[Op]((x => x), {
    r.negate(_)
  }) op: Q): Op.InPlaceImpl2[VectorBuilder[T], T] = {
    new Op.InPlaceImpl2[VectorBuilder[T], T] {
      val r = implicitly[Ring[T]]

      def apply(a: VectorBuilder[T], b: T): Unit = {
        var i = 0
        // read once here in case we're doing a += a
        while (i < a.activeSize) {
          a.add(i, op(b))
          i += 1
        }
      }
    }
  }

  @expand
  @expand.valify
  implicit def canSet[@expand.args(Double, Long, Float, Int) T]
  : OpSet.InPlaceImpl2[VectorBuilder[T], VectorBuilder[T]] = {
    new OpSet.InPlaceImpl2[VectorBuilder[T], VectorBuilder[T]] {
      def apply(a: VectorBuilder[T], b: VectorBuilder[T]): Unit = {
        if (a eq b) return
        a.clear()
        a.reserve(b.activeSize)
        var i = 0
        while (i < b.activeSize) {
          a.add(b.index(i), b.data(i))
          i += 1
        }
      }
    }
  }

  implicit def canSet[T]: OpSet.InPlaceImpl2[VectorBuilder[T], VectorBuilder[T]] = {
    new OpSet.InPlaceImpl2[VectorBuilder[T], VectorBuilder[T]] {
      def apply(a: VectorBuilder[T], b: VectorBuilder[T]): Unit = {
        if (a eq b) return
        a.clear()
        a.reserve(b.activeSize)
        var i = 0
        while (i < b.activeSize) {
          a.add(b.index(i), b.data(i))
          i += 1
        }
      }
    }
  }

  implicit def opFromCopyAndUpdate[Op, V, Other](
                                                  implicit op: InPlaceImpl2[Op, VectorBuilder[V], Other],
                                                  semi: Semiring[V],
                                                  dev: Zero[V],
                                                  classTag: ClassTag[V]): UImpl2[Op, VectorBuilder[V], Other, VectorBuilder[V]] = {
    BinaryOp.fromCopyAndUpdate[VectorBuilder[V], Other, Op](op, canCopyBuilder[V])
  }

  @expand
  @expand.valify
  implicit def canAxpy[@expand.args(Double, Long, Float, Int) T]
  : scaleAdd.InPlaceImpl3[VectorBuilder[T], T, VectorBuilder[T]] = {
    new scaleAdd.InPlaceImpl3[VectorBuilder[T], T, VectorBuilder[T]] {
      def apply(a: VectorBuilder[T], s: T, b: VectorBuilder[T]): Unit = {
        require(a.length < 0 || b.length < 0 || a.length == b.length, "Dimension mismatch!")
        if (a eq b) {
          a :*= (1 + s)
        } else {
          val bActiveSize: Int = b.activeSize
          a.reserve(bActiveSize + a.activeSize)
          var i = 0
          val bd = b.data
          while (i < bActiveSize) {
            a.add(b.index(i), s * bd(i))
            i += 1
          }
        }
      }
    }
  }

  implicit def canAxpy[T: Semiring : ClassTag]: scaleAdd.InPlaceImpl3[VectorBuilder[T], T, VectorBuilder[T]] = {
    new scaleAdd.InPlaceImpl3[VectorBuilder[T], T, VectorBuilder[T]] {
      val sr = implicitly[Semiring[T]]

      def apply(a: VectorBuilder[T], s: T, b: VectorBuilder[T]): Unit = {
        require(a.length < 0 || b.length < 0 || a.length == b.length, "Dimension mismatch!")

        if (a eq b) {
          a :*= sr.+(sr.one, s)
        } else {
          val bActiveSize: Int = b.activeSize
          a.reserve(bActiveSize + a.activeSize)
          var i = 0
          val bd = b.data
          while (i < bActiveSize) {
            a.add(b.index(i), sr.*(s, bd(i)))
            i += 1
          }
        }
      }
    }
  }

  implicit def space[T: Field : ClassTag]: MutableModule[VectorBuilder[T], T] = {
    MutableModule.make[VectorBuilder[T], T]({ (a: VectorBuilder[T], b: VectorBuilder[T], tolerance: Double) =>
      val aHV = a.toHashVector
      implicit val hvSpace: MutableVectorField[HashVector[T], T] = HashVector.space[T]
      import hvSpace._
      val diff: Double = norm(a.toHashVector - b.toHashVector)
      diff < tolerance
    })
  }

  // operations involving vectors:

  implicit def canAddInto_V_VB[V, Vec](
                                        implicit ev: Vec <:< Vector[V],
                                        semi: Semiring[V]): OpAdd.InPlaceImpl2[Vec, VectorBuilder[V]] = {
    new OpAdd.InPlaceImpl2[Vec, VectorBuilder[V]] {
      def apply(a: Vec, b: VectorBuilder[V]): Unit = {
        require(b.length < 0 || a.length == b.length, "Dimension mismatch!")
        var i = 0
        val bd = b.data
        while (i < b.activeSize) {
          a(b.index(i)) = semi.+(a(b.index(i)), bd(i))
          i += 1
        }
      }
    }

  }

  implicit def canSubInto_V_VB[V, Vec](
                                        implicit ev: Vec <:< Vector[V],
                                        semi: Ring[V]): OpSub.InPlaceImpl2[Vec, VectorBuilder[V]] = {
    new OpSub.InPlaceImpl2[Vec, VectorBuilder[V]] {
      def apply(a: Vec, b: VectorBuilder[V]): Unit = {
        require(b.length < 0 || a.length == b.length, "Dimension mismatch!")
        var i = 0
        val bd = b.data
        while (i < b.activeSize) {
          a(b.index(i)) = semi.-(a(b.index(i)), bd(i))
          i += 1
        }
      }
    }

  }

  implicit def canAddInto_VV_V[V, Vec](implicit ev: Vec <:< Vector[V]): OpAdd.InPlaceImpl2[VectorBuilder[V], Vec] = {
    new OpAdd.InPlaceImpl2[VectorBuilder[V], Vec] {
      def apply(a: VectorBuilder[V], b: Vec): Unit = {
        require(a.length < 0 || a.length == b.length, "Dimension mismatch!")
        ev(b) match {
          case b: StorageVector[V] =>
            var i = 0
            val bd = b.data
            while (i < b.iterableSize) {
              if (b.isActive(i))
                a.add(b.indexAt(i), bd(i))
              i += 1
            }

          case _ =>
            a.reserve(a.activeSize + b.activeSize)
            require(a.length == b.length, "Dimension mismatch!")
            for ((i, v) <- b.activeIterator) {
              a.add(i, v)
            }
        }

      }
    }

  }

  implicit def canSubInto_VV_V[V, Vec](
                                        implicit ev: Vec <:< Vector[V],
                                        ring: Ring[V]): OpSub.InPlaceImpl2[VectorBuilder[V], Vec] = {
    new OpSub.InPlaceImpl2[VectorBuilder[V], Vec] {
      def apply(a: VectorBuilder[V], b: Vec): Unit = {
        require(a.length < 0 || a.length == b.length, "Dimension mismatch!")
        b match {
          case b: StorageVector[V@unchecked] =>
            var i = 0
            val bd = b.data
            while (i < b.iterableSize) {
              if (b.isActive(i))
                a.add(b.indexAt(i), ring.negate(bd(i)))
              i += 1
            }

          case _ =>
            a.reserve(a.activeSize + b.activeSize)
            require(a.length == b.length, "Dimension mismatch!")
            for ((i, v) <- b.activeIterator) {
              a.add(i, ring.negate(v))
            }
        }

      }
    }

  }

  implicit def canDot_V_VB[Vec, V](
                                    implicit ev: Vec <:< Vector[V],
                                    semi: Semiring[V]): OpMulInner.Impl2[Vec, VectorBuilder[V], V] = {
    new OpMulInner.Impl2[Vec, VectorBuilder[V], V] {
      def apply(a: Vec, b: VectorBuilder[V]) = {
        require(b.length < 0 || a.length == b.length, "Dimension mismatch!")
        var result: V = semi.zero
        var i = 0
        val bd = b.data
        while (i < b.activeSize) {
          result = semi.+(result, semi.*(a(b.index(i)), bd(i)))
          i += 1
        }
        result
      }
    }
  }

  implicit def canAxpy_V_VB_Semi[V, Vec](
                                          implicit ev: Vec <:< Vector[V],
                                          semi: Semiring[V]): scaleAdd.InPlaceImpl3[Vec, V, VectorBuilder[V]] = {
    new scaleAdd.InPlaceImpl3[Vec, V, VectorBuilder[V]] {
      def apply(a: Vec, s: V, b: VectorBuilder[V]): Unit = {
        require(b.length < 0 || a.length == b.length, "Dimension mismatch!")
        var i = 0
        val bd = b.data
        while (i < b.activeSize) {
          a(b.index(i)) = semi.+(a(b.index(i)), semi.*(s, bd(i)))
          i += 1
        }
      }
    }
  }

  implicit def canDot_VB_V[Vec, V](
                                    implicit ev: Vec <:< Vector[V],
                                    semi: Semiring[V]): OpMulInner.Impl2[VectorBuilder[V], Vec, V] = {
    new OpMulInner.Impl2[VectorBuilder[V], Vec, V] {
      def apply(b: VectorBuilder[V], a: Vec) = {
        require(b.length < 0 || a.length == b.length, "Dimension mismatch!")
        var result: V = semi.zero
        var i = 0
        val bd = b.data
        while (i < b.activeSize) {
          result = semi.+(result, semi.*(bd(i), a(b.index(i))))
          i += 1
        }
        result
      }
    }
  }

  @expand
  @expand.valify
  implicit def canMulDMVB[@expand.args(Double, Int, Float, Long) T]
  : OpMulMatrix.Impl2[DenseMatrix[T], VectorBuilder[T], DenseVector[T]] = {
    new OpMulMatrix.Impl2[DenseMatrix[T], VectorBuilder[T], DenseVector[T]] {
      def apply(a: DenseMatrix[T], b: VectorBuilder[T]): DenseVector[T] = {
        val result = DenseVector.zeros[T](a.rows)
        cforRange(0 until b.activeSize) { i =>
          axpy(b.data(i), a(::, b.index(i)), result)
        }
        result
      }
    }
  }

  implicit def canMulDMVB_Semi[T: ClassTag](
                                             implicit semi: Semiring[T]): OpMulMatrix.Impl2[DenseMatrix[T], VectorBuilder[T], DenseVector[T]] = {
    new OpMulMatrix.Impl2[DenseMatrix[T], VectorBuilder[T], DenseVector[T]] {
      def apply(a: DenseMatrix[T], b: VectorBuilder[T]): DenseVector[T] = {
        val result = DenseVector.zeros[T](a.rows)
        cforRange(0 until b.activeSize) { i =>
          axpy(b.data(i), a(::, b.index(i)), result)
        }
        result
      }
    }
  }

}
