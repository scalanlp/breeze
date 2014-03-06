package breeze.linalg.operators

import breeze.macros.expand
import breeze.generic.UFunc.{UImpl2, InPlaceImpl2}
import breeze.math.{Ring, MutableVectorSpace, Semiring}
import breeze.storage.DefaultArrayValue
import scala.reflect.ClassTag
import breeze.linalg.support.CanAxpy
import breeze.linalg._


trait VectorBuilderOps { this: VectorBuilder.type =>
  @expand
  @expand.valify
  implicit def canOpInto_V_S[@expand.args(OpMulScalar, OpDiv) Op,
  @expand.args(Double, Long, Float, Int) T](implicit @expand.sequence[Op]((_ * _), (_ / _)) op: Q): Op.InPlaceImpl2[VectorBuilder[T], T] =  {
    new  Op.InPlaceImpl2[VectorBuilder[T], T]  {
      def apply(a: VectorBuilder[T], b: T) {
        var i = 0
        while(i < a.activeSize) {
          a.data(i) = op(a.data(i), b)
          i += 1
        }
      }
    }

  }


  @expand
  @expand.valify
  implicit def canOpInto_V_V[@expand.args(OpAdd, OpSub) Op,
  @expand.args(Double, Long, Float, Int) T](implicit @expand.sequence[Op]((x => x), (- _)) op: Q): Op.InPlaceImpl2[VectorBuilder[T], VectorBuilder[T]] =  {
    new  Op.InPlaceImpl2[VectorBuilder[T], VectorBuilder[T]]  {
      def apply(a: VectorBuilder[T], b: VectorBuilder[T]) {
        require(a.length == b.length, "Dimension mismatch!")
        a.reserve(a.activeSize + b.activeSize)
        var i = 0
        // read once here in case we're doing a += a
        val bActiveSize = b.activeSize
        while(i < bActiveSize) {
          a.add(b.index(i), op(b.data(i)))
          i += 1
        }
      }
    }

  }


  @expand
  @expand.valify
  implicit def canSet[@expand.args(Double, Long, Float, Int) T]: OpSet.InPlaceImpl2[VectorBuilder[T], VectorBuilder[T]] =  {
    new  OpSet.InPlaceImpl2[VectorBuilder[T], VectorBuilder[T]]  {
      def apply(a: VectorBuilder[T], b: VectorBuilder[T]) {
        if(a eq b) return
        a.clear()
        a.reserve(b.activeSize)
        var i = 0
        while(i < b.activeSize) {
          a.add(b.index(i), b.data(i))
          i += 1
        }
      }
    }

  }

  implicit def opFromCopyAndUpdate[Op, V, Other](implicit op: InPlaceImpl2[Op, VectorBuilder[V], Other],
                                                 semi: Semiring[V],
                                                 dev: DefaultArrayValue[V],
                                                 classTag: ClassTag[V]): UImpl2[Op, VectorBuilder[V], Other, VectorBuilder[V]] = {
    BinaryOp.fromCopyAndUpdate[VectorBuilder[V], Other, Op](op, canCopyBuilder[V])
  }


  @expand
  @expand.valify
  implicit def canAxpy[@expand.args(Double, Long, Float, Int) T]: CanAxpy[T, VectorBuilder[T], VectorBuilder[T]] = {
    new  CanAxpy[T, VectorBuilder[T], VectorBuilder[T]]  {
      def apply(s: T, b: VectorBuilder[T], a: VectorBuilder[T]) {
        require(a.length == b.length, "Dimension mismatch!")
        if(a eq b) {
          a :*= (1+s)
        } else {
          val bActiveSize: Int = b.activeSize
          a.reserve(bActiveSize + a.activeSize)
          var i = 0
          val bd = b.data
          while(i < bActiveSize) {
            a.add(b.index(i), s * bd(i))
            i += 1
          }
        }
      }
    }
  }

  @expand
  @expand.valify
  implicit def mvector_space[@expand.args(Double, Long, Float, Int) T]: MutableVectorSpace[VectorBuilder[T], T] = {
    MutableVectorSpace.make[VectorBuilder[T], T] { (a:VectorBuilder[T], b: VectorBuilder[T], tolerance: Double) =>
      val diff: Double = (a.toHashVector - b.toHashVector).norm(2)
      if(diff > tolerance)
        println((a,b,a.toHashVector, b.toHashVector,diff))
      diff < tolerance
    }
  }


  // operations involving vectors:

  implicit def canAddInto_V_VB[V, Vec](implicit ev: Vec <:<Vector[V], semi: Semiring[V]): OpAdd.InPlaceImpl2[Vec, VectorBuilder[V]] =  {
    new  OpAdd.InPlaceImpl2[Vec, VectorBuilder[V]]  {
      def apply(a: Vec, b: VectorBuilder[V]) {
        require(a.length == b.length, "Dimension mismatch!")
        var i = 0
        val bd = b.data
        while(i < b.activeSize) {
          a(b.index(i)) = semi.+(a(b.index(i)), bd(i))
          i += 1
        }
      }
    }

  }

  implicit def canSubInto_V_VB[V, Vec](implicit ev: Vec <:<Vector[V], semi: Ring[V]): OpSub.InPlaceImpl2[Vec, VectorBuilder[V]] =  {
    new  OpSub.InPlaceImpl2[Vec, VectorBuilder[V]]  {
      def apply(a: Vec, b: VectorBuilder[V]) {
        require(a.length == b.length, "Dimension mismatch!")
        var i = 0
        val bd = b.data
        while(i < b.activeSize) {
          a(b.index(i)) = semi.-(a(b.index(i)), bd(i))
          i += 1
        }
      }
    }

  }

  implicit def canAddInto_VV_V[V, Vec](implicit ev: Vec <:<Vector[V]): OpAdd.InPlaceImpl2[VectorBuilder[V], Vec] =  {
    new  OpAdd.InPlaceImpl2[VectorBuilder[V], Vec]  {
      def apply(a: VectorBuilder[V], b: Vec) {
        ev(b) match {
          case b: StorageVector[V] =>
            var i = 0
            val bd = b.data
            while(i < b.iterableSize) {
              if(b.isActive(i))
                a.add(b.indexAt(i), bd(i))
              i += 1
            }

          case _ =>
            a.reserve(a.activeSize + b.activeSize)
            require(a.length == b.length, "Dimension mismatch!")
            for( (i,v) <- b.activeIterator) {
              a.add(i, v)
            }
        }

      }
    }

  }

  implicit def canSubInto_VV_V[V, Vec](implicit ev: Vec <:<Vector[V], ring: Ring[V]): OpSub.InPlaceImpl2[VectorBuilder[V], Vec] =  {
    new  OpSub.InPlaceImpl2[VectorBuilder[V], Vec]  {
      def apply(a: VectorBuilder[V], b: Vec) {
        b match {
          case b: StorageVector[V] =>
            var i = 0
            val bd = b.data
            while(i < b.iterableSize) {
              if(b.isActive(i))
                a.add(b.indexAt(i), ring.negate(bd(i)))
              i += 1
            }

          case _ =>
            a.reserve(a.activeSize + b.activeSize)
            require(a.length == b.length, "Dimension mismatch!")
            for( (i,v) <- b.activeIterator) {
              a.add(i, ring.negate(v))
            }
        }

      }
    }

  }

  implicit def canDot_V_VB[Vec, V](implicit ev: Vec <:<Vector[V], semi: Semiring[V]): OpMulInner.Impl2[Vec, VectorBuilder[V], V] =  {
    new  OpMulInner.Impl2[Vec, VectorBuilder[V], V]  {
      def apply(a: Vec, b: VectorBuilder[V]) =  {
        require(a.length == b.length, "Dimension mismatch!")
        var result : V = semi.zero
        var i = 0
        val bd = b.data
        while(i < b.activeSize) {
          result = semi.+(result, semi.*(a(b.index(i)), bd(i)))
          i += 1
        }
        result
      }
    }
  }

  implicit def canAxpy_V_VB_Double[V, Vec](implicit ev: Vec <:< Vector[V], semi: Semiring[V]): CanAxpy[V, VectorBuilder[V], Vec] = {
    new  CanAxpy[V, VectorBuilder[V], Vec]  {
      def apply(s: V, b: VectorBuilder[V], a: Vec) {
        require(a.length == b.length, "Dimension mismatch!")
        var i = 0
        val bd = b.data
        while(i < b.activeSize) {
          a(b.index(i)) = semi.+(a(b.index(i)), semi.*(s, bd(i)))
          i += 1
        }
      }
    }
  }

  implicit def canDot_VB_V[Vec, V](implicit ev: Vec <:<Vector[V], semi: Semiring[V]): OpMulInner.Impl2[VectorBuilder[V], Vec, V] =  {
    new  OpMulInner.Impl2[VectorBuilder[V], Vec, V]  {
      def apply(b: VectorBuilder[V], a: Vec) =  {
        require(a.length == b.length, "Dimension mismatch!")
        var result : V = semi.zero
        var i = 0
        val bd = b.data
        while(i < b.activeSize) {
          result = semi.+(result, semi.*(bd(i), a(b.index(i))))
          i += 1
        }
        result
      }
    }
  }



}