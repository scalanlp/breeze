package breeze.math

import breeze.linalg.{norm, NumericOps}
import breeze.linalg.support._
import breeze.linalg.operators._
import breeze.util.Isomorphism
import breeze.generic.{UFunc}
import breeze.generic.UFunc.UImpl2
import spire.algebra._
import spire.implicits._

/**
 *
 *
 * @author dlwh
 */
trait MutablizingAdaptor[+VS[_, _], MVS[_,_], V, S] {
  type Wrapper
  val underlying: VS[V, S]
  implicit val mutaVspace: MVS[Wrapper, S]

  implicit val isomorphism:Isomorphism[V, Wrapper] = new Isomorphism[V, Wrapper] {
    def forward(value: V): Wrapper = wrap(value)

    def backward(u: Wrapper): V = unwrap(u)
  }

  def wrap(v: V):Wrapper
  def unwrap(w: Wrapper):V
}

object MutablizingAdaptor {
  def ensureMutable[V, S](vs: VectorSpace[V, S]):MutablizingAdaptor[VectorSpace, MutableVectorSpace, V, S] = {
    if(vs.isInstanceOf[MutableVectorSpace[_, _]]) IdentityWrapper(vs.asInstanceOf[MutableVectorSpace[V, S]])
    else VectorSpaceAdaptor(vs)
  }

  def ensureMutable[V, S](vs: InnerProductSpace[V, S]):MutablizingAdaptor[InnerProductSpace, MutableInnerProductSpace, V, S] = {
    if(vs.isInstanceOf[MutableInnerProductSpace[_, _]]) IdentityWrapper(vs.asInstanceOf[MutableInnerProductSpace[V, S]])
    else InnerProductSpaceAdaptor(vs)
  }


  def ensureMutable[V, S](vs: CoordinateSpace[V, S]):MutablizingAdaptor[CoordinateSpace, MutableCoordinateSpace, V, S] = {
    if(vs.isInstanceOf[MutableCoordinateSpace[_, _]]) IdentityWrapper(vs.asInstanceOf[MutableCoordinateSpace[V, S]])
    else CoordinateSpaceAdaptor(vs)
  }

  case class IdentityWrapper[VS[_, _], V, S](underlying: VS[V, S]) extends MutablizingAdaptor[VS, VS, V, S] {
    type Wrapper = V
    implicit val mutaVspace: VS[Wrapper, S] = underlying

    def wrap(v: V): Wrapper = v
    def unwrap(v: Wrapper):V = v
  }

  case class VectorSpaceAdaptor[V, S](val underlying: VectorSpace[V, S]) extends MutablizingAdaptor[VectorSpace, MutableVectorSpace, V, S] {
    type Wrapper = Ref[V]


    def wrap(v: V): Wrapper = Ref(v)

    def unwrap(w: Wrapper): V = w.value

    implicit val mutaVspace: MutableVectorSpace[Wrapper, S] = new MutableVectorSpace[Wrapper, S] {
      val u = underlying
      def field: Field[S] = underlying.field
      implicit def isNumericOps(v: Wrapper) = v

      implicit def zeros: CanCreateZerosLike[Wrapper, Wrapper] = new CanCreateZerosLike[Wrapper, Wrapper] {
        // Should not inherit from Form=>To because the compiler will try to use it to coerce types.
        def apply(from: Wrapper): Wrapper = from.map(underlying.zeros apply)
      }

      implicit def copy: CanCopy[Wrapper] = new CanCopy[Wrapper] {
        // Should not inherit from Form=>To because the compiler will try to use it to coerce types.
        def apply(from: Wrapper): Wrapper = from
      }

      def liftUpdate[Op<:OpType](implicit op: UFunc.UImpl2[Op, V, S, V]):UFunc.InPlaceImpl2[Op, Wrapper, S] = new UFunc.InPlaceImpl2[Op, Wrapper, S] {
        def apply(a: Wrapper, b: S) {
          a.value = op(a.value, b)
        }
      }

      def liftUpdateV[Op <: OpType](implicit op: UFunc.UImpl2[Op, V, V, V]):UFunc.InPlaceImpl2[Op, Wrapper, Wrapper] = new UFunc.InPlaceImpl2[Op, Wrapper, Wrapper] {
        def apply(a: Wrapper, b: Wrapper) {
          a.value = op(a.value, b.value)
        }
      }

      def liftOp[Op <: OpType](implicit op: UFunc.UImpl2[Op, V, S, V]):UFunc.UImpl2[Op, Wrapper, S, Wrapper] = new UImpl2[Op, Wrapper, S, Wrapper] {
        def apply(a: Wrapper, b: S) = {
          a.map(op(_, b))
        }
      }

      def liftOpV[Op <: OpType](implicit op:UFunc.UImpl2[Op, V, V, V]):UFunc.UImpl2[Op, Wrapper, Wrapper, Wrapper] = new UFunc.UImpl2[Op, Wrapper, Wrapper, Wrapper] {
        def apply(a: Wrapper, b: Wrapper) = {
          a.map(op(_, b.value))
        }
      }

      implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[Wrapper, S] = liftUpdate(u.mulVS)

      implicit def divIntoVS: OpDiv.InPlaceImpl2[Wrapper, S] = liftUpdate(u.divVS)

      implicit def addIntoVV: OpAdd.InPlaceImpl2[Wrapper, Wrapper] = liftUpdateV(u.addVV)

      implicit def subIntoVV: OpSub.InPlaceImpl2[Wrapper, Wrapper] = liftUpdateV(u.subVV)

      implicit def setIntoVV: OpSet.InPlaceImpl2[Wrapper, Wrapper] = new OpSet.InPlaceImpl2[Wrapper, Wrapper] {
        def apply(a: Wrapper, b: Wrapper) {
          a.value = b.value
        }
      }

      implicit def axpyVV: CanAxpy[S, Wrapper, Wrapper] = {
        new CanAxpy[S, Wrapper, Wrapper] {
          def apply(a: S, x: Wrapper, y: Wrapper) {y += x * a}
        }
      }

      implicit def mulVS: OpMulScalar.Impl2[Wrapper, S, Wrapper] = liftOp(u.mulVS)

      implicit def divVS: OpDiv.Impl2[Wrapper, S, Wrapper] = liftOp(u.divVS)

      implicit def addVV: OpAdd.Impl2[Wrapper, Wrapper, Wrapper] = liftOpV(u.addVV)

      implicit def subVV: OpSub.Impl2[Wrapper, Wrapper, Wrapper] = liftOpV(u.subVV)

      def close(a: Wrapper, b: Wrapper, tolerance: Double): Boolean = u.close(a.value, b.value, tolerance)

      // default implementations
      implicit def neg: OpNeg.Impl[Wrapper, Wrapper] = new OpNeg.Impl[Wrapper, Wrapper] {
        def apply(a: Wrapper): Wrapper = a.map(u.neg apply)
      }
    }
  }

  case class InnerProductSpaceAdaptor[V, S](val underlying: InnerProductSpace[V, S]) extends MutablizingAdaptor[InnerProductSpace, MutableInnerProductSpace, V, S] {
    type Wrapper = Ref[V]

    def wrap(v: V): Wrapper = Ref(v)

    def unwrap(w: Wrapper): V = w.value

    implicit val mutaVspace: MutableInnerProductSpace[Wrapper, S] = new MutableInnerProductSpace[Wrapper, S] {
      val u = underlying
      def field: Field[S] = underlying.field
      implicit def isNumericOps(v: Wrapper) = v

      implicit def zeros: CanCreateZerosLike[Wrapper, Wrapper] = new CanCreateZerosLike[Wrapper, Wrapper] {
        // Should not inherit from Form=>To because the compiler will try to use it to coerce types.
        def apply(from: Wrapper): Wrapper = from.map(underlying.zeros apply)
      }

      implicit def copy: CanCopy[Wrapper] = new CanCopy[Wrapper] {
        // Should not inherit from Form=>To because the compiler will try to use it to coerce types.
        def apply(from: Wrapper): Wrapper = from
      }

      def liftUpdate[Op <: OpType](implicit op: UImpl2[Op, V, S, V]):UFunc.InPlaceImpl2[Op, Wrapper, S] = new UFunc.InPlaceImpl2[Op, Wrapper, S] {
        def apply(a: Wrapper, b: S) {
          a.value = op(a.value, b)
        }
      }

      def liftUpdateV[Op <: OpType](implicit op: UImpl2[Op, V, V, V]):UFunc.InPlaceImpl2[Op, Wrapper, Wrapper] = new UFunc.InPlaceImpl2[Op, Wrapper, Wrapper] {
        def apply(a: Wrapper, b: Wrapper) {
          a.value = op(a.value, b.value)
        }
      }

      def liftOp[Op <: OpType](implicit op: UImpl2[Op, V, S, V]):UImpl2[Op, Wrapper, S, Wrapper] = new UImpl2[Op, Wrapper, S, Wrapper] {
        def apply(a: Wrapper, b: S) = {
          a.map(op(_, b))
        }
      }

      def liftOpV[Op <: OpType](implicit op: UImpl2[Op, V, V, V]):UImpl2[Op, Wrapper, Wrapper, Wrapper] = new UImpl2[Op, Wrapper, Wrapper, Wrapper] {
        def apply(a: Wrapper, b: Wrapper) = {
          a.map(op(_, b.value))
        }
      }


      implicit def scalarNorm = u.scalarNorm

      implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[Wrapper, S] = liftUpdate(u.mulVS)

      implicit def divIntoVS: OpDiv.InPlaceImpl2[Wrapper, S] = liftUpdate(u.divVS)

      implicit def addIntoVV: OpAdd.InPlaceImpl2[Wrapper, Wrapper] = liftUpdateV(u.addVV)

      implicit def subIntoVV: OpSub.InPlaceImpl2[Wrapper, Wrapper] = liftUpdateV(u.subVV)

      implicit def setIntoVV: OpSet.InPlaceImpl2[Wrapper, Wrapper] = new OpSet.InPlaceImpl2[Wrapper, Wrapper] {
        def apply(a: Wrapper, b: Wrapper) {
          a.value = b.value
        }
      }

      implicit def axpyVV: CanAxpy[S, Wrapper, Wrapper] = {
        new CanAxpy[S, Wrapper, Wrapper] {
          def apply(a: S, x: Wrapper, y: Wrapper) {y += x * a}
        }
      }

      implicit def mulVS: OpMulScalar.Impl2[Wrapper, S, Wrapper] = liftOp(u.mulVS)

      implicit def divVS: OpDiv.Impl2[Wrapper, S, Wrapper] = liftOp(u.divVS)

      implicit def addVV: OpAdd.Impl2[Wrapper, Wrapper, Wrapper] = liftOpV(u.addVV)

      implicit def subVV: OpSub.Impl2[Wrapper, Wrapper, Wrapper] = liftOpV(u.subVV)

      override def close(a: Wrapper, b: Wrapper, tolerance: Double): Boolean = u.close(a.value, b.value, tolerance)

      // default implementations
      implicit def neg: OpNeg.Impl[Wrapper, Wrapper] = new OpNeg.Impl[Wrapper, Wrapper] {
        def apply(a: Wrapper): Wrapper = a.map(u.neg apply)
      }

      implicit def dotVV: OpMulInner.Impl2[Wrapper, Wrapper, S] = new OpMulInner.Impl2[Wrapper, Wrapper, S] {
        def apply(a: Wrapper, b: Wrapper): S = {
          u.dotVV(a.value, b.value)
        }
      }
    }
  }

  case class CoordinateSpaceAdaptor[V, S](underlying: CoordinateSpace[V, S]) extends MutablizingAdaptor[CoordinateSpace, MutableCoordinateSpace, V, S] {
    type Wrapper = Ref[V]

    def wrap(v: V): Wrapper = Ref(v)

    def unwrap(w: Wrapper): V = w.value

    implicit val mutaVspace: MutableCoordinateSpace[Wrapper, S] = new MutableCoordinateSpace[Wrapper, S] {
      val u = underlying
      def field: Field[S] = underlying.field
      implicit def isNumericOps(v: Wrapper) = v

      implicit def zeros: CanCreateZerosLike[Wrapper, Wrapper] = new CanCreateZerosLike[Wrapper, Wrapper] {
        // Should not inherit from Form=>To because the compiler will try to use it to coerce types.
        def apply(from: Wrapper): Wrapper = from.map(underlying.zeros apply)
      }

      implicit def copy: CanCopy[Wrapper] = new CanCopy[Wrapper] {
        // Should not inherit from Form=>To because the compiler will try to use it to coerce types.
        def apply(from: Wrapper): Wrapper = from
      }

      def liftUpdate[Op <: OpType](implicit op: UFunc.UImpl2[Op, V, S, V]):UFunc.InPlaceImpl2[Op, Wrapper, S] = new UFunc.InPlaceImpl2[Op, Wrapper, S] {
        def apply(a: Wrapper, b: S) {
          a.value = op(a.value, b)
        }
      }

      def liftUpdateV[Op <: OpType](implicit op:UFunc.UImpl2[Op, V, V, V]):UFunc.InPlaceImpl2[Op, Wrapper, Wrapper] = new UFunc.InPlaceImpl2[Op, Wrapper, Wrapper] {
        def apply(a: Wrapper, b: Wrapper) {
          a.value = op(a.value, b.value)
        }
      }

      def liftOp[Op <: OpType](implicit op:UFunc.UImpl2[Op, V, S, V]):UFunc.UImpl2[Op, Wrapper, S, Wrapper] = new UFunc.UImpl2[Op, Wrapper, S, Wrapper] {
        def apply(a: Wrapper, b: S) = {
          a.map(op(_, b))
        }
      }

      def liftOpV[Op <: OpType](implicit op: UFunc.UImpl2[Op, V, V, V]): UFunc.UImpl2[Op, Wrapper, Wrapper, Wrapper] = new UFunc.UImpl2[Op, Wrapper, Wrapper, Wrapper] {
        def apply(a: Wrapper, b: Wrapper) = {
          a.map(op(_, b.value))
        }
      }

      implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[Wrapper, S] = liftUpdate(u.mulVS)

      implicit def divIntoVS: OpDiv.InPlaceImpl2[Wrapper, S] = liftUpdate(u.divVS)

      implicit def addIntoVV: OpAdd.InPlaceImpl2[Wrapper, Wrapper] = liftUpdateV(u.addVV)

      implicit def subIntoVV: OpSub.InPlaceImpl2[Wrapper, Wrapper] = liftUpdateV(u.subVV)

      implicit def setIntoVV: OpSet.InPlaceImpl2[Wrapper, Wrapper] = new OpSet.InPlaceImpl2[Wrapper, Wrapper] {
        def apply(a: Wrapper, b: Wrapper) {
          a.value = b.value
        }
      }

      implicit def axpyVV: CanAxpy[S, Wrapper, Wrapper] = {
        new CanAxpy[S, Wrapper, Wrapper] {
          def apply(a: S, x: Wrapper, y: Wrapper) {y += x * a}
        }
      }

      implicit def mulVS: OpMulScalar.Impl2[Wrapper, S, Wrapper] = liftOp(u.mulVS)

      implicit def divVS: OpDiv.Impl2[Wrapper, S, Wrapper] = liftOp(u.divVS)

      implicit def addVV: OpAdd.Impl2[Wrapper, Wrapper, Wrapper] = liftOpV(u.addVV)

      implicit def subVV: OpSub.Impl2[Wrapper, Wrapper, Wrapper] = liftOpV(u.subVV)

      override def close(a: Wrapper, b: Wrapper, tolerance: Double): Boolean = u.close(a.value, b.value, tolerance)

      // default implementations
      implicit def neg: OpNeg.Impl[Wrapper, Wrapper] = new OpNeg.Impl[Wrapper, Wrapper] {
        def apply(a: Wrapper): Wrapper = a.map(u.neg apply)
      }

      implicit def dotVV: OpMulInner.Impl2[Wrapper, Wrapper, S] = new OpMulInner.Impl2[Wrapper, Wrapper, S] {
        def apply(a: Wrapper, b: Wrapper): S = {
          u.dotVV(a.value, b.value)
        }
      }

      implicit def normImplDouble: norm.Impl2[Wrapper, Double, Double]  = new norm.Impl2[Wrapper, Double, Double] {
        def apply(v1: Wrapper, v2: Double): Double = u.normImplDouble(v1.value, v2)
      }


      implicit def scalarNorm = u.scalarNorm

      implicit def mapValues: CanMapValues[Wrapper, S, S, Wrapper] = new CanMapValues[Wrapper, S, S, Wrapper] {
        /** Maps all key-value pairs from the given collection. */
        def map(from: Wrapper, fn: (S) => S): Wrapper = {
          from.map(u.mapValues.map(_, fn))
        }

        /** Maps all active key-value pairs from the given collection. */
        def mapActive(from: Wrapper, fn: (S) => S): Wrapper = {
          from.map(u.mapValues.mapActive(_, fn))
        }
      }

      implicit def zipMapValues: CanZipMapValues[Wrapper, S, S, Wrapper] = new CanZipMapValues[Wrapper, S, S, Wrapper] {
        /** Maps all corresponding values from the two collections. */
        def map(from: Wrapper, from2: Wrapper, fn: (S, S) => S): Wrapper = {
          from.map(u.zipMapValues.map(_, from2.value, fn))
        }
      }

      implicit def addVS: OpAdd.Impl2[Wrapper, S, Wrapper] = liftOp(u.addVS)

      implicit def subVS: OpSub.Impl2[Wrapper, S, Wrapper] = liftOp(u.subVS)

      implicit def mulVV: OpMulScalar.Impl2[Wrapper, Wrapper, Wrapper] = liftOpV(u.mulVV)

      implicit def divVV: OpDiv.Impl2[Wrapper, Wrapper, Wrapper] = liftOpV(u.divVV)

      implicit def addIntoVS: OpAdd.InPlaceImpl2[Wrapper, S] = liftUpdate(u.addVS)

      implicit def subIntoVS: OpSub.InPlaceImpl2[Wrapper, S] = liftUpdate(u.subVS)

      implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[Wrapper, Wrapper] = liftUpdateV(u.mulVV)

      implicit def divIntoVV: OpDiv.InPlaceImpl2[Wrapper, Wrapper] = liftUpdateV(u.divVV)

      implicit def setIntoVS: OpSet.InPlaceImpl2[Wrapper, S] = new OpSet.InPlaceImpl2[Wrapper, S] {
        def apply(a: Wrapper, b: S) {
          a.value = (zeros(a) + b).value
        }
      }
    }
  }



  case class Ref[T](var value: T) extends NumericOps[Ref[T]] {
    def repr: Ref[T] = this
    def map[U](f: T=>U) = Ref(f(value))
  }



}
