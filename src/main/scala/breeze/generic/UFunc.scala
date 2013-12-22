package breeze.generic

import breeze.macros.expand
import breeze.linalg.support.CanZipMapValues
import breeze.linalg.Axis

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


/**
 * "Universal" Functions that mimic numpy's. A universal function is typically defined
 * on anything that supports elementwise maps.
 *
 * For example, exp is a UFunc: it just calls exp on all components of the passed in
 * object.
 *
 * Additional implementations can be added as implicits by extending a UFunc's
 * Impl or InPlaceImpl traits. For example, [[breeze.math.Complex]] extends [[breeze.numerics.log]]
 * with the following implicit:
 *
 * {{{
    implicit object logComplexImpl extends breeze.numerics.log.Impl[Complex, Complex] { def apply(v: Complex) = v.log }
 * }}}
 *
 *
 *
 *@author dlwh
 */
trait UFunc {
  final def apply[V, VR](v: V)(implicit impl: Impl[V, VR]):VR = impl(v)
  final def apply[V1, V2, VR](v1: V1, v2: V2)(implicit impl: Impl2[V1, V2, VR]):VR = impl(v1, v2)
  final def inPlace[V](v: V)(implicit impl: InPlaceImpl[V]) = impl(v)


  type Impl[V, VR] = UFunc.UImpl[this.type, V, VR]
  type Impl2[V1, V2, VR] = UFunc.UImpl2[this.type, V1, V2, VR]


  trait InPlaceImpl[V] {
    def apply(v: V)
  }



  implicit def implicitDoubleUTag[V, VR](implicit conv: V=>Double, impl: Impl[Double, VR]):Impl[V, VR] = {
    new Impl[V, VR] {
      def apply(v: V): VR = impl(v)
    }
  }


  implicit def canTransformValuesUFunc[Tag, T, V](implicit canTransform: CanTransformValues[T, V, V],
                                                      impl: Impl[V, V]):InPlaceImpl[T] = {
    new InPlaceImpl[T] {
      def apply(v: T) = { canTransform.transform(v, impl.apply) }
    }
  }


  implicit def collapseUred[V1, AxisT<:Axis, TA, VR, Result](implicit handhold: CanCollapseAxis.HandHold[V1, AxisT, TA], impl: Impl[TA, VR], collapse: CanCollapseAxis[V1, AxisT, TA, VR, Result]) = new Impl2[V1, AxisT, Result] {
    def apply(v: V1, v2: AxisT): Result = collapse.apply(v, v2)(impl(_))
  }
}

trait MappingUFunc extends UFuncX { this: UFunc =>

  implicit def fromCanMapValues[T[_], V, V2, U[_]](implicit impl: Impl[V, V2], canMapValues: CanMapValues[T[V], V, V2, U[V2]]): Impl[T[V], U[V2]] = {
    new Impl[T[V], U[V2]] {
      def apply(v: T[V]): U[V2] = canMapValues.map(v, impl.apply)
    }
  }


}

trait UFuncX { this: UFunc =>
  implicit def fromLowOrderCanMapValues[T, V, V2, U](implicit canMapValues: CanMapValues[T, V, V2, U], impl: Impl[V, V2]): Impl[T, U] = {
    new Impl[T, U] {
      def apply(v: T): U = canMapValues.map(v, impl.apply)
    }
  }

}

object UFunc extends UFunc2LowPrio {
  trait UImpl[Tag, V, VR] {
    def apply(v: V):VR
  }

  trait UImpl2[Tag, V1, V2, VR] {
    def apply(v: V1, v2: V2):VR
  }



  /*
  implicit def canMapValues[Tag, T, V, VR, U](implicit impl: UImpl[Tag, V, VR], canMapValues: CanMapValues[T, V, VR, U]): UImpl[Tag, T, U] = {
    new UImpl[Tag, T, U] {
      def apply(v: T): U = canMapValues.map(v, impl.apply)
    }
  }
  */


}

import breeze.generic.UFunc.UImpl2

trait UFunc2LowPrio { this: UFunc.type =>
  /*
  implicit def canZipMapValues[Tag, T, V1, VR, U](implicit impl: UImpl2[Tag, V1, V1, VR], canZipMapValues: CanZipMapValues[T, V1, VR, U]): UImpl2[Tag, T, T, U] = {
    new UImpl2[Tag, T, T, U] {
      def apply(v1: T, v2: T): U = canZipMapValues.map(v1, v2, impl.apply)
    }
  }

  implicit def canMapV1Values[Tag, T, V1, V2, VR, U](implicit impl: UImpl2[Tag, V1, V2, VR], canMapValues: CanMapValues[T, V1, VR, U]): UImpl2[Tag, T, V2, U] = {
    new UImpl2[Tag, T, V2, U] {
      def apply(v1: T, v2: V2): U = canMapValues.map(v1, impl.apply(_, v2))
    }
  }

  implicit def canMapV2Values[Tag, T, V1, V2, VR, U](implicit impl: UImpl2[Tag, V1, V2, VR], canMapValues: CanMapValues[T, V2, VR, U]): UImpl2[Tag, V1, T, U] = {
    new UImpl2[Tag, V1, T, U] {
      def apply(v1: V1, v2: T): U = canMapValues.map(v2, impl.apply(v1, _))
    }
  }
  */
}

/* Sadly we have to specialize these for types we want to use. Rawr */
trait UFunc2ZippingImplicits[T[_]] {

  implicit def canZipMapValuesUImpl[Tag, V1, VR, U](implicit impl: UImpl2[Tag, V1, V1, VR], canZipMapValues: CanZipMapValues[T[V1], V1, VR, U]): UImpl2[Tag, T[V1], T[V1], U] = {
    new UImpl2[Tag, T[V1], T[V1], U] {
      def apply(v1: T[V1], v2: T[V1]): U = canZipMapValues.map(v1, v2, impl.apply)
    }
  }

  implicit def canMapV1DV[Tag, V1, V2, VR, U](implicit impl: UImpl2[Tag, V1, V2, VR], canMapValues: CanMapValues[T[V1], V1, VR, U]): UImpl2[Tag, T[V1], V2, U] = {
    new UImpl2[Tag, T[V1], V2, U] {
      def apply(v1: T[V1], v2: V2): U = canMapValues.map(v1, impl.apply(_, v2))
    }
  }

  implicit def canMapV2Values[Tag, V1, V2, VR, U](implicit impl: UImpl2[Tag, V1, V2, VR], canMapValues: CanMapValues[T[V2], V2, VR, U]): UImpl2[Tag, V1, T[V2], U] = {
    new UImpl2[Tag, V1, T[V2], U] {
      def apply(v1: V1, v2: T[V2]): U = canMapValues.map(v2, impl.apply(v1, _))
    }
  }

}

