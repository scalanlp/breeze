package breeze.generic

import breeze.linalg.Axis
import breeze.linalg.support._

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
 * For example, exp is a UFunc: It just calls exp on all components of the passed in
 * object.
 *
 * Moreover, "operators" like [[breeze.linalg.operators.OpAdd]] are UFuncs as well,
 * with syntactic sugar provided by way of [[breeze.linalg.NumericOps]].
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
  final def apply[@specialized(Int, Double, Float) V,
                  @specialized(Int, Double, Float) VR]
                  (v: V)(implicit impl: Impl[V, VR]):VR = impl(v)

  final def apply[@specialized(Int, Double, Float) V1,
                  @specialized(Int, Double, Float) V2,
                  @specialized(Int, Double, Float) VR]
                   (v1: V1, v2: V2)(implicit impl: Impl2[V1, V2, VR]):VR = impl(v1, v2)

  // if there are three arguments, the first one is almost always a vector or something,
  // so no point specializing.
  final def apply[V1,
  @specialized(Int, Double, Float) V2,
  @specialized(Int, Double, Float) V3,
  @specialized(Int, Double, Float) VR]
  (v1: V1, v2: V2, v3: V3)(implicit impl: Impl3[V1, V2, V3, VR]):VR = impl(v1, v2, v3)

  final def apply[V1,V2, V3, V4, VR](v1: V1, v2: V2, v3: V3, v4: V4)(implicit impl: Impl4[V1, V2, V3, V4, VR]):VR = impl(v1, v2, v3, v4)

  final def inPlace[V](v: V)(implicit impl: UFunc.InPlaceImpl[this.type, V]) = impl(v)
  final def inPlace[V, V2](v: V, v2: V2)(implicit impl: UFunc.InPlaceImpl2[this.type, V, V2]) = impl(v, v2)
  final def inPlace[V, V2, V3](v: V, v2: V2, v3: V3)(implicit impl: UFunc.InPlaceImpl3[this.type, V, V2, V3]) = impl(v, v2, v3)

//  @implicitNotFound("Could not find an implicit implementation for this UFunc with arguments ${V}")
  type Impl[V, VR] = UFunc.UImpl[this.type, V, VR]
//  @implicitNotFound("Could not find an implicit implementation for this UFunc with arguments ${V1}, ${V2}")
  type Impl2[V1, V2, VR] = UFunc.UImpl2[this.type, V1, V2, VR]
//  @implicitNotFound("Could not find an implicit implementation for this UFunc with arguments ${V1}, ${V2}, ${V3}")
  type Impl3[V1, V2, V3, VR] = UFunc.UImpl3[this.type, V1, V2, V3, VR]
//  @implicitNotFound("Could not find an implicit implementation for this UFunc with arguments ${V1}, ${V2}, ${V3}, ${V4}")
  type Impl4[V1, V2, V3, V4, VR] = UFunc.UImpl4[this.type, V1, V2, V3, V4, VR]
//  @implicitNotFound("Could not find an implicit inplace implementation for this UFunc with arguments ${V}")
  type InPlaceImpl[V] = UFunc.InPlaceImpl[this.type, V]
//  @implicitNotFound("Could not find an inplace implicit implementation for this UFunc with arguments ${V1}, ${V2}")
  type InPlaceImpl2[V1, V2] = UFunc.InPlaceImpl2[this.type, V1, V2]
//  @implicitNotFound("Could not find an inplace implicit implementation for this UFunc with arguments ${V1}, ${V2}, ${V3}")
  type InPlaceImpl3[V1, V2, V3] = UFunc.InPlaceImpl3[this.type, V1, V2, V3]



  implicit def canZipMapValuesImpl[T, V1, VR, U](implicit handhold: ScalarOf[T, V1], impl: Impl2[V1, V1, VR], canZipMapValues: CanZipMapValues[T, V1, VR, U]): Impl2[T, T, U] = {
    new Impl2[T, T, U] {
      def apply(v1: T, v2: T): U = canZipMapValues.map(v1, v2, impl.apply)
    }
  }
}

trait VariableUFunc[U <: UFunc, T <: VariableUFunc[U,T]] { self:T =>
  final def apply[@specialized(Int, Double, Float) V,
                  @specialized(Int, Double, Float) VR]
                  (v: V)(implicit impl: UFunc.UImpl2[U,T,V,VR]):VR = impl(self, v)

  final def apply[@specialized(Int, Double, Float) V1,
                  @specialized(Int, Double, Float) V2,
                  @specialized(Int, Double, Float) VR]
                   (v1: V1, v2: V2)(implicit impl: UFunc.UImpl3[U,T,V1, V2, VR]):VR = impl(self, v1, v2)
}

trait MappingUFunc extends MappingUFuncLowPrio { this: UFunc =>
  implicit def fromLowOrderCanMapValues[T, V, V2, U](implicit handhold: ScalarOf[T, V], impl: Impl[V, V2], canMapValues: CanMapValues[T, V, V2, U]): Impl[T, U] = {
    new Impl[T, U] {
      def apply(v: T): U = canMapValues.map(v, impl.apply)
    }
  }



  implicit def canMapV1DV[T, V1, V2, VR, U](implicit handhold: ScalarOf[T, V1],
                                            impl: Impl2[V1, V2, VR],
                                            canMapValues: CanMapValues[T, V1, VR, U]): Impl2[T, V2, U] = {
    new Impl2[T, V2, U] {
      def apply(v1: T, v2: V2): U = canMapValues.map(v1, impl.apply(_, v2))
    }
  }



}

sealed trait MappingUFuncLowPrio { this: UFunc =>
  implicit def canMapV2Values[T, V1, V2, VR, U](implicit handhold: ScalarOf[T, V2], impl: Impl2[V1, V2, VR], canMapValues: CanMapValues[T, V2, VR, U]): Impl2[V1, T, U] = {
    new Impl2[V1, T, U] {
      def apply(v1: V1, v2: T): U = canMapValues.map(v2, impl.apply(v1, _))
    }
  }

}

object UFunc {
//  @implicitNotFound("Could not find an implicit implementation for ${Tag} with arguments ${V}")
  trait UImpl[Tag, @specialized(Int, Double, Float) V, @specialized(Int, Double, Float) +VR] extends Serializable {
    def apply(v: V):VR
  }

//  @implicitNotFound("Could not find an implicit implementation for ${Tag} with arguments ${V1}, ${V2}")
  trait UImpl2[Tag, @specialized(Int, Double, Float) V1, @specialized(Int, Double, Float) V2, @specialized(Int, Double, Float) +VR] extends Serializable {
    def apply(v: V1, v2: V2):VR
  }

//  @implicitNotFound("Could not find an implicit implementation for ${Tag} with arguments ${V1}, ${V2}, ${V3}")
  trait UImpl3[Tag, @specialized(Int, Double, Float) V1,
                    @specialized(Int, Double, Float) V2,
                    @specialized(Int, Double, Float) V3,
                    @specialized(Int, Double, Float) +VR] extends Serializable {
    def apply(v: V1, v2: V2, v3: V3):VR
  }

//  @implicitNotFound("Could not find an implicit implementation for ${Tag} with arguments ${V1}, ${V2}, ${V3}, ${V4}")
  trait UImpl4[Tag, @specialized(Int, Double, Float) V1,
                    @specialized(Int, Double, Float) V2,
                    @specialized(Int, Double, Float) V3,
                    @specialized(Int, Double, Float) V4,
                    @specialized(Int, Double, Float) +VR] extends Serializable {
    def apply(v: V1, v2: V2, v3: V3, v4: V4):VR
  }

//  @implicitNotFound("Could not find an implicit inplace implementation for ${Tag} with arguments ${V}")
  trait InPlaceImpl[Tag, V] extends Serializable {
    def apply(v: V)
  }

//  @implicitNotFound("Could not find an implicit inplace implementation for ${Tag} with arguments ${V}, ${V2}")
  trait InPlaceImpl2[Tag, V,  @specialized(Int, Double, Float) V2] extends Serializable {
    def apply(v: V, v2: V2)
  }

//  @implicitNotFound("Could not find an implicit inplace implementation for ${Tag} with arguments ${V}, ${V2}, ${V3}")
  trait InPlaceImpl3[Tag, V, V2, V3] extends Serializable {
    def apply(v: V, v2: V2, v3: V3)
  }

  // implicits for add impl's


// DEPRECATED
// ToDo: add Int => Double implicits for all necessary UFuncs
// (see https://github.com/scalanlp/breeze/issues/236)
//  implicit def implicitDoubleUTag[Tag, V, VR](implicit conv: V=>Double, impl: UImpl[Tag, Double, VR]):UImpl[Tag, V, VR] = {
//    new UImpl[Tag, V, VR] {
//      def apply(v: V): VR = impl(v)
//    }
//  }


  implicit def canTransformValuesUFunc[Tag, T, V](implicit canTransform: CanTransformValues[T, V],
                                                  impl: UImpl[Tag, V, V]):InPlaceImpl[Tag, T] = {
    new InPlaceImpl[Tag, T] {
      def apply(v: T) = { canTransform.transform(v, impl.apply) }
    }
  }


  implicit def collapseUred[Tag, V1, AxisT<:Axis, TA, VR, Result](implicit handhold: CanCollapseAxis.HandHold[V1, AxisT, TA], impl: UImpl[Tag, TA, VR], collapse: CanCollapseAxis[V1, AxisT, TA, VR, Result]) = new UImpl2[Tag, V1, AxisT, Result] {
    def apply(v: V1, v2: AxisT): Result = collapse.apply(v, v2)(impl(_))
  }

  implicit def collapseUred3[Tag, V1, AxisT<:Axis, V3, TA, VR, Result](implicit handhold: CanCollapseAxis.HandHold[V1, AxisT, TA], impl: UImpl2[Tag, TA, V3, VR], collapse: CanCollapseAxis[V1, AxisT, TA, VR, Result]) = new UImpl3[Tag, V1, AxisT, V3, Result] {
    def apply(v: V1, v2: AxisT, v3: V3): Result = collapse.apply(v, v2)(impl(_, v3))
  }

}
