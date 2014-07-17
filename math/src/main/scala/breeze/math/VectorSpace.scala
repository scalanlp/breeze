package breeze.math

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.linalg._

/**
 * Top-level Space trait defines only the implicit for retrieving Numeric Operations
 * Not entirely sure it should extend ElementWiseOps, but I guess I don't see any good
 * reason not to, since all the collections implement these already.
 * Wouldn't be hard to create an alternate hierarchy of spaces that include this trait though,
 * if there's a reason I'm missing to keep them separate.
 *
 * @tparam V Vector type
 * @author gabeos
 */
trait Space[V, S] {
  // Should be overridden to return *Ops representing the Numeric Operations available for
  // a given space.
  implicit def hasOps(v: V): SemiringElemOps[V]
}

// Operate on Vectors by element
trait ElementWiseImplOps[V, S] {
  implicit def mapValues: CanMapValues[V, S, S, V]

  implicit def zipMapValues: CanZipMapValues[V, S, S, V]

  implicit def iterateValues: CanTraverseValues[V, S]
}

trait Module[V, S] extends Space[V, S] {
  implicit def scalars: Ring[S]

  // This shouldn't really be here, but is for convenience..
  implicit def zeroLike: CanCreateZerosLike[V, V]

  // Abelian Group operator (addition)
  implicit def addVV: OpAdd.Impl2[V, V, V]

  implicit def subVV: OpSub.Impl2[V, V, V]

  implicit def neg: OpNeg.Impl[V, V]

  // Module operator
  implicit def mulVS: OpMulScalar.Impl2[V, S, V]

  implicit def mulVS_M: OpMulMatrix.Impl2[V, S, V] = mulVS.asInstanceOf[OpMulMatrix.Impl2[V, S, V]]

  implicit def hasOps(v: V): RingElemOps[V]

  def close(a: V, b: V, tolerance: Double): Boolean
}

trait MutableModuleImplOps[V, S] {
  self: Module[V, S] =>
  implicit def copy: CanCopy[V]

  implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S]

  implicit def mulIntoVS_M: OpMulMatrix.InPlaceImpl2[V, S] = mulIntoVS.asInstanceOf[OpMulMatrix.InPlaceImpl2[V, S]]

  implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V]

  implicit def subIntoVV: OpSub.InPlaceImpl2[V, V]

  implicit def setIntoVV: OpSet.InPlaceImpl2[V, V]

  implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V]
}

trait VectorSpaceImplOps[V, I, S] {
  self: Module[V, S] =>
  implicit def scalars: Field[S]

  implicit def divVS: OpDiv.Impl2[V, S, V]
}

trait MutableVectorSpaceImplOps[V, I, S] {
  self: VectorSpaceImplOps[V, I, S] with MutableModuleImplOps[V, S] with Module[V,S] =>
  implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S]
}

trait NormedSpaceImplOps[V, S] {
  self: Module[V, S] =>
  implicit def normImpl: norm.Impl[V, Double]

  implicit def scalarNorm: norm.Impl[S,Double] = scalars.normImpl

  implicit def normImplDouble: norm.Impl2[V, Double, Double]

  def close(a: V, b: V, tolerance: Double): Boolean = norm(subVV(a, b)) <= tolerance * math.max(norm(a), norm(b))
}

trait InnerProductSpaceImplOps[V, S] {
  self: Module[V, S] =>
  implicit def dotVV: OpMulInner.Impl2[V, V, S]

  implicit def normImpl: norm.Impl[V, Double] = new norm.Impl[V, Double] {
    def apply(v: V): Double = math.sqrt(scalars.sNorm(dotVV(v, v)))
  }
}

// The idea here is to treat a Module as a Ring of vectors under 
// element-wise addition and multiplication.
trait VectorRingImplOps[V, S] extends ElementWiseImplOps[V, S] {
  self: Module[V, S] with NormedSpaceImplOps[V,S] =>
  implicit def mulVV: OpMulScalar.Impl2[V, V, V]

  // Implicitly Broadcast scalars to vector-space
  implicit def addVS: OpAdd.Impl2[V, S, V]

  implicit def subVS: OpSub.Impl2[V, S, V]
}

// Mutable ops for VectorRing
trait MutableVectorRingOps[V, S] {
  self: VectorRingImplOps[V, S] with MutableModuleImplOps[V,S] with Module[V,S] =>
  implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S]

  implicit def subIntoVS: OpSub.InPlaceImpl2[V, S]

  implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V]

  implicit def setIntoVS: OpSet.InPlaceImpl2[V, S]
}

// Treat Vector Space as Field of Vectors under
// element-wise addition and multiplication, and inverses
trait VectorFieldImplOps[V, I, S] {
  self: VectorSpaceImplOps[V, I, S] with VectorRingImplOps[V, S] with NormedSpaceImplOps[V,S] with Module[V,S] =>
  implicit def divVV: OpDiv.Impl2[V, V, V]

  implicit def zero: CanCreateZeros[V,I]
}

// Mutable Ops for VectorField
trait MutableVectorFieldImplOps[V, I, S] {
  self: VectorFieldImplOps[V, I, S] with MutableVectorRingOps[V, S] with VectorRingImplOps[V,S]
    with NormedSpaceImplOps[V,S] with MutableVectorSpaceImplOps[V,I,S] with VectorSpaceImplOps[V,I,S]
    with MutableModuleImplOps[V,S] with Module[V,S] =>
  implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V]
}

// Modules

trait MutableModule[V, S] extends Module[V, S] with MutableModuleImplOps[V, S] {
  implicit def hasOps(v: V): MutableRingElemOps[V]
}

trait NormedModule[V, S] extends Module[V, S] with NormedSpaceImplOps[V, S] {
  implicit def hasOps(v: V): RingElemOps[V] with NormOps[V]
}

trait MutableNormedModule[V, S] extends MutableModule[V, S] with NormedModule[V, S] {
  implicit def hasOps(v: V): MutableRingElemOps[V] with NormOps[V]
}

trait InnerProductModule[V, S] extends NormedModule[V, S] with InnerProductSpaceImplOps[V, S] {
  implicit def hasOps(v: V): RingElemOps[V] with NormOps[V] with InnerProductOps[V]
}

trait MutableInnerProductModule[V, S] extends MutableModule[V, S] with InnerProductModule[V, S] {
  implicit def hasOps(v: V): MutableRingElemOps[V] with NormOps[V] with InnerProductOps[V]
}

trait VectorRing[V, S] extends VectorRingImplOps[V, S] with InnerProductModule[V, S] {
  implicit def hasOps(v: V): RingElemOps[V] with NormOps[V] with InnerProductOps[V]
}

trait MutableVectorRing[V, S] extends VectorRing[V, S] with MutableInnerProductModule[V,S] with MutableVectorRingOps[V, S] {
  implicit def hasOps(v: V): MutableRingElemOps[V] with NormOps[V] with InnerProductOps[V]
}

// Vector Spaces
trait VectorSpace[V, I, S] extends Module[V, S] with VectorSpaceImplOps[V, I, S] {
  implicit def hasOps(v: V): FieldElemOps[V]
}

trait MutableModuleSpace[V, I, S] extends MutableModule[V, S] with VectorSpace[V, I, S] with MutableVectorSpaceImplOps[V, I, S] {
  implicit def hasOps(v: V): MutableFieldElemOps[V]//with QuasiTensor[I,V]
}

trait NormedVectorSpace[V, I, S] extends VectorSpace[V, I, S] with NormedModule[V,S] {
  implicit def hasOps(v: V): FieldElemOps[V] with NormOps[V]
}

trait MutableNormedVectorSpace[V, I, S] extends MutableModuleSpace[V, I, S] with NormedVectorSpace[V, I, S] {
  implicit def hasOps(v: V): MutableFieldElemOps[V] with NormOps[V] //with QuasiTensor[I,V]
}

trait InnerProductVectorSpace[V, I, S] extends NormedVectorSpace[V, I, S] with InnerProductModule[V, S] {
  implicit def hasOps(v: V): FieldElemOps[V] with NormOps[V] with InnerProductOps[V]
}

trait MutableInnerProductVectorSpace[V, I, S] extends MutableModuleSpace[V, I, S]
                                                      with MutableInnerProductModule[V,S]
                                                      with InnerProductVectorSpace[V, I, S] {
  implicit def hasOps(v: V): MutableFieldElemOps[V] with NormOps[V] with InnerProductOps[V] // with QuasiTensor[I,V]
}

trait VectorField[V, I, S] extends VectorFieldImplOps[V, I, S] with InnerProductVectorSpace[V, I, S] with VectorRing[V, S] {
  implicit def hasOps(v: V): FieldElemOps[V] with InnerProductOps[V] with NormOps[V] with QuasiTensor[I,S]
}

trait MutableVectorField[V, I, S] extends VectorField[V, I, S]
                                          with MutableVectorRing[V, S]
                                          with MutableInnerProductVectorSpace[V,I,S]
                                          with MutableVectorFieldImplOps[V, I, S] {
  implicit def hasOps(v: V): MutableFieldElemOps[V] with NormOps[V] with InnerProductOps[V] with QuasiTensor[I,S]

}

object VectorField {
  def make[V, I, S](implicit _norm2: norm.Impl2[V, Double, Double],
                    _norm: norm.Impl[V, Double],
                    _field: Field[S],
                    //                    _scalarNorm: norm.Impl[S, Double] ,
                    _addVS: OpAdd.Impl2[V, S, V],
                    _subVS: OpSub.Impl2[V, S, V],
                    _mulVV: OpMulScalar.Impl2[V, V, V],
                    _divVV: OpDiv.Impl2[V, V, V],
                    _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
                    _zeroLike: CanCreateZerosLike[V, V],
                    _zero: CanCreateZeros[V, I],
                    _mulVS: OpMulScalar.Impl2[V, S, V],
                    _divVS: OpDiv.Impl2[V, S, V],
                    _addVV: OpAdd.Impl2[V, V, V],
                    _subVV: OpSub.Impl2[V, V, V],
                    _dotVV: OpMulInner.Impl2[V, V, S],
                    _neg: OpNeg.Impl[V, V],
                    _isVectorFieldOps: V <:< FieldElemOps[V] with InnerProductOps[V] with NormOps[V] with QuasiTensor[I,S],
                    _zipMapVals: CanZipMapValues[V, S, S, V],
                    _traverseVals: CanTraverseValues[V, S],
                    _mapVals: CanMapValues[V, S, S, V]): VectorField[V, I, S] = new VectorField[V, I, S] {
    //    def dim: I = d

    def scalars: Field[S] = _field

    //    override implicit def hasOps[VV <: V](v: VV): MutableRingElemOps[VV] with NormOps[VV]] = ???

    override implicit def hasOps(v: V): FieldElemOps[V] with InnerProductOps[V] with NormOps[V] with QuasiTensor[I,S] = _isVectorFieldOps(v)

    override implicit def normImpl: norm.Impl[V, Double] = _norm

    implicit def normImplDouble: norm.Impl2[V, Double, Double] = _norm2

    implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV

    implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS

    implicit def zero: CanCreateZeros[V, I] = _zero

    implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike

    implicit def subVS: OpSub.Impl2[V, S, V] = _subVS

    implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV

    implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV

    implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS

    implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS

    implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV

    implicit def subVV: OpSub.Impl2[V, V, V] = _subVV

    implicit def neg: OpNeg.Impl[V, V] = _neg

    override implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals

    override implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals

    override implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals

  }
}

object MutableModuleSpace {
  /** Construct a MutableInnerProductSpace for the given type from the available implicits */
  def make[V, I, S](closeTo: (V, V, Double) => Boolean)(implicit _field: Field[S],
                                                              _Ops: V <:< MutableFieldElemOps[V], // with QuasiTensor[I, V],
                                                              _zeroLike: CanCreateZerosLike[V, V],
                                                              _zero: CanCreateZeros[V,I],
                                                              _mulVS: OpMulScalar.Impl2[V, S, V],
                                                              _divVS: OpDiv.Impl2[V, S, V],
                                                              _addVV: OpAdd.Impl2[V, V, V],
                                                              _subVV: OpSub.Impl2[V, V, V],
                                                              _neg: OpNeg.Impl[V, V],
                                                              _copy: CanCopy[V],
                                                              _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
                                                              _divIntoVS: OpDiv.InPlaceImpl2[V, S],
                                                              _addIntoVV: OpAdd.InPlaceImpl2[V, V],
                                                              _subIntoVV: OpSub.InPlaceImpl2[V, V],
                                                              _setIntoVV: OpSet.InPlaceImpl2[V, V],
                                                              _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V]
//                                                              _zipMapVals: CanZipMapValues[V, S, S, V],
//                                                              _traverseVals: CanTraverseValues[V, S],
//                                                              _mapVals: CanMapValues[V, S, S, V]
    ): MutableModuleSpace[V, I, S] = new MutableModuleSpace[V, I, S] {


    def scalars: Field[S] = _field

    def close(a: V, b: V, tolerance: Double): Boolean = closeTo(a, b, tolerance)

    implicit def hasOps(v: V): MutableFieldElemOps[V] = _Ops(v)

    implicit def zero: CanCreateZeros[V,I] = _zero

    implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike

    implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS

    implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS

    implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV

    implicit def subVV: OpSub.Impl2[V, V, V] = _subVV

    implicit def neg: OpNeg.Impl[V, V] = _neg

    implicit def copy: CanCopy[V] = _copy

    implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS

    implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS

    implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV

    implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV

    implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV

    implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV

//    override implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals
//
//    override implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals
//
//    override implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals
  }
}

object MutableModule {
  /** Construct a MutableInnerProductSpace for the given type from the available implicits */
  def make[V, S](closeTo: (V, V, Double) => Boolean)(implicit _ring: Ring[S],
                                                     _zeroLike: CanCreateZerosLike[V, V],
                                                     _Ops: V <:< MutableRingElemOps[V],
                                                     _mulVS: OpMulScalar.Impl2[V, S, V],
                                                     _addVV: OpAdd.Impl2[V, V, V],
                                                     _subVV: OpSub.Impl2[V, V, V],
                                                     _neg: OpNeg.Impl[V, V],
                                                     _copy: CanCopy[V],
                                                     _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
                                                     _addIntoVV: OpAdd.InPlaceImpl2[V, V],
                                                     _subIntoVV: OpSub.InPlaceImpl2[V, V],
                                                     _setIntoVV: OpSet.InPlaceImpl2[V, V],
                                                     _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V]
//                                                     _zipMapVals: CanZipMapValues[V, S, S, V],
//                                                     _traverseVals: CanTraverseValues[V, S],
//                                                     _mapVals: CanMapValues[V, S, S, V]
  ): MutableModule[V, S] = new MutableModule[V, S] {
    def scalars: Ring[S] = _ring

    def close(a: V, b: V, tolerance: Double): Boolean = closeTo(a, b, tolerance)

    implicit def hasOps(v: V): MutableRingElemOps[V] = _Ops(v)

    implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike

    implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS

    implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV

    implicit def subVV: OpSub.Impl2[V, V, V] = _subVV

    implicit def neg: OpNeg.Impl[V, V] = _neg

    implicit def copy: CanCopy[V] = _copy

    implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS

    implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV

    implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV

    implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV

    implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV

//    override implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals
//
//    override implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals
//
//    override implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals
  }
}

object MutableInnerProductVectorSpace {
  /** Construct a MutableInnerProductSpace for the given type from the available implicits */
  def make[V, I, S](implicit _field: Field[S],
                          _normImpl: norm.Impl2[V, Double, Double],
                          _Ops: V <:< MutableFieldElemOps[V] with NormOps[V] with InnerProductOps[V],// with QuasiTensor[I, V],
                          _zeroLike: CanCreateZerosLike[V, V],
                          _zero: CanCreateZeros[V,I],
                          _mulVS: OpMulScalar.Impl2[V, S, V],
                          _divVS: OpDiv.Impl2[V, S, V],
                          _addVV: OpAdd.Impl2[V, V, V],
                          _subVV: OpSub.Impl2[V, V, V],
                          _neg: OpNeg.Impl[V, V],
                          _dotVV: OpMulInner.Impl2[V, V, S],
                          _copy: CanCopy[V],
                          _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
                          _divIntoVS: OpDiv.InPlaceImpl2[V, S],
                          _addIntoVV: OpAdd.InPlaceImpl2[V, V],
                          _subIntoVV: OpSub.InPlaceImpl2[V, V],
                          _setIntoVV: OpSet.InPlaceImpl2[V, V],
                          _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V]
//                          _zipMapVals: CanZipMapValues[V, S, S, V],
//                          _traverseVals: CanTraverseValues[V, S],
//                          _mapVals: CanMapValues[V, S, S, V]
    ): MutableInnerProductVectorSpace[V, I, S] = new MutableInnerProductVectorSpace[V, I, S] {
    def scalars: Field[S] = _field

    implicit def normImplDouble: norm.Impl2[V, Double, Double] = _normImpl

    implicit def hasOps(v: V): MutableFieldElemOps[V] with NormOps[V] with InnerProductOps[V] = _Ops(v)

    implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike

    implicit def zero: CanCreateZeros[V,I] = _zero

    implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS

    implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS

    implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV

    implicit def subVV: OpSub.Impl2[V, V, V] = _subVV

    implicit def neg: OpNeg.Impl[V, V] = _neg

    implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV

    implicit def copy: CanCopy[V] = _copy

    implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS

    implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS

    implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV

    implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV

    implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV

    implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV

//    override implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals
//
//    override implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals
//
//    override implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals
  }
}

object MutableInnerProductModule {
  /** Construct a MutableInnerProductModule for the given type from the available implicits */
  def make[V, S](implicit _ring: Ring[S],
                 _isIPOps: V <:< MutableRingElemOps[V] with NormOps[V] with InnerProductOps[V],
                 _zeroLike: CanCreateZerosLike[V, V],
                 _mulVS: OpMulScalar.Impl2[V, S, V],
                 _addVV: OpAdd.Impl2[V, V, V],
                 _subVV: OpSub.Impl2[V, V, V],
                 _neg: OpNeg.Impl[V, V],
                 _dotVV: OpMulInner.Impl2[V, V, S],
                 _normImplDouble: norm.Impl2[V, Double, Double],
                 _copy: CanCopy[V],
                 _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
                 _addIntoVV: OpAdd.InPlaceImpl2[V, V],
                 _subIntoVV: OpSub.InPlaceImpl2[V, V],
                 _setIntoVV: OpSet.InPlaceImpl2[V, V],
                 _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V]
//                 _zipMapVals: CanZipMapValues[V, S, S, V],
//                 _traverseVals: CanTraverseValues[V, S],
//                 _mapVals: CanMapValues[V, S, S, V]
  ): MutableInnerProductModule[V, S] = new MutableInnerProductModule[V, S] {
    def scalars: Ring[S] = _ring

    implicit def hasOps(v: V): MutableRingElemOps[V] with NormOps[V] with InnerProductOps[V] = _isIPOps(v)

    implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike

    implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS

    implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV

    implicit def subVV: OpSub.Impl2[V, V, V] = _subVV

    implicit def neg: OpNeg.Impl[V, V] = _neg

    implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV

    override implicit def normImplDouble: norm.Impl2[V, Double, Double] = _normImplDouble

    implicit def copy: CanCopy[V] = _copy

    implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS

    implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV

    implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV

    implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV

    implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV

//    override implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals
//
//    override implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals
//
//    override implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals

  }
}

object MutableVectorField {
  def make[V, I, S](implicit
                          _norm2: norm.Impl2[V, Double, Double],
                          _norm: norm.Impl[V, Double],
                          _field: Field[S],
                          _addVS: OpAdd.Impl2[V, S, V],
                          _subVS: OpSub.Impl2[V, S, V],
                          _mulVV: OpMulScalar.Impl2[V, V, V],
                          _divVV: OpDiv.Impl2[V, V, V],
                          _copy: CanCopy[V],
                          _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
                          _divIntoVS: OpDiv.InPlaceImpl2[V, S],
                          _addIntoVV: OpAdd.InPlaceImpl2[V, V],
                          _subIntoVV: OpSub.InPlaceImpl2[V, V],
                          _addIntoVS: OpAdd.InPlaceImpl2[V, S],
                          _subIntoVS: OpSub.InPlaceImpl2[V, S],
                          _mulIntoVV: OpMulScalar.InPlaceImpl2[V, V],
                          _divIntoVV: OpDiv.InPlaceImpl2[V, V],
                          _setIntoVV: OpSet.InPlaceImpl2[V, V],
                          _setIntoVS: OpSet.InPlaceImpl2[V, S],
                          //                   _powIntoVV:  OpPow.InPlaceImpl2[V, V],
                          //                   _powIntoVS:  OpPow.InPlaceImpl2[V, S],
                          //                   _modIntoVV:  OpMod.InPlaceImpl2[V, V],
                          //                   _modIntoVS:  OpMod.InPlaceImpl2[V, S],
                          _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
                          _zeroLike: CanCreateZerosLike[V, V],
                          _zero: CanCreateZeros[V, I],
                          _mulVS: OpMulScalar.Impl2[V, S, V],
                          _divVS: OpDiv.Impl2[V, S, V],
                          _addVV: OpAdd.Impl2[V, V, V],
                          _subVV: OpSub.Impl2[V, V, V],
                          _neg: OpNeg.Impl[V, V],
                          _isVectorFieldOps: V <:< MutableFieldElemOps[V] with NormOps[V] with InnerProductOps[V] with QuasiTensor[I,S],
                          _dotVV: OpMulInner.Impl2[V, V, S],
                          _zipMapVals: CanZipMapValues[V, S, S, V],
                          _traverseVals: CanTraverseValues[V, S],
                          _mapVals: CanMapValues[V, S, S, V]): MutableVectorField[V, I, S] = new MutableVectorField[V, I, S] {
//    def dim: I = d

    def scalars: Field[S] = _field

//    override implicit def hasOps[VV <: V](v: VV): MutableRingElemOps[VV] with NormOps[VV]] = ???

    override implicit def hasOps(v: V): MutableFieldElemOps[V] with NormOps[V] with InnerProductOps[V] with QuasiTensor[I,S] = _isVectorFieldOps(v)

    override implicit def normImpl: norm.Impl[V, Double] = _norm

    implicit def normImplDouble: norm.Impl2[V, Double, Double] = _norm2

    implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS

    implicit def zero: CanCreateZeros[V, I] = _zero

    implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike

    implicit def subVS: OpSub.Impl2[V, S, V] = _subVS

    implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV

    implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV

    //    implicit def powVV: OpPow.Impl2[V, V, V] = _powVV
    //
    //    implicit def powVS: OpPow.Impl2[V, S, V] = _powVS
    //
    //    implicit def modVV: OpMod.Impl2[V, V, V] = _modVV
    //
    //    implicit def modVS: OpMod.Impl2[V, S, V] = _modVS

    implicit def copy: CanCopy[V] = _copy

    implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS

    implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS

    implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV

    implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV

    implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S] = _addIntoVS

    implicit def subIntoVS: OpSub.InPlaceImpl2[V, S] = _subIntoVS

    implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = _mulIntoVV

    implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V] = _divIntoVV

    //    implicit def powIntoVV: OpPow.InPlaceImpl2[V, V] = _powIntoVV
    //
    //    implicit def powIntoVS: OpPow.InPlaceImpl2[V, S] = _powIntoVS
    //
    //    implicit def modIntoVV: OpMod.InPlaceImpl2[V, V] = _modIntoVV
    //
    //    implicit def modIntoVS: OpMod.InPlaceImpl2[V, S] = _modIntoVS

    implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS

    implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS

    implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV

    implicit def subVV: OpSub.Impl2[V, V, V] = _subVV

    implicit def neg: OpNeg.Impl[V, V] = _neg

    //    implicit def isVectorFieldOps(v: V): VectorFieldOps[V] with QuasiTensor[I, S] = _isVectorFieldOps(v)

    implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV

    implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV

    implicit def setIntoVS: OpSet.InPlaceImpl2[V, S] = _setIntoVS

    implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV

    override implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals

    override implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals

    override implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals

  }
}
