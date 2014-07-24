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
import breeze.storage.Zero

import scala.reflect.ClassTag

/**
 * @tparam V Vector type
 * @tparam S Scalar type
 * @author gabeos
 */
// Operate on Vectors by element
trait ElementWiseImplOps[V, S] {
  implicit def mapValues: CanMapValues[V, S, S, V]
  implicit def zipMapValues: CanZipMapValues[V, S, S, V]
  implicit def iterateValues: CanTraverseValues[V, S]
}

trait AdditiveTensorAbelianGroup[V, S] {
  implicit def scalars: Semiring[S]
  implicit def addVV: OpAdd.Impl2[V, V, V]   // Abelian Group operator (addition)
  implicit def addVS: OpAdd.Impl2[V, S, V]   // Implicitly Broadcast scalars to vector-space
}

trait Module[V, S] extends AdditiveTensorAbelianGroup[V, S] {
  implicit def scalars: Ring[S]

  // Extra operations that are defined over ring derived from abelian group addition
  // e.g. scalar additive inverse + abelian group addition
  implicit def subVV: OpSub.Impl2[V, V, V]
  implicit def subVS: OpSub.Impl2[V, S, V]
  implicit def zeroLike: CanCreateZerosLike[V, V]

  // Module operator
  implicit def mulVS: OpMulScalar.Impl2[V, S, V]
  implicit def mulVS_M: OpMulMatrix.Impl2[V, S, V] = mulVS.asInstanceOf[OpMulMatrix.Impl2[V, S, V]]

  // Brings NumericOps into scope
  implicit def hasOps(v: V): NumericOps[V]

  def close(a: V, b: V, tolerance: Double): Boolean
}

trait MutableModuleImplOps[V, S] {
  self: Module[V, S] =>
  implicit def copy: CanCopy[V]
  implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S]
  implicit def mulIntoVS_M: OpMulMatrix.InPlaceImpl2[V, S] = mulIntoVS.asInstanceOf[OpMulMatrix.InPlaceImpl2[V, S]]
  implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V]
  implicit def subIntoVV: OpSub.InPlaceImpl2[V, V]
  implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S]
  implicit def subIntoVS: OpSub.InPlaceImpl2[V, S]
  implicit def setIntoVV: OpSet.InPlaceImpl2[V, V]
  implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V]
}

trait VectorSpaceImplOps[V, S] {
  self: Module[V, S] =>
  implicit def scalars: Field[S]
  implicit def divVS: OpDiv.Impl2[V, S, V]   // Inverse module operator since Fields have multiplicative inverse
}

trait MutableVectorSpaceImplOps[V, S] {
  self: VectorSpaceImplOps[V, S] with MutableModuleImplOps[V, S] with Module[V, S] =>
  implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S]
}

trait NormedSpaceImplOps[V, S] {
  self: Module[V, S] =>
  implicit def normImpl: norm.Impl[V, Double]
  implicit def scalarNorm: norm.Impl[S, Double] = scalars.normImpl
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

// Immutable Ops for Ring of Vectors (under element-wise operations)
trait VectorRingImplOps[V, S] {
  self: Module[V, S] with NormedSpaceImplOps[V, S] =>
  implicit def mulVV: OpMulScalar.Impl2[V, V, V]
  implicit def neg: OpNeg.Impl[V, V]
//  implicit def modVV: OpMod.Impl2[V, V, V]
//  implicit def modVS: OpMod.Impl2[V, S, V]
}

// Mutable ops for Ring of Vectors
trait MutableVectorRingOps[V, S] extends ElementWiseImplOps[V, S] {
  self: VectorRingImplOps[V, S] with MutableModuleImplOps[V, S] with Module[V, S] =>
  implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V]
//  implicit def modIntoVV: OpMod.InPlaceImpl2[V, V]
//  implicit def modIntoVS: OpMod.InPlaceImpl2[V, S]
  implicit def setIntoVS: OpSet.InPlaceImpl2[V, S]
}

// Immutable ops for Field of Vectors (under element-wise operations)
trait VectorFieldImplOps[V, S] {
  self: VectorSpaceImplOps[V, S] with VectorRingImplOps[V, S] with NormedSpaceImplOps[V, S] with Module[V, S] =>
  implicit def divVV: OpDiv.Impl2[V, V, V]
//  implicit def powVV: OpPow.Impl2[V, V, V]
//  implicit def powVS: OpPow.Impl2[V, S, V]
}

// Mutable Ops for Field of Vectors
trait MutableVectorFieldImplOps[V, S] {
  self: VectorFieldImplOps[V, S] with MutableVectorRingOps[V, S] with VectorRingImplOps[V, S]
    with NormedSpaceImplOps[V, S] with MutableVectorSpaceImplOps[V, S] with VectorSpaceImplOps[V, S]
    with MutableModuleImplOps[V, S] with Module[V, S] =>
  implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V]
//  implicit def powIntoVV: OpPow.InPlaceImpl2[V, V]
//  implicit def powIntoVS: OpPow.InPlaceImpl2[V, S]
}

// Ops for Tensors where key type is specified
trait RestrictedDomainTensorFieldImplOps[V, I, S] {
  self: VectorFieldImplOps[V, S] with VectorRingImplOps[V, S]
    with NormedSpaceImplOps[V, S] with VectorSpaceImplOps[V, S]
    with Module[V, S] =>
  implicit def zero: CanCreateZeros[V, I]
  implicit def canDim: dim.Impl[V,I]
  implicit def tabulateTensor: CanTabulate[I,V,S]
}

// Modules
trait MutableModule[V, S] extends Module[V, S] with MutableModuleImplOps[V, S]

trait NormedModule[V, S] extends Module[V, S] with NormedSpaceImplOps[V, S]

trait MutableNormedModule[V, S] extends MutableModule[V, S] with NormedModule[V, S]

trait InnerProductModule[V, S] extends NormedModule[V, S] with InnerProductSpaceImplOps[V, S]

trait MutableInnerProductModule[V, S] extends MutableModule[V, S] with InnerProductModule[V, S]

// Vector Spaces
trait VectorSpace[V, S] extends Module[V, S] with VectorSpaceImplOps[V, S]

trait MutableVectorSpace[V, S] extends MutableModule[V, S] with VectorSpace[V, S] with MutableVectorSpaceImplOps[V, S]

trait NormedVectorSpace[V, S] extends VectorSpace[V, S] with NormedModule[V, S]

trait MutableNormedVectorSpace[V, S] extends MutableVectorSpace[V, S] with NormedVectorSpace[V, S]

trait InnerProductVectorSpace[V, S] extends NormedVectorSpace[V, S] with InnerProductModule[V, S]

trait MutableInnerProductVectorSpace[V, S] extends MutableVectorSpace[V, S]
                                                   with MutableInnerProductModule[V, S]
                                                   with InnerProductVectorSpace[V, S]

// Groups over vectors under element-wise operations.
// e.g. VectorField is a Field of Vectors under element-wise addition, negation, multiplication and inversion.
//      Under the corresponding Matrix operations, vectors no longer form a Field
trait VectorRing[V, S] extends VectorRingImplOps[V, S] with InnerProductModule[V, S]

trait MutableVectorRing[V, S] extends VectorRing[V, S] with MutableInnerProductModule[V, S] with MutableVectorRingOps[V, S]

trait VectorField[V, S] extends VectorFieldImplOps[V, S] with InnerProductVectorSpace[V, S] with VectorRing[V, S]

trait MutableVectorField[V, S] extends VectorField[V, S]
                                       with MutableVectorRing[V, S]
                                       with MutableInnerProductVectorSpace[V, S]
                                       with MutableVectorFieldImplOps[V, S]


// Same idea as VectorField, but with explicit key type specified. 
// Brings QuasiTensor methods into scope.
trait TensorField[V, I, S] extends VectorField[V, S] {
  implicit def hasOps(v: V): NumericOps[V] with QuasiTensor[I, S]

}

trait MutableTensorField[V, I, S] extends TensorField[V, I, S] with MutableVectorField[V, S]


// Tensor Field with generic zeros operation. Only useful for the Matrix and Vector hierarchies where the domain can be
// specified by the dimension of the Tensor.
trait RestrictedDomainTensorField[V, I, S] extends TensorField[V, I, S] with RestrictedDomainTensorFieldImplOps[V, I, S]

trait MutableRestrictedDomainTensorField[V, I, S] extends RestrictedDomainTensorField[V, I, S] with MutableTensorField[V, I, S]

trait MutableOptimizationSpace[M,V,S] extends MutableRestrictedDomainTensorField[V,Int,S] {
  def toMatrix(v: V): M
  def toVector(m: M): V
  implicit def mulMMS: OpMulScalar.Impl2[M,M,M]
  implicit def mulMMM: OpMulMatrix.Impl2[M,M,M]
  implicit def mulMVV: OpMulMatrix.Impl2[M,V,V]
  implicit def mulVTM: OpMulMatrix.Impl2[V,Transpose[V],M]
  implicit def canTrans: CanTranspose[V,Transpose[V]]
  implicit def negM: OpNeg.Impl[M,M]
  implicit def tabulateTensorM: CanTabulate[(Int,Int), M, S]
  implicit def zeroM: CanCreateZeros[M, (Int,Int)]
  implicit def canDimM: dim.Impl[M, (Int,Int)]
  implicit def hasMOps(v: M): NumericOps[M] with QuasiTensor[(Int,Int), S]
  implicit def normMImplDouble: norm.Impl2[M, Double, Double]
  implicit def divMM: OpDiv.Impl2[M,M,M]
  implicit def subMS: OpSub.Impl2[M,S,M]
  implicit def subMM: OpSub.Impl2[M, M, M]
  implicit def mulMS: OpMulScalar.Impl2[M, S, M]
  implicit def zeroLikeM: CanCreateZerosLike[M, M]
  implicit def addMS: OpAdd.Impl2[M, S, M]
  implicit def addMM: OpAdd.Impl2[M, M, M]
  implicit def divMS: OpDiv.Impl2[M, S, M]
  implicit def dotMM: OpMulInner.Impl2[M, M, S]
  implicit def divIntoMM: OpDiv.InPlaceImpl2[M, M]
  implicit def divIntoMS: OpDiv.InPlaceImpl2[M, S]
  implicit def copyM: CanCopy[M]
  implicit def mulIntoMS: OpMulScalar.InPlaceImpl2[M, S]
  implicit def addIntoMM: OpAdd.InPlaceImpl2[M, M]
  implicit def subIntoMM: OpSub.InPlaceImpl2[M, M]
  implicit def addIntoMS: OpAdd.InPlaceImpl2[M, S]
  implicit def subIntoMS: OpSub.InPlaceImpl2[M, S]
  implicit def setIntoMM: OpSet.InPlaceImpl2[M, M]
  implicit def scaleAddMM: scaleAdd.InPlaceImpl3[M, S, M]
  implicit def setIntoMS: OpSet.InPlaceImpl2[M, S]
  implicit def mulIntoMM: OpMulScalar.InPlaceImpl2[M, M]
  implicit def mapValuesM: CanMapValues[M, S, S, M]
  implicit def zipMapValuesM: CanZipMapValues[M, S, S, M]
  implicit def iterateValuesM: CanTraverseValues[M, S]
}

object VectorField {
  def make[V, S](implicit _norm2: norm.Impl2[V, Double, Double],
                 _norm: norm.Impl[V, Double],
                 _field: Field[S],
                 _addVS: OpAdd.Impl2[V, S, V],
                 _subVS: OpSub.Impl2[V, S, V],
                 _mulVV: OpMulScalar.Impl2[V, V, V],
                 _divVV: OpDiv.Impl2[V, V, V],
                 _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
                 _zeroLike: CanCreateZerosLike[V, V],
                 _mulVS: OpMulScalar.Impl2[V, S, V],
                 _divVS: OpDiv.Impl2[V, S, V],
                 _addVV: OpAdd.Impl2[V, V, V],
                 _subVV: OpSub.Impl2[V, V, V],
                 _dotVV: OpMulInner.Impl2[V, V, S],
                 _neg: OpNeg.Impl[V, V],
                 _ops: V <:< NumericOps[V]
//                 _modVV: OpMod.Impl2[V, V, V],
//                 _modVS: OpMod.Impl2[V, S, V],
//                 _powVV: OpPow.Impl2[V, V, V],
//                 _powVS: OpPow.Impl2[V, S, V]
                  ): VectorField[V, S] = new VectorField[V, S] {
    def scalars: Field[S] = _field
    override implicit def hasOps(v: V): NumericOps[V] = _ops(v)
    override implicit def normImpl: norm.Impl[V, Double] = _norm
    override implicit def normImplDouble: norm.Impl2[V, Double, Double] = _norm2
    override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    override implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def subVS: OpSub.Impl2[V, S, V] = _subVS
    override implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV
    override implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def neg: OpNeg.Impl[V, V] = _neg
//    override implicit def modVV: OpMod.Impl2[V, V, V] = _modVV
//    override implicit def modVS: OpMod.Impl2[V, S, V] = _modVS
//    override implicit def powVS: OpPow.Impl2[V, S, V] = _powVS
//    override implicit def powVV: OpPow.Impl2[V, V, V] = _powVV
  }
}

object MutableModule {
  /** Construct a MutableInnerProductSpace for the given type from the available implicits */
  def make[V, S](closeTo: (V, V, Double) => Boolean)(implicit _ring: Ring[S],
                                                     _zeroLike: CanCreateZerosLike[V, V],
                                                     _ops: V <:< NumericOps[V],
                                                     _mulVS: OpMulScalar.Impl2[V, S, V],
                                                     _addVV: OpAdd.Impl2[V, V, V],
                                                     _addVS: OpAdd.Impl2[V, S, V],
                                                     _subVV: OpSub.Impl2[V, V, V],
                                                     _subVS: OpSub.Impl2[V, S, V],
                                                     _neg: OpNeg.Impl[V, V],
                                                     _copy: CanCopy[V],
                                                     _mulIntoVS: OpMulScalar.InPlaceImpl2[V, S],
                                                     _addIntoVV: OpAdd.InPlaceImpl2[V, V],
                                                     _addIntoVS: OpAdd.InPlaceImpl2[V, S],
                                                     _subIntoVV: OpSub.InPlaceImpl2[V, V],
                                                     _subIntoVS: OpSub.InPlaceImpl2[V, S],
                                                     _setIntoVV: OpSet.InPlaceImpl2[V, V],
                                                     _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V]
    ): MutableModule[V, S] = new MutableModule[V, S] {
    def scalars: Ring[S] = _ring
    def close(a: V, b: V, tolerance: Double): Boolean = closeTo(a, b, tolerance)
    override implicit def hasOps(v: V): NumericOps[V] = _ops(v)
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def copy: CanCopy[V] = _copy
    override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
    override implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS
    override implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S] = _addIntoVS
    override implicit def subIntoVS: OpSub.InPlaceImpl2[V, S] = _subIntoVS
    override implicit def subVS: OpSub.Impl2[V, S, V] = _subVS
    override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
  }
}

object MutableInnerProductVectorSpace {
  /** Construct a MutableInnerProductSpace for the given type from the available implicits */
  def make[V, S](implicit _field: Field[S],
                 _normImpl: norm.Impl2[V, Double, Double],
                 _ops: V <:< NumericOps[V],
                 _zeroLike: CanCreateZerosLike[V, V],
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
                 _addVS: OpAdd.Impl2[V, S, V],
                 _subVS: OpSub.Impl2[V, S, V],
                 _addIntoVS: OpAdd.InPlaceImpl2[V, S],
                 _subIntoVS: OpSub.InPlaceImpl2[V, S],
                 _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V]
                  ): MutableInnerProductVectorSpace[V, S] = new MutableInnerProductVectorSpace[V, S] {
    def scalars: Field[S] = _field
    override implicit def normImplDouble: norm.Impl2[V, Double, Double] = _normImpl
    override implicit def hasOps(v: V): NumericOps[V] = _ops(v)
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    override implicit def copy: CanCopy[V] = _copy
    override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    override implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
    override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
    override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
    override implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS
    override implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S] = _addIntoVS
    override implicit def subIntoVS: OpSub.InPlaceImpl2[V, S] = _subIntoVS
    override implicit def subVS: OpSub.Impl2[V, S, V] = _subVS
  }
}

object MutableInnerProductModule {
  /** Construct a MutableInnerProductModule for the given type from the available implicits */
  def make[V, S](implicit _ring: Ring[S],
                 _ops: V <:< NumericOps[V],
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
                 _addVS: OpAdd.Impl2[V, S, V],
                 _subVS: OpSub.Impl2[V, S, V],
                 _addIntoVS: OpAdd.InPlaceImpl2[V, S],
                 _subIntoVS: OpSub.InPlaceImpl2[V, S],
                 _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V]
                  ): MutableInnerProductModule[V, S] = new MutableInnerProductModule[V, S] {
    def scalars: Ring[S] = _ring
    override implicit def hasOps(v: V): NumericOps[V] = _ops(v)
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    override implicit def normImplDouble: norm.Impl2[V, Double, Double] = _normImplDouble
    override implicit def copy: CanCopy[V] = _copy
    override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
    override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
    override implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS
    override implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S] = _addIntoVS
    override implicit def subIntoVS: OpSub.InPlaceImpl2[V, S] = _subIntoVS
    override implicit def subVS: OpSub.Impl2[V, S, V] = _subVS
  }
}

object MutableVectorField {
  def make[V, S](implicit
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
                 _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
                 _zeroLike: CanCreateZerosLike[V, V],
                 _mulVS: OpMulScalar.Impl2[V, S, V],
                 _divVS: OpDiv.Impl2[V, S, V],
                 _addVV: OpAdd.Impl2[V, V, V],
                 _subVV: OpSub.Impl2[V, V, V],
                 _neg: OpNeg.Impl[V, V],
                 _ops: V <:< NumericOps[V],
                 _dotVV: OpMulInner.Impl2[V, V, S],
                 _zipMapVals: CanZipMapValues[V, S, S, V],
                 _traverseVals: CanTraverseValues[V, S],
                 _mapVals: CanMapValues[V, S, S, V]
//                 _modVV: OpMod.Impl2[V, V, V],
//                 _modVS: OpMod.Impl2[V, S, V],
//                 _powVV: OpPow.Impl2[V, V, V],
//                 _powVS: OpPow.Impl2[V, S, V],
//                 _modIntoVV: OpMod.InPlaceImpl2[V, V],
//                 _modIntoVS: OpMod.InPlaceImpl2[V, S],
//                 _powIntoVV: OpPow.InPlaceImpl2[V, V],
//                 _powIntoVS: OpPow.InPlaceImpl2[V, S]
                  ): MutableVectorField[V, S] = new MutableVectorField[V, S] {

    def scalars: Field[S] = _field
    override implicit def hasOps(v: V): NumericOps[V] = _ops(v)
    override implicit def normImpl: norm.Impl[V, Double] = _norm
    override implicit def normImplDouble: norm.Impl2[V, Double, Double] = _norm2
    override implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def subVS: OpSub.Impl2[V, S, V] = _subVS
    override implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV
    override implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV
//    override implicit def powVV: OpPow.Impl2[V, V, V] = _powVV
//    override implicit def powVS: OpPow.Impl2[V, S, V] = _powVS
//    override implicit def modVV: OpMod.Impl2[V, V, V] = _modVV
//    override implicit def modVS: OpMod.Impl2[V, S, V] = _modVS
    override implicit def copy: CanCopy[V] = _copy
    override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    override implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
    override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
    override implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S] = _addIntoVS
    override implicit def subIntoVS: OpSub.InPlaceImpl2[V, S] = _subIntoVS
    override implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = _mulIntoVV
    override implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V] = _divIntoVV
//    override implicit def powIntoVV: OpPow.InPlaceImpl2[V, V] = _powIntoVV
//    override implicit def powIntoVS: OpPow.InPlaceImpl2[V, S] = _powIntoVS
//    override implicit def modIntoVV: OpMod.InPlaceImpl2[V, V] = _modIntoVV
//    override implicit def modIntoVS: OpMod.InPlaceImpl2[V, S] = _modIntoVS
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def neg: OpNeg.Impl[V, V] = _neg
    override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    override implicit def setIntoVS: OpSet.InPlaceImpl2[V, S] = _setIntoVS
    override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
    override implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals
    override implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals
    override implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals
  }
}

object MutableTensorField {
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
                    _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
                    _zeroLike: CanCreateZerosLike[V, V],
                    _mulVS: OpMulScalar.Impl2[V, S, V],
                    _divVS: OpDiv.Impl2[V, S, V],
                    _addVV: OpAdd.Impl2[V, V, V],
                    _subVV: OpSub.Impl2[V, V, V],
                    _neg: OpNeg.Impl[V, V],
//                    _modVV: OpMod.Impl2[V, V, V],
//                    _modVS: OpMod.Impl2[V, S, V],
//                    _powVV: OpPow.Impl2[V, V, V],
//                    _powVS: OpPow.Impl2[V, S, V],
//                    _modIntoVV: OpMod.InPlaceImpl2[V, V],
//                    _modIntoVS: OpMod.InPlaceImpl2[V, S],
//                    _powIntoVV: OpPow.InPlaceImpl2[V, V],
//                    _powIntoVS: OpPow.InPlaceImpl2[V, S],
                    _ops: V <:< NumericOps[V] with QuasiTensor[I, S],
                    _dotVV: OpMulInner.Impl2[V, V, S],
                    _zipMapVals: CanZipMapValues[V, S, S, V],
                    _traverseVals: CanTraverseValues[V, S],
                    _mapVals: CanMapValues[V, S, S, V]): MutableTensorField[V, I, S] = new MutableTensorField[V, I, S] {
    def scalars: Field[S] = _field
    override implicit def hasOps(v: V): NumericOps[V] with QuasiTensor[I, S] = _ops(v)
    override implicit def normImpl: norm.Impl[V, Double] = _norm
    override implicit def normImplDouble: norm.Impl2[V, Double, Double] = _norm2
    override implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def subVS: OpSub.Impl2[V, S, V] = _subVS
    override implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV
    override implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV
    override implicit def copy: CanCopy[V] = _copy
    override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    override implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
    override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
    override implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S] = _addIntoVS
    override implicit def subIntoVS: OpSub.InPlaceImpl2[V, S] = _subIntoVS
    override implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = _mulIntoVV
    override implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V] = _divIntoVV
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def neg: OpNeg.Impl[V, V] = _neg
    override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    override implicit def setIntoVS: OpSet.InPlaceImpl2[V, S] = _setIntoVS
    override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
    override implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals
    override implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals
    override implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals
//    override implicit def powIntoVV: OpPow.InPlaceImpl2[V, V] = _powIntoVV
//    override implicit def powIntoVS: OpPow.InPlaceImpl2[V, S] = _powIntoVS
//    override implicit def modIntoVV: OpMod.InPlaceImpl2[V, V] = _modIntoVV
//    override implicit def modIntoVS: OpMod.InPlaceImpl2[V, S] = _modIntoVS
//    override implicit def powVV: OpPow.Impl2[V, V, V] = _powVV
//    override implicit def powVS: OpPow.Impl2[V, S, V] = _powVS
//    override implicit def modVV: OpMod.Impl2[V, V, V] = _modVV
//    override implicit def modVS: OpMod.Impl2[V, S, V] = _modVS
  }
}

object MutableRestrictedDomainTensorField {
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
                    _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
                    _zeroLike: CanCreateZerosLike[V, V],
                    _zero: CanCreateZeros[V, I],
                    _dim: dim.Impl[V, I],
                    _mulVS: OpMulScalar.Impl2[V, S, V],
                    _divVS: OpDiv.Impl2[V, S, V],
                    _addVV: OpAdd.Impl2[V, V, V],
                    _subVV: OpSub.Impl2[V, V, V],
                    _neg: OpNeg.Impl[V, V],
                    _tabulate: CanTabulate[I,V,S],
//                    _modVV: OpMod.Impl2[V, V, V],
//                    _modVS: OpMod.Impl2[V, S, V],
//                    _powVV: OpPow.Impl2[V, V, V],
//                    _powVS: OpPow.Impl2[V, S, V],
//                    _modIntoVV: OpMod.InPlaceImpl2[V, V],
//                    _modIntoVS: OpMod.InPlaceImpl2[V, S],
//                    _powIntoVV: OpPow.InPlaceImpl2[V, V],
//                    _powIntoVS: OpPow.InPlaceImpl2[V, S],
                    _ops: V <:< NumericOps[V] with QuasiTensor[I, S],
                    _dotVV: OpMulInner.Impl2[V, V, S],
                    _zipMapVals: CanZipMapValues[V, S, S, V],
                    _traverseVals: CanTraverseValues[V, S],
                    _mapVals: CanMapValues[V, S, S, V]
                     ): MutableRestrictedDomainTensorField[V, I, S] = new MutableRestrictedDomainTensorField[V, I, S] {
    def scalars: Field[S] = _field
    override implicit def hasOps(v: V): NumericOps[V] with QuasiTensor[I, S] = _ops(v)
    override implicit def normImpl: norm.Impl[V, Double] = _norm
    override implicit def normImplDouble: norm.Impl2[V, Double, Double] = _norm2
    override implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS
    override implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    override implicit def subVS: OpSub.Impl2[V, S, V] = _subVS
    override implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV
    override implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV
    override implicit def copy: CanCopy[V] = _copy
    override implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    override implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
    override implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    override implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
    override implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S] = _addIntoVS
    override implicit def subIntoVS: OpSub.InPlaceImpl2[V, S] = _subIntoVS
    override implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = _mulIntoVV
    override implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V] = _divIntoVV
    override implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    override implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
    override implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    override implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    override implicit def neg: OpNeg.Impl[V, V] = _neg
    override implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    override implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    override implicit def setIntoVS: OpSet.InPlaceImpl2[V, S] = _setIntoVS
    override implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
    override implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals
    override implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals
    override implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals
    override implicit def zero: CanCreateZeros[V, I] = _zero
    override implicit def canDim: dim.Impl[V, I] = _dim
    override implicit def tabulateTensor: CanTabulate[I, V, S] = _tabulate

    //    override implicit def powIntoVV: OpPow.InPlaceImpl2[V, V] = _powIntoVV
//    override implicit def powIntoVS: OpPow.InPlaceImpl2[V, S] = _powIntoVS
//    override implicit def modIntoVV: OpMod.InPlaceImpl2[V, V] = _modIntoVV
//    override implicit def modIntoVS: OpMod.InPlaceImpl2[V, S] = _modIntoVS
//    override implicit def powVV: OpPow.Impl2[V, V, V] = _powVV
//    override implicit def powVS: OpPow.Impl2[V, S, V] = _powVS
//    override implicit def modVV: OpMod.Impl2[V, V, V] = _modVV
//    override implicit def modVS: OpMod.Impl2[V, S, V] = _modVS
  }
}

object MutableOptimizationSpace {
  object SparseOptimizationSpace {
    import FrobeniusMatrixInnerProductNorms._
    import CSCMatrix.FrobeniusInnerProductCSCMatrixSpace._
    implicit def sparseOptSpace[S:Field:Zero:ClassTag] = make[CSCMatrix[S],SparseVector[S],S](_.asCSCMatrix(),_.flatten())
  }

  object DenseOptimizationSpace {
    import FrobeniusMatrixInnerProductNorms._
    implicit def denseOptSpace[S:Field:ClassTag] = make[DenseMatrix[S],DenseVector[S],S](_.asDenseMatrix,_.flatten())
  }

  def make[M,V,S](toMat: V => M, toVec: M => V)(implicit
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
                  _scaleAddVSV: scaleAdd.InPlaceImpl3[V, S, V],
                  _zeroLike: CanCreateZerosLike[V, V],
                  _zero: CanCreateZeros[V, Int],
                  _dim: dim.Impl[V, Int],
                  _mulVS: OpMulScalar.Impl2[V, S, V],
                  _divVS: OpDiv.Impl2[V, S, V],
                  _addVV: OpAdd.Impl2[V, V, V],
                  _subVV: OpSub.Impl2[V, V, V],
                  _neg: OpNeg.Impl[V, V],
                  _tabulate: CanTabulate[Int,V,S],
                  _ops: V <:< NumericOps[V] with QuasiTensor[Int, S],
                  _dotVV: OpMulInner.Impl2[V, V, S],
                  _zipMapVals: CanZipMapValues[V, S, S, V],
                  _traverseVals: CanTraverseValues[V, S],
                  _mapVals: CanMapValues[V, S, S, V],
                  _norm2M: norm.Impl2[M, Double, Double],
                  _normM: norm.Impl[M, Double],
                  _addMS: OpAdd.Impl2[M, S, M],
                  _subMS: OpSub.Impl2[M, S, M],
                  _mulMM: OpMulScalar.Impl2[M, M, M],
                  _divMM: OpDiv.Impl2[M, M, M],
                  _copyM: CanCopy[M],
                  _mulIntoMS: OpMulScalar.InPlaceImpl2[M, S],
                  _divIntoMS: OpDiv.InPlaceImpl2[M, S],
                  _addIntoMM: OpAdd.InPlaceImpl2[M, M],
                  _subIntoMM: OpSub.InPlaceImpl2[M, M],
                  _addIntoMS: OpAdd.InPlaceImpl2[M, S],
                  _subIntoMS: OpSub.InPlaceImpl2[M, S],
                  _mulIntoMM: OpMulScalar.InPlaceImpl2[M, M],
                  _divIntoMM: OpDiv.InPlaceImpl2[M, M],
                  _setIntoMM: OpSet.InPlaceImpl2[M, M],
                  _setIntoMS: OpSet.InPlaceImpl2[M, S],
                  _scaleAddMSM: scaleAdd.InPlaceImpl3[M, S, M],
                  _zeroLikeM: CanCreateZerosLike[M, M],
                  _zeroM: CanCreateZeros[M, (Int,Int)],
                  _dimM: dim.Impl[M, (Int,Int)],
                  _mulMS: OpMulScalar.Impl2[M, S, M],
                  _divMS: OpDiv.Impl2[M, S, M],
                  _addMM: OpAdd.Impl2[M, M, M],
                  _subMM: OpSub.Impl2[M, M, M],
                  _negM: OpNeg.Impl[M, M],
                  _tabulateM: CanTabulate[(Int,Int),M,S],
                  _opsM: M <:< NumericOps[M] with QuasiTensor[(Int,Int), S],
                  _dotMM: OpMulInner.Impl2[M, M, S],
                  _zipMapValsM: CanZipMapValues[M, S, S, M],
                  _traverseValsM: CanTraverseValues[M, S],
                  _mapValsM: CanMapValues[M, S, S, M],
                  _mulMMM: OpMulMatrix.Impl2[M, M, M],
                  _mulMVV: OpMulMatrix.Impl2[M, V, V],
                  _mulVTM: OpMulMatrix.Impl2[V, Transpose[V], M],
                  _canTrans: CanTranspose[V,Transpose[V]]
    ): MutableOptimizationSpace[M,V,S] =  new MutableOptimizationSpace[M,V,S] {
    def toMatrix(v: V): M = toMat(v)
    def toVector(m: M): V = toVec(m)
    implicit def setIntoMM: OpSet.InPlaceImpl2[M, M] = _setIntoMM
    implicit def divMS: OpDiv.Impl2[M, S, M] = _divMS
    implicit def normMImplDouble: norm.Impl2[M, Double, Double] = _norm2M
    implicit def divMM: OpDiv.Impl2[M, M, M] = _divMM
    implicit def zeroLikeM: CanCreateZerosLike[M, M] = _zeroLikeM
    implicit def scaleAddMM: scaleAdd.InPlaceImpl3[M, S, M] = _scaleAddMSM
    implicit def addIntoMS: OpAdd.InPlaceImpl2[M, S] = _addIntoMS
    implicit def tabulateTensorM: CanTabulate[(Int, Int), M, S] = _tabulateM
    implicit def negM: OpNeg.Impl[M, M] = _negM
    implicit def subIntoMS: OpSub.InPlaceImpl2[M, S] = _subIntoMS
    implicit def addIntoMM: OpAdd.InPlaceImpl2[M, M] = _addIntoMM
    implicit def canDimM: dim.Impl[M, (Int, Int)] = _dimM
    implicit def subIntoMM: OpSub.InPlaceImpl2[M, M] = _subIntoMM
    implicit def copyM: CanCopy[M] = _copyM
    implicit def mulMS: OpMulScalar.Impl2[M, S, M] = _mulMS
    implicit def dotMM: OpMulInner.Impl2[M, M, S] = _dotMM
    implicit def subMS: OpSub.Impl2[M, S, M] = _subMS
    implicit def mulMMS: OpMulScalar.Impl2[M, M, M] = _mulMM
    implicit def mulIntoMS: OpMulScalar.InPlaceImpl2[M, S] = _mulIntoMS
    implicit def subMM: OpSub.Impl2[M, M, M] = _subMM
    implicit def divIntoMM: OpDiv.InPlaceImpl2[M, M] = _divIntoMM
    implicit def mulIntoMM: OpMulScalar.InPlaceImpl2[M, M] = _mulIntoMM
    implicit def hasMOps(v: M): NumericOps[M] with QuasiTensor[(Int, Int), S] = _opsM(v)
    implicit def zeroM: CanCreateZeros[M, (Int, Int)] = _zeroM
    implicit def addMM: OpAdd.Impl2[M, M, M] = _addMM
    implicit def divIntoMS: OpDiv.InPlaceImpl2[M, S] = _divIntoMS
    implicit def addMS: OpAdd.Impl2[M, S, M] = _addMS
    implicit def setIntoMS: OpSet.InPlaceImpl2[M, S] = _setIntoMS
    implicit def hasOps(v: V): NumericOps[V] with QuasiTensor[Int, S] = _ops(v)
    implicit def neg: OpNeg.Impl[V, V] = _neg
    implicit def mulVV: OpMulScalar.Impl2[V, V, V] = _mulVV
    implicit def tabulateTensor: CanTabulate[Int, V, S] = _tabulate
    implicit def canDim: dim.Impl[V, Int] = _dim
    implicit def zero: CanCreateZeros[V, Int] = _zero
    implicit def normImplDouble: norm.Impl2[V, Double, Double] = _norm2
    implicit def divIntoVV: OpDiv.InPlaceImpl2[V, V] = _divIntoVV
    implicit def mulIntoVV: OpMulScalar.InPlaceImpl2[V, V] = _mulIntoVV
    implicit def setIntoVS: OpSet.InPlaceImpl2[V, S] = _setIntoVS
    implicit def zipMapValues: CanZipMapValues[V, S, S, V] = _zipMapVals
    implicit def iterateValues: CanTraverseValues[V, S] = _traverseVals
    implicit def mapValues: CanMapValues[V, S, S, V] = _mapVals
    implicit def divVV: OpDiv.Impl2[V, V, V] = _divVV
    implicit def subVS: OpSub.Impl2[V, S, V] = _subVS
    implicit def subVV: OpSub.Impl2[V, V, V] = _subVV
    implicit def mulVS: OpMulScalar.Impl2[V, S, V] = _mulVS
    implicit def zeroLike: CanCreateZerosLike[V, V] = _zeroLike
    implicit def addVS: OpAdd.Impl2[V, S, V] = _addVS
    implicit def addVV: OpAdd.Impl2[V, V, V] = _addVV
    implicit def scalars: Field[S] = _field
    implicit def divVS: OpDiv.Impl2[V, S, V] = _divVS
    implicit def dotVV: OpMulInner.Impl2[V, V, S] = _dotVV
    implicit def divIntoVS: OpDiv.InPlaceImpl2[V, S] = _divIntoVS
    implicit def addIntoVV: OpAdd.InPlaceImpl2[V, V] = _addIntoVV
    implicit def addIntoVS: OpAdd.InPlaceImpl2[V, S] = _addIntoVS
    implicit def subIntoVS: OpSub.InPlaceImpl2[V, S] = _subIntoVS
    implicit def subIntoVV: OpSub.InPlaceImpl2[V, V] = _subIntoVV
    implicit def mulIntoVS: OpMulScalar.InPlaceImpl2[V, S] = _mulIntoVS
    implicit def copy: CanCopy[V] = _copy
    implicit def setIntoVV: OpSet.InPlaceImpl2[V, V] = _setIntoVV
    implicit def scaleAddVV: scaleAdd.InPlaceImpl3[V, S, V] = _scaleAddVSV
    implicit def mapValuesM: CanMapValues[M, S, S, M] = _mapValsM
    implicit def iterateValuesM: CanTraverseValues[M, S] = _traverseValsM
    implicit def zipMapValuesM: CanZipMapValues[M, S, S, M] = _zipMapValsM
    implicit def mulMMM: OpMulMatrix.Impl2[M, M, M] = _mulMMM
//    implicit def mulMVM: OpMulMatrix.Impl2[M, V, M] = _mulMVM
//    implicit def mulVMM: OpMulMatrix.Impl2[V, M, M] = _mulVMM
    implicit def mulMVV: OpMulMatrix.Impl2[M, V, V] = _mulMVV
    implicit def mulVTM: OpMulMatrix.Impl2[V, Transpose[V], M] = _mulVTM
    implicit def canTrans: CanTranspose[V,Transpose[V]] = _canTrans
  }
}