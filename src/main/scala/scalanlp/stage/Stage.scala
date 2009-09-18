/*
 Copyright 2009 David Hall, Daniel Ramage

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
package scalanlp.stage;

import scala.reflect.Manifest;

import scalanlp.collection.immutable.HTMap;

/**
 * A pipeline consists of a series of stages that each operate on a
 * parcel, possibly updating its metdata and changing its data type.
 * See classes like Mapper and MetaBuilder for more concrete (but still
 * abstract) instantiations of how to use Stages to accomplish particular
 * tasks.
 * 
 * @author dramage
 */
abstract class Stage[InputMeta,OutputMeta,InputData,OutputData]
(implicit mMI : Manifest[InputMeta], mMO : Manifest[OutputMeta],
          mID : Manifest[InputData], mIO : Manifest[OutputData]) {
  
  /** The primary abstract method of a stage: transforms a parcel. */
  def process[M<:InputMeta](parcel : Parcel[M,InputData])
  : Parcel[M with OutputMeta, OutputData];

  /**
   * Open composition -- composes this stage with another stage that
   * requires at least some metadata to be provided by some earlier stage.
   */
  def |> [IM2>:InputMeta with OutputMeta,OM2,NextData](next : Stage[IM2,OM2,OutputData,NextData])
  (implicit im2 : Manifest[IM2], om2 : Manifest[OM2], nm : Manifest[NextData]) =
    Stage.ComposeOpen(this, next);
  
  /**
   * Closed composition -- composes this stage with another stage that
   * requires metadata that this stage has generated.
   */
  def |< [IM2>:InputMeta with OutputMeta,OM2,NextData](next : Stage[IM2,OM2,OutputData,NextData])
  (implicit im2 : Manifest[IM2], om2 : Manifest[OM2], nm : Manifest[NextData]) =
    Stage.ComposeClosed(this, next);
  
  /**
   * Fixed composition -- composes this stage with another stage
   * where both stages have the same metadta.
   */
  def |= [NextData](next : Stage[OutputMeta,OutputMeta,OutputData,NextData])
  (implicit nm : Manifest[NextData]) =
    Stage.ComposeFixed(this, next);
}

/**
 * Companion object to Stage provdies default stage implementations and
 * simple static constructors for making stages from functions.
 * 
 * @author dramage
 */
object Stage {
  /**
   * Composes two stages, where the second stage requires some metadata
   * not provided by the first stage.
   * 
   * @author dramage
   */
  case class ComposeOpen[IM1,OM1,IM2>:IM1 with OM1,OM2,I,X,O]
  (stage1 : Stage[IM1,OM1,I,X], stage2 : Stage[IM2,OM2,X,O])
  (implicit mIm1 : Manifest[IM1], mOm1 : Manifest[OM1],
   mIm2 : Manifest[IM2], mOm2 : Manifest[OM2],
   mI : Manifest[I], mO : Manifest[O])
  extends Stage[IM1 with IM2, OM1 with OM2, I, O] {
    override def process[M<:IM1 with IM2](parcel : Parcel[M,I]) =
      stage2.process(stage1.process(parcel));
  }
  
  /**
   * Composes two stages, where the second stage requires metadata produced
   * by the first stage.
   * 
   * @author dramage
   */
  case class ComposeClosed[IM1,OM1,IM2>:IM1 with OM1,OM2,I,X,O]
  (stage1 : Stage[IM1,OM1,I,X], stage2 : Stage[IM2,OM2,X,O])
  (implicit mIm1 : Manifest[IM1], mOm1 : Manifest[OM1],
   mIm2 : Manifest[IM2], mOm2 : Manifest[OM2],
   mI : Manifest[I], mO : Manifest[O])
  extends Stage[IM1, OM1 with OM2, I, O] {
    override def process[M<:IM1](parcel : Parcel[M,I]) =
      stage2.process(stage1.process(parcel));
  }
  
  /**
   * Composes two stages, where the second stage does not alter the metadata.
   * 
   * @author dramage
   */
  case class ComposeFixed[IM,OM,I,X,O]
  (stage1 : Stage[IM,OM,I,X], stage2 : Stage[OM,OM,X,O])
  (implicit mIm : Manifest[IM], mOm : Manifest[OM], mI : Manifest[I], mO : Manifest[O])
  extends Stage[IM, OM, I, O] {
    override def process[M<:IM](parcel : Parcel[M,I]) =
      stage2.process(stage1.process(parcel));
  }
  
  /**
    * A mapper is a stage that transforms the data from an Iterable[I] to an
    * Iterable[O] but adds no metadata.  See the MapperN variants for mappers
    * that can read metadata during the mapping process.
    * 
    * @author dramage
    */
  abstract class FunctionWithoutMeta[I,O]
  (implicit mI : Manifest[I], mO : Manifest[O])
  extends Stage[Any,Any,I,O] {
    /** Transforms the input data without using metadata. */
    def apply(data : I) : O;

    /** Calls map. */
    override def process[M](parcel : Parcel[M,I]) =
      parcel.withHistory(this).map(apply _);
  }
  
  /** Transforms the input data using one piece of metadata. */
  abstract class FunctionWithMeta1[I,O,M1]
  (implicit m1 : Manifest[M1], mI : Manifest[I], mO : Manifest[O])
  extends Stage[M1,M1,I,O] {
    def apply(meta : M1)(data : I) : O;
      
    override def process[M <: M1](parcel : Parcel[M,I]) : Parcel[M,O] =
      parcel.withHistory(this).map(apply(parcel.meta[M1]) _);
  }

  /** Transforms the input data using two pieces of metadata. */
  abstract class FunctionWithMeta2[I,O,M1,M2]
  (implicit mm1 : Manifest[M1], mm2 : Manifest[M2], mI : Manifest[I], mO : Manifest[O])
  extends Stage[M1 with M2, M1 with M2, I, O] {
    def apply(m1 : M1, m2 : M2)(data : I) : O;
      
    override def process[M <: M1 with M2](parcel : Parcel[M,I]) : Parcel[M,O] =
      parcel.withHistory(this).map(apply(parcel.meta[M1], parcel.meta[M2]) _);
  }

  /** Construct a stage from a parcel mapping function. */
  def apply[IM,OM,I,O](f : ((Parcel[IM,I]) => (Manifest[IM], Manifest[OM]) => Parcel[OM,O]))
  (implicit im : Manifest[IM], om : Manifest[OM], mI : Manifest[I], mO : Manifest[O]) = {
    new Stage[IM,OM,I,O] {
      override def process[M<:IM](parcel : Parcel[M,I]) =
        f(parcel.asInstanceOf[Parcel[IM,I]])(im,om).asInstanceOf[Parcel[M with OM,O]];
    }
  }
  
  /** Construct a stage from an arbitrary function with no metadata. */
  def apply[I,O](f : I => O)(implicit mI : Manifest[I], mO : Manifest[O]) =
    new FunctionWithoutMeta[I,O] {
      override def apply(data : I) = f(data); }
  
  /** Construct a stage from an arbitrary function with one piece of metadata. */
  def apply[I,O,M1](f : M1 => I => O)
  (implicit mm1 : Manifest[M1], mI : Manifest[I], mO : Manifest[O]) =
    new FunctionWithMeta1[I,O,M1] {
      override def apply(m1 : M1)(data : I) : O = f(m1)(data); }
  
  /** Construct a stage from an arbitrary function with two pieces of metadata. */
  def apply[I,O,M1,M2](f : (M1,M2) => I => O)
  (implicit mm1 : Manifest[M1], mm2 : Manifest[M2], mI : Manifest[I], mO : Manifest[O]) =
    new FunctionWithMeta2[I,O,M1,M2] {
      override def apply(m1 : M1, m2 : M2)(data : I) : O = f(m1,m2)(data); }
}


/**
 * Builds metadata statistics from the given data.
 * 
 * @author dramage
 */
abstract class MetaBuilder[OutputMeta,Data]
(implicit mO : Manifest[OutputMeta], mD : Manifest[Data])
extends Stage[Any,Any with OutputMeta,Data,Data] {
  /** Builds new metadata for the Parcel as a function of the data. */
  def build(data : Data) : OutputMeta;

  /** Calls build, adding the returned metadata to the parcel. */
  override def process[M](parcel : Parcel[M,Data])
  : Parcel[M with OutputMeta, Data] = {
    // for some reason this has to be assigned to a val before returning
    val rv = parcel.withHistory(this).withMeta(build(parcel.data));
    rv;
  }
}
