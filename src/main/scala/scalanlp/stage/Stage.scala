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

/**
 * A pipeline consists of a series of stages that each operate on a
 * parcel, possibly updating its metadata and changing its data type.
 * See classes like Mapper and MetaBuilder for more concrete (but still
 * abstract) instantiations of how to use Stages to accomplish particular
 * tasks.
 * 
 * @author dramage
 */
abstract class Stage[I,O] extends (Parcel[I] => Parcel[O]) { stage =>
  def ~>[X](g : Parcel[O]=>Parcel[X]) = new Stage[I,X] {
    override def apply(parcel : Parcel[I]) : Parcel[X] = g(stage(parcel));
  }
}

/**
 * Companion object to Stage provdies default stage implementations and
 * simple static constructors for making stages from functions.
 * 
 * @author dramage
 */
object Stage {
  implicit def apply[I,O](f : I=>O)(implicit m : Manifest[O]) : Stage[I,O] =
    apply(f.toString)(f);

  implicit def apply[I,O](name : String)(f : I=>O)(implicit m : Manifest[O]) = new Stage[I,O] {
    override def apply(parcel : Parcel[I]) : Parcel[O] =
      Parcel(parcel.history + this, parcel.meta, f(parcel.data));

    override def toString =
      name;
  }
}


/**
 * Builds metadata statistics from the given data.
 * 
 * @author dramage
 */
abstract class MetaBuilder[Meta,Data](implicit m : Manifest[Meta], mD : Manifest[Data]) extends Stage[Data,Data] {
  /** Builds new metadata for the Parcel as a function of the data. */
  def build(data : Data) : Meta;

  /** Calls build, adding the returned metadata to the parcel. */
  override def apply(parcel : Parcel[Data]) : Parcel[Data] =
    Parcel(parcel.history + this, parcel.meta + build(parcel.data), parcel.data);
}
