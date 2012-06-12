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
package breeze;
package stage;

import java.io.File;

import breeze.ra.Cell;
import breeze.serialization.{TextSerialization, FileSerialization};

/**
 * Cache a batch to a compact binary representation on disk.
 *
 * @author dramage
 */
case class Cache[I:Manifest:FileSerialization.Readable:FileSerialization.Writable]() extends Stage[I,I] {
  override def apply(parcel : Parcel[I]) : Parcel[I] = {
    val path = parcel.meta[File].getName + ".cache." + parcel.history.signature+".dat";

    // first, make sure we have created the cache on disk
    Cell.cache(path)(parcel.data);

    // then, cache it again to make sure we use the one loaded from disk
    Parcel(parcel.history, parcel.meta, Cell.cache(path)(parcel.data));
  }
  
  override def toString = "Cache()";
}
