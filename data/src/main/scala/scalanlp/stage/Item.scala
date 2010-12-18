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
package scalanlp;
package stage;

import serialization.DataSerialization;

/**
 * An item represents a single item corresponding to the
 * given numbered item from the origin.
 *
 * @param number The number of this item.
 * @param value The value of this item.
 *
 * @author dramage
 */
case class Item[ID,+V](id : ID, value : V) {
  def map[O](f : V => O) =
    Item[ID,O](id, f(value));
}

object Item {
  implicit def dataReadable[ID:DataSerialization.Readable,V:DataSerialization.Readable]
  : DataSerialization.Readable[Item[ID,V]]
  = new DataSerialization.Readable[Item[ID,V]] {
    override def read(from : DataSerialization.Input) = {
      Item(implicitly[DataSerialization.Readable[ID]].read(from),
           implicitly[DataSerialization.Readable[V]].read(from));
    }
  }

  implicit def dataWritable[ID:DataSerialization.Writable,V:DataSerialization.Writable]
  : DataSerialization.Writable[Item[ID,V]]
  = new DataSerialization.Writable[Item[ID,V]] {
    override def write(to : DataSerialization.Output, value : Item[ID,V]) = {
      implicitly[DataSerialization.Writable[ID]].write(to, value.id);
      implicitly[DataSerialization.Writable[V]].write(to, value.value);
    }
  }
}