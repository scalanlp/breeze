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
package text;

import scalanlp.serialization.TextSerialization;
import scalanlp.collection.LazyIterable;

import scalanlp.text.tokenize.Tokenizer;

/**
 * Tokenizes the given input documents according to the given tokenizer.
 *
 * @author dramage
 */
case class TokenizeWith[ID:Manifest:TextSerialization.Writable](tokenizer : Tokenizer)
extends Stage[LazyIterable[Item[ID,String]],LazyIterable[Item[ID,Iterable[String]]]] {
  override def apply(parcel : Parcel[LazyIterable[Item[ID,String]]]) : Parcel[LazyIterable[Item[ID,Iterable[String]]]] =
    Parcel(parcel.history + this, parcel.meta + tokenizer, parcel.data.map(_.map(tokenizer)));

  override def toString =
    "TokenizeWith(" + scalanlp.serialization.TextSerialization.toString(tokenizer) + ")";
}
