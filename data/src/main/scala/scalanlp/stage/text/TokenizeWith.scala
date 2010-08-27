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
package scalanlp.stage.text;

import scalanlp.collection.LazyIterable;
import scalanlp.stage.{Parcel,Stage,Item};
import scalanlp.stage.Stage._;

import scalanlp.text.tokenize.Tokenizer;
import scalanlp.serialization.TypedCompanion1;

/**
 * Tokenizes the given input documents according to the given tokenizer.
 *
 * @author dramage
 */
case class TokenizeWith(tokenizer : Tokenizer)
extends Stage[LazyIterable[Item[String]],LazyIterable[Item[Iterable[String]]]] {
  override def apply(parcel : Parcel[LazyIterable[Item[String]]]) : Parcel[LazyIterable[Item[Iterable[String]]]] =
    Parcel(parcel.history + this, parcel.meta + this, parcel.data.map(_.map(tokenizer)));

  override def toString =
    scalanlp.serialization.TextSerialization.toString(this);
}

object TokenizeWith extends TypedCompanion1[Tokenizer,TokenizeWith] {
  prepare();
}
