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
package text;
package tokenize;

import breeze.serialization.TypedCompanion1;

/**
 * Filters out tokens composed of fewer than minLength characters.
 *
 * @author dramage
 */
case class MinimumLengthFilter(minLength : Int) extends Transformer {
  override def apply(doc : Iterable[String]) : Iterable[String] =
    doc.filter(token => token.length >= minLength);
}

object MinimumLengthFilter extends TypedCompanion1[Int,MinimumLengthFilter] {
  prepare();
}
