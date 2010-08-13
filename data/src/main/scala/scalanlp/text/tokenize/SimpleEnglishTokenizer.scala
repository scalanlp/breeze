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
package scalanlp.text.tokenize

import scalanlp.serialization.{SubtypedCompanion,TypedCompanion0};

/**
 * Simple English document tokenizer pre-processor based on regular
 * expressions from Steven Bethard.
 * 
 * Because this class may improve over time in non-backwards-compatible ways,
 * the default behavior of SimpleEnglishTokenizer.apply() is to return an
 * instance of SimpleEnglishTokenizer.V0;
 *
 * @author dramage
 */
trait SimpleEnglishTokenizer extends Tokenizer;

object SimpleEnglishTokenizer extends SubtypedCompanion[SimpleEnglishTokenizer] {
  prepare();

  for (cc <- List(this, Tokenizer)) {
    cc.register[V0]("SimpleEnglishTokenizer.V0");
  }

  def apply() = V0();

  /** Version 0 of the SimpleEnglishTokenizer. */
  class V0 extends SimpleEnglishTokenizer {
    override def apply(in : String) : Iterable[String] = {
      var string = in;
      string = V0.r1.replaceAllIn(string, "");
      string = V0.r2.replaceAllIn(string, "$1 ");
      string = V0.r3.replaceAllIn(string, " $1");
      string.split("\\s+");
    }
  }

  object V0 extends TypedCompanion0[V0] {
    prepare();

    // delete word-final hyphens when followed by newlines
    val r1 = "(?<=\\w)-\\s*\n\\s*".r;

    // add spaces around non-word-internal punctuation
    val r2 = "(?<=\\W)(\\p{P})(?! )".r;
    val r3 = "(?! )(\\p{P})(?=\\W)".r;

    private val _instance = new V0();
    def apply() = _instance;

    override def name = "SimpleEnglishTokenizer.V0"
  }
}

