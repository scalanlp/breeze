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

package scalanlp.text.tokenize;

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class SimpleEnglishTokenizerTest extends FunSuite with Checkers {
  import Arbitrary.arbitrary;

  val tokenizer = SimpleEnglishTokenizer.V1();

  private def isOneToken(w: String) =
    w === tokenizer(w).head;
  
  test("simple words") {
    val words = List("Hi","there","pilgrim","happy","Thanksgiving","there");
    for(w <- words) {
      assert(isOneToken(w));
    }
  }

  test("some symbols") {
    val words = List(".","...","$","-","/");
    for(w <- words) {
      assert(isOneToken(w))
    }
  }

  test("simple sentences") {
    val sents = Map( "Every good boy does fine." -> List("Every","good","boy","does","fine","."),
      "Hi there, pilgrim; happy Thanksgiving there, pilgrim?" -> List("Hi","there",",","pilgrim",";","happy","Thanksgiving","there",",","pilgrim","?"),
      "Hi there, pilgrim; happy Thanksgiving there, pilgrim!" -> List("Hi","there",",","pilgrim",";","happy","Thanksgiving","there",",","pilgrim","!")
    );
    for( (s,toks) <- sents) {
      assert(tokenizer(s).toList === toks)
    }
  }

  test("quotes") {
    val sents = Map("\"Hi there\"" -> List("\"","Hi","there","\""),
      "\"Hi there.\"" -> List("\"","Hi","there",".\""));
    for( (s,toks) <- sents) {
      assert(tokenizer(s).toList === toks);
    }
  }

  test("word-internal punctuation") {
    val terms = List(
      "didn't",
      "ya'll",
      "we're",
      "we've",
      "YA'LL",
      "WE'RE",
      "WE'VE",
      "I'm",
      "He's"
    );

    for(s <- terms) {
      assert(tokenizer(s).toList === List(s));
    }
  }
}
