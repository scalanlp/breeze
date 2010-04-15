package scalanlp.data.process;

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

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class PTBTokenizerTest extends FunSuite with Checkers {
  import Arbitrary.arbitrary;
  
  private def isOneToken(w: String) = {
    w === PTBTokenizer(w).left.get(0)
  }
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
      assert(PTBTokenizer(s).left.get === toks)
    }
  }

  test("quotes") {
    val sents = Map("\"Hi there\"" -> List("\"","Hi","there","\""),
      "\"Hi there.\"" -> List("\"","Hi","there",".","\""));
    for( (s,toks) <- sents) {
      assert(PTBTokenizer(s).left.get === toks);
    }
  }

  test("contractions") {
    val sents = Map(//"didn't" -> List("did","n't"),
      "ya'll" -> List("ya","'ll"),
      "we're" -> List("we","'re"),
      "we've" -> List("we","'ve"),
      "YA'LL" -> List("YA","'LL"),
      "WE'RE" -> List("WE","'RE"),
      "WE'VE" -> List("WE","'VE"),
      "I'm" -> List("I","'m"),
      "He's" -> List("He","'s")
    );
    for( (s,toks) <- sents) {
      assert(PTBTokenizer(s).left.get === toks);
    }
  }

  test("moneys") {
    val words = List("99","$99","$99.33");
    for(w <- words) {
      assert(isOneToken(w))
    }
    
  }

  test("special words") {
    val words = Map("cannot" -> List("can","not"),
        "d'ye"-> List("d'","ye"),
        "gimme"->List("gim","me"),
        "gonna"->List("gon","na"),
        "gotta"->List("got","ta"),
        "Lemme"->List("Lem","me"),
        "more'n"->List("more","'n"),
        "'tis"->List("'t","is"),
        "'Tis"->List("'T","is"),
        "wanna"->List("wan","na"),
        "Whaddya"->List("Wha","dd","ya"),
        "Whatcha"->List("Wha","t","cha")
    );
    for( (s,toks) <- words) {
      assert(PTBTokenizer(s).left.get === toks);
    }

  }
}
