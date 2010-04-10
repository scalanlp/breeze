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

import scalanlp.counters.Counters._;
import scala.io.Source;

@RunWith(classOf[JUnitRunner])
class PorterStemmerTest extends FunSuite with Checkers {
  test("massive vocabulary test") {
    val wStream = this.getClass.getClassLoader.getResourceAsStream("porter-voc.txt");
    val words = Source.fromInputStream(wStream).getLines();
    val sStream = this.getClass.getClassLoader.getResourceAsStream("porter-stem.txt");
    val stems = Source.fromInputStream(sStream).getLines();
    try {
      for( (w,s) <- words zip stems) {
        expect(s,w)(PorterStemmer(w));
      }
    } finally {
      wStream.close();
      sStream.close();
    }
  }
}
