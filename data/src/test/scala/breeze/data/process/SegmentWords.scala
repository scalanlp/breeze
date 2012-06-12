package breeze.data.process
/*
 Copyright 2010 David Hall, Daniel Ramage

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
class SegmentWordsTest extends FunSuite {
  val text = """But, in a larger sense, we can not dedicate -- we can not consecrate -- we can not hallow -- this ground.""";

  test("Gettysburg address") {
    val words = SegmentWords(text).toSeq;
    assert(words.length === 28,words);
    assert(words.startsWith(Seq("But",",","in","a","larger","sense",",","we","can","not","dedicate","-")));
  }
}
