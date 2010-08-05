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

package scalanlp.util;

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith;

import scalanlp.serialization.FileSerialization;

@RunWith(classOf[JUnitRunner])
class IndexTest extends FunSuite with Checkers {

  test("Index serialization") {
    val tmpFile = java.io.File.createTempFile("index-", ".txt");
    val index = Index(List("a","b","c","d"));
    FileSerialization.write(tmpFile, index);
    assert(FileSerialization.read[Index[String]](tmpFile) == index);
    tmpFile.delete();
  }
}
