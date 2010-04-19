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
package scalanlp.serialization;

import org.scalatest.FunSuite;
import org.scalatest.junit.JUnitRunner;
import org.junit.runner.RunWith;

package typedexamples {
  case class MyCaseClass1(b : String);
  object MyCaseClass1 extends TypedCaseCompanion1[String,MyCaseClass1] {
    prepare();
  }

  case class MyCaseClass2(a : Int, b : String)
  object MyCaseClass2 extends TypedCaseCompanion2[Int,String,MyCaseClass2] {
    prepare();
  }

  case class MyCompoundCaseClass1(a : (Int,Double));
  object MyCompoundCaseClass1 extends TypedCaseCompanion1[(Int,Double),MyCompoundCaseClass1] {
    prepare();
  }

  case class MyCompoundCaseClass2(a : Int, b : MyCaseClass2)
  object MyCompoundCaseClass2 extends TypedCaseCompanion2[Int,MyCaseClass2,MyCompoundCaseClass2] {
    prepare();
  }

  case class MyCompoundCaseClass3(a : List[Double]);
  object MyCompoundCaseClass3 extends TypedCaseCompanion1[List[Double],MyCompoundCaseClass3] {
    prepare();
  }
}

@RunWith(classOf[JUnitRunner])
class TypedCaseCompanionTest extends FunSuite {
  import typedexamples._;

  def loop[T:StringSerialization.ReadWritable](value : T) = {
    val string = StringSerialization.toString(value);
    val parsed = StringSerialization.fromString[T](string);
    assert(parsed === value, "Failure on "+string);
  }

  test("Check case class to and from string") {
    loop(MyCaseClass1("hi"));
    loop(MyCaseClass2(1,"hi"));
    loop(MyCompoundCaseClass1((1,2.0)));
    loop(MyCompoundCaseClass2(1,MyCaseClass2(2,"yo")));
    loop(MyCompoundCaseClass3(List(1.0,2.0)));
  }
}
