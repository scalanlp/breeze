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
package scalanlp.serialization

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.scalacheck.util._;
import org.junit.runner.RunWith;
import scalanlp.util.Index;

///**
// * Abstract test trait for table serializers.
// *
// * @author dramage
// */
//trait TableSerializationTestBase extends FunSuite with Checkers {
//
//  implicit val arbString = Arbitrary(Gen.alphaStr);
//  implicit val arbChar = Arbitrary(Gen.alphaChar);
//
//  def basicTest[T:Arbitrary:TableRowReadable:TableRowWritable]() = {
//    check( Prop.forAll { (a:T) =>
//      val bytes = TableRowSerialization.;
//      val b = serializer.fromBytes[T](bytes);
//      a == b
//    });
//  }
//
//  test("Primitives") {
//    basicTest[Int]();
//    basicTest[Boolean]();
//    basicTest[Double]();
//    basicTest[Long]();
//    basicTest[String]();
//    basicTest[Byte]();
//    basicTest[Short]();
//    basicTest[Float]();
//    basicTest[Char]();
//  }
//
//}


@RunWith(classOf[JUnitRunner])
class TableSerializationTest extends FunSuite with Checkers {
  def checkCSV[V:TableReadable:TableWritable](value : V) = {
    val sb = new StringBuilder();
    CSVTableSerialization.write(sb, value);
    val sv = sb.toString;

    assert(CSVTableSerialization.read[V](sv) === value);
  }

  test("CSV") {
    checkCSV(List(List(1,2,3),List(4,5,6)));
    checkCSV(List(("a","b"),("c","d")));
    checkCSV(List((1,List("a\n\"b","\r\n\"bb")),(2,List("c,","d"))));
  }
}
