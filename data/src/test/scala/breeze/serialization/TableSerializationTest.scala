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
package breeze.serialization

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.scalacheck.util._;
import org.junit.runner.RunWith;

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

  test("Options") {
    val values : List[(String,Option[Int])] = List(("a",Some(2)), ("b",None));
    val sb = new StringBuilder();
    CSVTableSerialization.write(sb, values);
    
    assert(sb.toString.trim === "a,2\nb,");
    
    assert(CSVTableSerialization.read[List[(String,Option[Int])]](sb.toString) === values);
  }

  test("Embedded Tuples") {
    val values : List[((Int,String),(Double,Double))] =
      List(((1,"a"),(3.0,4.0)),
           ((2,"b"),(1.0,2.0)));
    val sb = new StringBuilder();
    CSVTableSerialization.write(sb, values);

    assert(sb.toString.trim === "1,a,3.0,4.0\n2,b,1.0,2.0");

    assert(CSVTableSerialization.read[List[((Int,String),(Double,Double))]](sb.toString) === values);
  }

  test("Headers") {
    val values : List[(String,Option[Int])] = List(("a",Some(2)), ("b",None));
    val sb = new StringBuilder();
    CSVTableSerialization.write(sb, values, List("Column 1", "Column 2"));
    
    assert(sb.toString.trim === "Column 1,Column 2\na,2\nb,");
  }

  //
  // TODO: work out why this gives a diverging implicit expansion error
  //

  test("RowCompanion") {
    val values = List(
      ExampleRow("a",13,Array(1.0,2.0)),
      ExampleRow("b",27,Array(2.0,3.0)));

    val sb = new StringBuilder();
    CSVTableSerialization.write(sb, values);

    assert(sb.toString.trim === "id,count,values\na,13,1.0,2.0\nb,27,2.0,3.0");
  }
}

case class ExampleRow(id : String, count : Int, values : Array[Double]);
object ExampleRow extends TableRowCompanion[ExampleRow,(String,Int,Array[Double])];
