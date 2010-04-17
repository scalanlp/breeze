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
import org.junit.runner.RunWith
import scalanlp.util.Index;

@RunWith(classOf[JUnitRunner])
class JavaDataSerializationTest extends FunSuite with Checkers {
  import JavaDataSerialization._;

  def basicTest[T:Arbitrary:ReadWritable]() = check( Prop.forAll { (a:T) =>
    val bytes = toBytes[T](a);
    val b = fromBytes[T](bytes);
    a == b
  });

  test("Primitives") {
    basicTest[Int]();
    basicTest[Boolean]();
    basicTest[Double]();
    basicTest[Long]();
    basicTest[String]();
    basicTest[Byte]();
    basicTest[Short]();
    basicTest[Float]();
    basicTest[Char]();
  }

  def tuple2Test[T1:Arbitrary:ReadWritable,T2:Arbitrary:ReadWritable]() = basicTest[(T1,T2)]();
  def tuple3Test[T1:Arbitrary:ReadWritable,T2:Arbitrary:ReadWritable,T3:Arbitrary:ReadWritable]() = basicTest[(T1,T2,T3)]();
  def tuple4Test[T1:Arbitrary:ReadWritable,T2:Arbitrary:ReadWritable,T3:Arbitrary:ReadWritable,T4:Arbitrary:ReadWritable]() = basicTest[(T1,T2,T3,T4)]();

  test("Some primitive tuples") {
    tuple4Test[Int,Boolean,Long,Double]();
    tuple4Test[Int,String,String,Double]();
    tuple4Test[Boolean,String,String,Double]();
    tuple4Test[Char,String,String,Double]();
    tuple4Test[String,String,String,String]();

    tuple3Test[Int,Boolean,Long]();
    tuple3Test[Int,String,String]();
    tuple3Test[Boolean,String,String]();
    tuple3Test[Char,String,String]();
    tuple3Test[String,String,String]();

    tuple2Test[Int,Boolean]();
    tuple2Test[Int,String]();
    tuple2Test[Boolean,String]();
    tuple2Test[Char,String]();
    tuple2Test[String,String]();
  }

  implicit def arbIndex[T:Arbitrary]:Arbitrary[Index[T]] = Arbitrary {
    import Arbitrary.arbitrary;
    for( s <- arbitrary[List[T]]) yield {
      val ind = Index[T]();
      s foreach { ind.index _ };
      ind:Index[T]
    };
  }

  def indexTest[T:ReadWritable:Arbitrary]() = {
    basicTest[Index[T]]();
    tuple2Test[Index[T],Index[T]]()
  };

  test("Index") {
    indexTest[String]();
    indexTest[Int]();
    indexTest[(String,String)]();
  }

}