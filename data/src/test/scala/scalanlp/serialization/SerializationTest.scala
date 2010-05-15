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

/**
 * Abstract test trait for serializers that support primitive and compound
 * types.
 *
 * @author dlwh
 * @author dramage
 */
trait SerializationTestBase extends FunSuite with Checkers {
  val serializer : ByteSerialization with SerializationFormat.PrimitiveTypes with SerializationFormat.CompoundTypes;

  import serializer._;

  implicit val arbString = Arbitrary(Gen.alphaStr);
  implicit val arbChar = Arbitrary(Gen.alphaChar);


  def basicTest[T:Arbitrary:ReadWritable]() = check( Prop.forAll { (a:T) =>
    val bytes = serializer.toBytes[T](a);
    val b = serializer.fromBytes[T](bytes);
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

  test("Compound tuples") {
    tuple2Test[Int,List[Int]]();
    tuple2Test[List[Int],String]();
    tuple2Test[List[List[String]],Map[String,Int]]();
    tuple2Test[List[List[String]],Map[(Int,String,Int),(String,Double)]]();
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

@RunWith(classOf[JUnitRunner])
class DataSerializationTest extends SerializationTestBase {
  override val serializer = DataSerialization;
}

@RunWith(classOf[JUnitRunner])
class TextSerializationTest extends SerializationTestBase {
  override val serializer = TextSerialization;
}

@RunWith(classOf[JUnitRunner])
class FileSerializationTest extends FunSuite {
  test("Read/write file") {
    val file = java.io.File.createTempFile("scalanlp-file-serialization-", ".txt");
      import FileSerialization.FromText._;
    FileSerialization.write(file, List(1,2,3));
    assert(FileSerialization.read[List[Int]](file) === List(1,2,3));
    file.delete();
  }
}
