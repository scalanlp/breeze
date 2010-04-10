package scalanlp.io

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith
import scalanlp.util.Index;

@RunWith(classOf[JUnitRunner])
class SerializationTest extends FunSuite with Checkers {
  import Serialization._;
  import Handlers._;
  import ScalanlpHandlers._;

  def basicTest[T:Arbitrary:Handler]() = check( Prop.forAll { (a:T) =>
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

  def tuple2Test[T1:Arbitrary:Handler,T2:Arbitrary:Handler]() = basicTest[(T1,T2)]();
  def tuple3Test[T1:Arbitrary:Handler,T2:Arbitrary:Handler,T3:Arbitrary:Handler]() = basicTest[(T1,T2,T3)]();
  def tuple4Test[T1:Arbitrary:Handler,T2:Arbitrary:Handler,T3:Arbitrary:Handler,T4:Arbitrary:Handler]() = basicTest[(T1,T2,T3,T4)]();

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

  def indexTest[T:Handler:Arbitrary]() = { 
    basicTest[Index[T]]();
    tuple2Test[Index[T],Index[T]]()
  };

  test("Index") {
    indexTest[String]();
    indexTest[Int]();
    indexTest[(String,String)]();
  }

}