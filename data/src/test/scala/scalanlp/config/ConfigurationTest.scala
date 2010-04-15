package scalanlp.config

import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

import java.util.Properties;

trait ConfigurationDemoIFace {
  def int:Int;
  def double:Double;
  def boolean:Boolean;
  def string:String;
}
case class DemoConfigurationClass(int: Int, double: Double, boolean: Boolean, string: String) extends ConfigurationDemoIFace;

trait ConfDemoHolderIFace[A] {
  def value: A
}

case class DemoConfHolder[A](value: A) extends ConfDemoHolderIFace[A];

@RunWith(classOf[JUnitRunner])
class ConfigurationTest extends FunSuite {

  test("we can read ints, doubles, booleans, and strings") {
    val p = new Properties();
    p.put("some.int","3");
    p.put("some.double","1E-4");
    p.put("some.boolean","false");
    p.put("some.string","....");
    val reader = Configuration.fromProperties(p);
    val myInt = reader.readIn[Int]("some.int");
    assert(myInt === 3);
    val myDouble = reader.readIn[Double]("some.double");
    assert(myDouble === 1E-4);
    val myBoolean = reader.readIn[Boolean]("some.boolean");
    assert(myBoolean === false);
  }

  test("we can read things embedded in a class") {
    val p = new Properties();
    p.put("some.int","3");
    p.put("some.double","1E-4");
    p.put("some.boolean","false");
    p.put("some.string","....");
    val reader = Configuration.fromProperties(p);
    val my = reader.readIn[DemoConfigurationClass]("some");
    assert(my.int === 3);
    assert(my.double === 1E-4);
    assert(my.boolean === false);
  }

  test("we can read in an interface backed by a class") {
    val p = new Properties();
    p.put("some","scalanlp.config.DemoConfigurationClass");
    p.put("some.int","3");
    p.put("some.double","1E-4");
    p.put("some.boolean","false");
    p.put("some.string","....");
    val reader = Configuration.fromProperties(p);
    val my = reader.readIn[ConfigurationDemoIFace]("some");
    assert(my.int === 3);
    assert(my.double === 1E-4);
    assert(my.boolean === false);
  }

  test("we can read in a class object") {
    val p = new Properties();
    p.put("some","scalanlp.config.DemoConfigurationClass");
    val reader = Configuration.fromProperties(p);
    val my = reader.readIn[Class[ConfigurationDemoIFace]]("some");
  }





  test("we can read a generic with a class inside") {
    val p = new Properties();
    p.put("some.value.int","3");
    p.put("value.double","1E-4");
    p.put("boolean","false");
    p.put("value.string","....");
    val reader = Configuration.fromProperties(p);
    val my = reader.readIn[DemoConfHolder[DemoConfigurationClass]]("some").value;
    assert(my.int === 3);
    assert(my.double === 1E-4);
    assert(my.boolean === false);
  }

  test("we can read a generic with a class inside with a generic iface") {
    val p = new Properties();
    p.put("some","scalanlp.config.DemoConfHolder");
    p.put("some.value.int","3");
    p.put("value.double","1E-4");
    p.put("boolean","false");
    p.put("value.string","....");
    val reader = Configuration.fromProperties(p);
    val my = reader.readIn[ConfDemoHolderIFace[DemoConfigurationClass]]("some").value;
    assert(my.int === 3);
    assert(my.double === 1E-4);
    assert(my.boolean === false);
  }

}
