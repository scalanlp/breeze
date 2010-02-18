/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package scalanlp.config
import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import scalanlp.counters._;
import Counters._;
import org.junit.runner.RunWith

import java.util.Properties;

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

  case class TestConf(int: Int, double: Double, boolean: Boolean, string: String);

  test("we can read things embedded in a class") {
    val p = new Properties();
    p.put("some.int","3");
    p.put("some.double","1E-4");
    p.put("some.boolean","false");
    p.put("some.string","....");
    val reader = Configuration.fromProperties(p);
    val my = reader.readIn[TestConf]("some");
    assert(my.int === 3);
    assert(my.double === 1E-4);
    assert(my.boolean === false);
  }

  case class Holder[A](value: A);

  test("we can read a generic with a class inside") {
    val p = new Properties();
    p.put("some.value.int","3");
    p.put("value.double","1E-4");
    p.put("boolean","false");
    p.put("value.string","....");
    val reader = Configuration.fromProperties(p);
    val my = reader.readIn[Holder[TestConf]]("some").value;
    assert(my.int === 3);
    assert(my.double === 1E-4);
    assert(my.boolean === false);
  }

}
