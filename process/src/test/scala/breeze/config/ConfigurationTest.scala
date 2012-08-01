package breeze.config

import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

import java.util.Properties

trait ConfigurationDemoIFace {
  def int:Int
  def double:Double
  def boolean:Boolean
  def string:String
}
case class DemoConfigurationClass(int: Int, double: Double, boolean: Boolean, string: String= "woowoo") extends ConfigurationDemoIFace

trait ConfDemoHolderIFace[A] {
  def value: A
}

case class DemoConfHolder[A](value: A) extends ConfDemoHolderIFace[A]
class SubDemoConfHolder[A](value: A) extends DemoConfHolder(value)

case class DemoConfigurationSeq(arr: Array[Int], seq: Seq[String])

@RunWith(classOf[JUnitRunner])
class ConfigurationTest extends FunSuite {

  test("we can read ints, doubles, booleans, and strings") {
    val p = new Properties()
    p.put("some.int","3")
    p.put("some.double","1E-4")
    p.put("some.boolean","false")
    p.put("some.string","....")
    val reader = Configuration.fromProperties(p)
    val myInt = reader.readIn[Int]("some.int")
    assert(myInt === 3)
    val myDouble = reader.readIn[Double]("some.double")
    assert(myDouble === 1E-4)
    val myBoolean = reader.readIn[Boolean]("some.boolean")
    assert(myBoolean === false)
  }

   test("we can read things embedded in a class") {
    val p = new Properties()
    p.put("some.int","3")
    p.put("some.double","1E-4")
    p.put("some.boolean","false")
    p.put("some.string","....")
    val reader = Configuration.fromProperties(p)
    val my = reader.readIn[DemoConfigurationClass]("some")
    assert(my.int === 3)
    assert(my.double === 1E-4)
    assert(my.boolean === false)
  }

  test("we can read things embedded in a class with defaults") {
    val p = new Properties()
    p.put("some.int","3")
    p.put("some.double","1E-4")
    p.put("some.boolean","false")
    val reader = Configuration.fromProperties(p)
    val my = reader.readIn[DemoConfigurationClass]("some")
    assert(my.int === 3)
    assert(my.double === 1E-4)
    assert(my.boolean === false)
    assert(my.string === "woowoo")
  }

  test("we can read in an interface backed by a class") {
    val p = new Properties()
    p.put("some","breeze.config.DemoConfigurationClass")
    p.put("some.int","3")
    p.put("some.double","1E-4")
    p.put("some.boolean","false")
    p.put("some.string","....")
    val reader = Configuration.fromProperties(p)
    val my = reader.readIn[ConfigurationDemoIFace]("some")
    assert(my.int === 3)
    assert(my.double === 1E-4)
    assert(my.boolean === false)
  }

  test("we can read in a class object") {
    val p = new Properties()
    p.put("some","breeze.config.DemoConfigurationClass")
    val reader = Configuration.fromProperties(p)
    val my = reader.readIn[Class[ConfigurationDemoIFace]]("some")
  }

  test("we can read a generic with a class inside") {
    val p = new Properties()
    p.put("some.value.int","3")
    p.put("value.double","1E-4")
    p.put("boolean","false")
    p.put("value.string","....")
    val reader = Configuration.fromProperties(p)
    val my = reader.readIn[DemoConfHolder[DemoConfigurationClass]]("some").value
    assert(my.int === 3)
    assert(my.double === 1E-4)
    assert(my.boolean === false)
  }

  test("we can read a generic with a class inside with a generic iface") {
    val p = new Properties()
    p.put("some","breeze.config.DemoConfHolder")
    p.put("some.value.int","3")
    p.put("value.double","1E-4")
    p.put("boolean","false")
    p.put("value.string","....")
    val reader = Configuration.fromProperties(p)
    val my = reader.readIn[ConfDemoHolderIFace[DemoConfigurationClass]]("some").value
    assert(my.int === 3)
    assert(my.double === 1E-4)
    assert(my.boolean === false)
  }

  test("we can read a subclass of a generic with a class inside with a generic iface") {
    val p = new Properties()
    p.put("some","breeze.config.SubDemoConfHolder")
    p.put("some.value.int","3")
    p.put("value.double","1E-4")
    p.put("boolean","false")
    p.put("value.string","....")
    val reader = Configuration.fromProperties(p)
    val my = reader.readIn[ConfDemoHolderIFace[DemoConfigurationClass]]("some").value
    assert(my.int === 3)
    assert(my.double === 1E-4)
    assert(my.boolean === false)
  }

  test("we can read a sequence contained in an object") {
    val p = new Properties()
    p.put("arr.0","1")
    p.put("arr.1","2")
    p.put("arr.2","3")
    p.put("seq.0","test")
    p.put("seq.1","foo")
    p.put("seq.2","bar")
    p.put("seq.4","baz") // skips this because 3 isn't present
    val reader = Configuration.fromProperties(p)
    val my = reader.readIn[DemoConfigurationSeq]("some")
    assert(my.seq.toList === List("test","foo","bar"))
    assert(my.arr.toList === List(1,2,3))
  }



}
