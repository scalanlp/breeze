package breeze.linalg
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class Counter2Test extends FunSuite with Checkers {
  def typeOf[X](value : X)(implicit m : scala.reflect.Manifest[X]) =
    m.toString
  def show[X](value : X)(implicit m : scala.reflect.Manifest[X]) = {
    println(typeOf(value) + ":")
    println(value)
    println()
  }

  test("Getting and setting") {
    val x = Counter2[String,Int,Double]()
    x("a",1) = 3.0
    x("b",2) = 7.75
    x("c",2) = 8.0

    assert(x.valuesIterator.toSet === Set(3.0,8.0,7.75))
  }

  /*
  test("Transpose") {
    val x = Counter2[String,Int,Double]()
    x(("a",1),("b",2),("c",2)) := List(3.0,7.75,8.0)

    assert(x.t.valuesIterator.toSet === Set(3.0,0.0, 0.0, 0.0, 8.0,7.75))
    assert(x.t.t === x)
    assert(x.t.t eq x)

    x.t(2,"a") = 1
    assert(x("a",2) === 1)
  }

  test("Slice table") {
    val x= Counter2[String,Int,Double]()
    x(("a",1),("b",2),("c",2)) := List(3.0,7.75,8.0)

    val table = x(List("a","b","c"),List(1,2))

    assert(table.domain === TableDomain(3,2))

    table(1,0) = 5
    assert(x("b",1) === 5)
  }
  */

  test("Slice rows and columns") {
//    val x = Counter2[String,Int,Double]()
//    x(("a",1),("b",2),("c",2)) := List(3.0,7.75,8.0)
    val x = Counter2[String,Int,Double](("a",1,3.0), ("b", 2, 7.75), ("c",2,8.0))

    // require expected static type
    val s1 : Counter[Int,Double] = x("a",::)
    assert(s1.toMap === Map(1->3.0))

    // write-through
    s1(1) = 4
    assert(x("a",1) === 4.0)
    x("a",1) = 3.0
    assert(s1(1) === 3.0)

    // require expected static type
    val s2 : Counter[String,Double] = x(::,2)
    assert(s2.toMap === Map("a" -> 0.0, "b" -> 7.75, "c" -> 8.0))

    // write-through
    s2("a") = 1
    assert(x("a",2) === 1.0)
    x("a",2) = 3.0
    assert(s2("a") === 3.0)
  }

  /*
  test("Addition") {
    // require expected static type
    val v1 : Counter2[String,String,Int] =
      Counter2(("a","a",1),("b","b",2)) + Counter2(("a","a",3))
    assert(v1 === Counter2(("a","a",4),("b","b",2)))

    // require expected static type
    val v2 : Counter2[String,Char,Int] =
      Counter2(("a",'a',3)) + Counter2(("a",'a',1),("b",'b',2))
    assert(v2 === Counter2(("a",'a',4),("b",'b',2)))
  }

  test("AddInto") {
    val x = Counter2[String,String,Int]()
    x += Counter2(("a","a",1))
    assert(x === Counter2(("a","a",1)))
    x += Counter2(("a","a",2),("a","b",4))
    assert(x === Counter2(("a","a",3),("a","b",4)))
  }

  test("Subtraction") {
    assert(Counter2(("a","a",1),("b","b",2)) - Counter2(("a","a",3)) === Counter2(("a","a",-2), ("b","b",2)))
    assert(Counter2(("a","a",3)) - Counter2(("a","a",1),("b","b",2)) === Counter2(("a","a",2), ("b","b",-2)))
  }

  test("Multiplication") {
    assert(Counter2(("a","a",1),("b","b",2)) :* Counter2(("a","a",3)) === Counter2(("a","a",3)))
    assert(Counter2(("a","a",3)) :* Counter2(("a","a",1),("b","b",2)) === Counter2(("a","a",3)))
  }

  test("Shaped Multiplication") {
    assert(Counter2((0,'a',1),(1,'a',2),(1,'b',3)) * Counter2(('a',0,1),('b',0,2)) ===
      Counter2((0,0,1),(1,0,8)))
  }
  */


  test("sum") {
    assert(sum(Counter2((1,'a,1.0),(1, 'b, 3.0), (2, 'a, 2.0), (2, 'b, 4.0)), Axis._0) === Counter('a -> 3., 'b -> 7.))
    assert(sum(Counter2((1,'a,1.0),(1, 'b, 3.0), (2, 'a, 2.0), (2, 'b, 4.0)), Axis._1) === Counter(1 -> 4., 2 -> 6.))
  }

  test("normalize Rows and columns") {
    assert(normalize(Counter2((1,'a,1.0),(1, 'b, 3.0), (2, 'a, 2.0), (2, 'b, 4.0)), Axis._0, 1) ===
      Counter2((1,'a,1.0/3.0),(1, 'b, 3.0/7.0), (2, 'a, 2.0/3.0), (2, 'b, 4.0/7.0)) )
    assert(normalize(Counter2((1,'a,1.0),(1, 'b, 3.0), (2, 'a, 2.0), (2, 'b, 4.0)), Axis._1, 1) ===
      Counter2((1,'a,1.0/4.0),(1, 'b, 3.0/4.0), (2, 'a, 2.0/6.0), (2, 'b, 4.0/6.0)) )
  }


}
